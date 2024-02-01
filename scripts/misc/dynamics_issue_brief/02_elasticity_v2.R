if (!require("pacman")) install.packages("pacman")
devtools::install_github("robin-a-young/RStata")

pacman::p_load(tidyverse,
               dplyr,
               readxl,
               sf,
               tidycensus,
               haven,
               stringr,
               data.table,
               RStata,
               SimDesign, #has quiet function
               cdlTools,
               properties)

#May need to modify this based on location of Stata executable on local machine
options("RStata.StataPath" = "\"C:\\Program Files\\Stata17\\StataMP-64\"")
options("RStata.StataVersion" = 17)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # sets directory to the current directory
setwd('../../../') # relative paths to move directory to the root project directory

source("scripts/simulation/fn_modify_returns.R")

input_path <- "processing/misc/dynamics_issue_brief/input_data/"

se_fips <- cdlTools::fips(c("AR","AL","FL","GA","KY","LA","MO","MS","NC","SC","TN")) %>% 
  str_pad(width = 2, side = "left", pad = "0")

est.path <- "results/initial_estimation/regs_2022-11/regs_2022-11-29_lcc_st_replace_urban_daveforestnr.est"
returns.df <- read_csv(sprintf("%sreturns_crprev_urbannrsub_daveforestnr.csv",input_path), 
                       col_types = cols(fips = "c")) %>% select(c(-1))
df <- read_csv(sprintf("%s/sim_df_crprev_urbannrsub_daveforestnr.csv",input_path), 
               col_types = cols(fips = "c")) %>% dplyr::select(c(-1)) %>% as.data.table()

urban.scenario <- read.csv("processing/misc/dynamics_issue_brief/urban_scenario.csv") %>% as.data.table()
forest.scenario <- read.csv("processing/misc/dynamics_issue_brief/forest_scenario.csv") %>% as.data.table()
aggressive.forest.scenario <- read.csv("processing/misc/dynamics_issue_brief/aggressive_forest_scenario.csv") %>% as.data.table()


# Function to simulate land use change given a returns vector ---------------

simulate_luc <- function(df_input, returns, estimates, pfactor, conv.type) {
  
  #Need to restrict to only crop because some rows in order to account for the variety of possible next-period uses
  df.a <- df_input %>% as.data.table() %>% 
            .[final_use == "Crop", .(acres = sum(initial_acres)), by = c('fips','lcc','initial_use')]

  df.r <- left_join(df_input, returns, by = c('fips','year')) %>% 
            mutate(pfactor = pfactor)
  
  stata_src <- sprintf('
  
    cd L:/Project-Land_Use/
    qui do "scripts/simulation/dynamics_issue_brief/03_program_calc_phats_lcc_replaceforest.do"
    
    qui calc_phat using "%s"
    
    ', estimates)
    
  df.out <- stata(stata_src, data.in = df.r, data.out = TRUE)
  
  print(colnames(df.out))
  
  df.p <- df.out %>% select(fips,lcc, ecoregion, year, initial_use, final_use, initial_acres, phat) %>% 
    pivot_wider(id_cols = c("fips","lcc","ecoregion",'year',"initial_use"),
                names_from = "final_use",
                names_prefix = "p.",
                values_from = "phat") %>%
    mutate(across(where(is.numeric), replace_na, 0))   %>%
    merge(df.a, ., by = c('fips','lcc','initial_use')) %>% 
    mutate(across(starts_with("p."), ~ .x*acres, 
                  .names = "a.{substr(col,3,nchar(col))}"))
          
  # Data frame for feeding into carbon model
  df.c <- df.p %>%
    select(-starts_with("p."), -acres) %>% 
    #Rename columns prior to pivot long
    mutate(across(starts_with("a."), ~ .x, 
                  .names = "{substr(col,3,nchar(col))}")) %>% 
    pivot_longer(cols = c('Crop','Forest','Other','Urban'),
                 names_to = "final_use",
                 names_repair = "unique",
                 values_to = "acres") %>% 
    select(-starts_with("a.")) %>% 
    group_by(ecoregion,initial_use,final_use) %>% 
    summarize(acres = sum(acres, na.rm = TRUE))
  
  # Data frame for feeding back into simulation
  df.l <- df.p %>% mutate(across(starts_with("a."), ~ .x, 
                                 .names = "{substr(col,3,nchar(col))}")) %>% #Rename columns prior to pivot long
    select(-initial_use, -acres) %>% # Remove old initial acres and initial use columns
    pivot_longer(cols = c('Crop','Forest','Other','Urban'),
                 names_to = "initial_use",
                 names_repair = "unique",
                 values_to = "initial_acres") %>% 
    as.data.table() %>%  
    .[ , .(initial_acres = sum(initial_acres, na.rm = TRUE)), by = c("fips","ecoregion","lcc","year","initial_use")] %>% 
    .[ , ":=" (Crop = 1, Forest = 1, Other = 1, Urban = 1)] %>%
    pivot_longer(cols = c('Crop','Forest','Other','Urban'),
                 names_to = "final_use",
                 values_to = "final_acres") %>% 
    select(-final_acres) %>% 
    select(fips, ecoregion, year, lcc, initial_use, final_use, initial_acres) %>% 
    merge(df.r %>% select(fips,lcc,year,initial_use,final_use,resid),
          by = c('fips','lcc','year','initial_use','final_use'))
  
  return(list(df.l,df.c))
  
  }


# Function to iterate over pfactor to match objective -----------------------

calc_pfactor <- function(df_input, returns, estimates, conversion, objective) {
  
  init.acres <- calc_total_acres(df_input) %>% 
                  .[initial_use == conversion] %>% 
                  .$total_acres
  diff <- 1e6
  guess <- 1
  
  while (abs(diff) > 1) {
    
    sim <- quiet(simulate_luc(df_input = df_input, 
                              returns = returns, 
                              estimates = estimates,
                              pfactor = guess,
                              conv.type = conversion))
    
    total_acres <- calc_total_acres(sim[[1]]) %>% 
                    .[initial_use == conversion] %>% 
                    .$total_acres
    
    diff_new <- (init.acres + objective) - total_acres
    
    if (guess != 1) {
      slope <- -(diff_new - diff)/increment
      increment <- diff_new/slope
    } else { 
      increment <- 0.05*(-sign(diff_new))
    }
    
    diff <- diff_new
    
    print(sprintf("%s, %s", guess, diff))

    guess <- guess + increment  
    
  }
    
  return(guess)
  
}

# Function to run simulation ------------------------------------------------

run_sim <- function(df_input, returns, estimates, 
                    conversion,
                    objectives, #Needs to be a vector of changes in conversion land use type
                    carbon_path = NA) {
  
  sim <- df_input %>% 
          mutate(conv_type = conversion)
  
  pfactor.list <- numeric(0)

  for (int in seq(1,length(objectives))) {
    
    if (!is.na(objectives[int])) {
    
      # Calculate correct pfactor to match total change in scenario
      pfactor <- calc_pfactor(df_input = sim, 
                              returns = returns, 
                              estimates = estimates, 
                              conversion = conversion,
                              objective = objectives[int])
    
      } else { pfactor <- 0 }
    
    pfactor.list <- c(pfactor.list, pfactor) #Create list of pfactors calculated
    
    # Run scenario with final pfactor value
    sim <- quiet(simulate_luc(df_input = sim, 
                              returns = returns, 
                              estimates = estimates,
                              pfactor = pfactor,
                              conv.type = conversion))
    
    # Save data frame for use in carbon model
    if (!is.na(carbon_path)) {
      dir.create(carbon_path, recursive = TRUE, showWarnings = FALSE)
      write.csv(sim[[2]], sprintf("%s/timestep_%s.csv",carbon_path,int))
    }
    
    #Return data frame in same format as source dataframe
    sim <- sim[[1]]  %>% 
      mutate(conv_type = conversion)
    
    save_county_acreages(sim, carbon_path, sprintf("acreages_%s.csv", int))
    
  }

  p.df <- data.frame(timestep = seq(1,length(objectives)), pfactor = pfactor.list)
  write.csv(p.df, sprintf("%s/pfactors.csv",carbon_path))
  
  return(sim)
}


# Useful functions for interpreting results ---------------------------------

save_county_acreages <- function(df_input, path, filename) { 
  
  county_acreages <- df_input %>% as.data.table() %>% 
    .[ , .(acres = mean(initial_acres)), by = c("fips","ecoregion","year","lcc","initial_use")] %>% 
    .[ , .(acres = sum(acres)), by = c("fips","ecoregion","year","initial_use")]

  #Save data frame for use in carbon model
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  write.csv(county_acreages, sprintf("%s%s",path,filename))

  return(county_acreages)
  
  }

calc_total_acres <- function(dt) {
  dt <- dt %>% as.data.table() %>% 
          .[final_use == "Crop", #Keep to avoid double-counting acres
            .(total_acres = sum(initial_acres, na.rm = TRUE)), by = "initial_use"]
  return(dt)
  }

compare_scenarios <- function(s1, s2) {
  
  s1 <- calc_total_acres(s1)
  s2 <- calc_total_acres(s2)
  
  comparison <- data.frame(initial_use = s1$initial_use,
                           diff = s1 %>% .$total_acres - s2 %>% .$total_acres)
  return(comparison)
  
}

calc_elasticity <- function(s1,s2,pct_change_P = 0.1) {
  
  # This is the elasticity formula se in Scott (2013)
  pct_change_Q <- compare_scenarios(s2,s1) %>% .$diff / calc_total_acres(s1) %>% .$total_acres
  pct_change_P <- pct_change_P
  
  elasticity <- data.frame(initial_use = compare_scenarios(s1,s2) %>% .$initial_use,
                           elasticity = pct_change_Q/pct_change_P)
  return(elasticity)
  
}


# Run simulations and calculate elasticities ------------------------------

output_path <- "processing/misc/dynamics_issue_brief/simulations/"

save_county_acreages(df, sprintf("%s/base/", output_path), "acreages.csv")

status_quo <- run_sim(df, 
                 returns = returns.df, 
                 estimates = est.path, 
                 conversion = "Urban",
                 objectives = rep(NA,10),
                 carbon_path = sprintf("%sstatus_quo/carbon_model_input/", output_path))
save_county_acreages(status_quo, sprintf("%sstatus_quo/", output_path), "acreages.csv")


# Decline in urbanization rate scenario
urban <- run_sim(df, 
                returns = returns.df, 
                estimates = est.path, 
                conversion = "Urban",
                objectives = urban.scenario[year > 2015]$inc.urban.acres,
                carbon_path = sprintf("%surbanization_decline_2/carbon_model_input/", output_path))

save_county_acreages(urban, sprintf("%surbanization_decline_2/", output_path), "acreages.csv")

# Increase in forest scenario
forest <- run_sim(df, 
                 returns = returns.df, 
                 estimates = est.path, 
                 conversion = "Forest",
                 objectives = forest.scenario[year > 2015]$inc.forest.acres,
                 carbon_path = sprintf("%sforest_increase_2/carbon_model_input/", output_path))

save_county_acreages(forest, sprintf("%sforest_increase_2/", output_path), "acreages.csv")

# Aggressive increase in forest scenario
aggressive.forest <- run_sim(df, 
                  returns = returns.df, 
                  estimates = est.path, 
                  conversion = "Forest",
                  objectives = aggressive.forest.scenario[year > 2015]$inc.forest.acres,
                  carbon_path = sprintf("%sagressive_forest_increase/carbon_model_input/", output_path))

save_county_acreages(aggressive.forest, sprintf("%saggressive_forest_increase/", output_path), "acreages.csv")
