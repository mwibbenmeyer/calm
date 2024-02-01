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
settings = unlist(read.properties("scripts/simulation/settings.txt"))

input_path <- "processing/simulation/input_data/"

se_fips <- cdlTools::fips(c("AR","AL","FL","GA","KY","LA","MO","MS","NC","SC","TN")) %>% 
  str_pad(width = 2, side = "left", pad = "0")


est.path <- "results/initial_estimation/regs_2022-11/regs_2022-11-29_lcc_st_replace_urban_daveforestnr.est"
returns.df <- read_csv(sprintf("%s/returns_crprev_urbannrsub_daveforestnr.csv",input_path), 
                       col_types = cols(fips = "c")) %>% select(c(-1))
df <- read_csv(sprintf("%s/sim_df_crprev_urbannrsub_daveforestnr.csv",input_path), 
               col_types = cols(fips = "c")) %>% dplyr::select(c(-1)) %>% as.data.table()



# Function to simulate land use change given a returns vector ---------------

simulate_luc <- function(df_input, returns, estimates, pfactor, conv.type) {
  
  #Need to restrict to only crop because some rows in order to account for the variety of possible next-period uses
  df.a <- df_input %>% as.data.table() %>% 
            .[final_use == "Crop", .(acres = sum(initial_acres)), by = c('fips','lcc','initial_use')]

  df.r <- left_join(df_input, returns, by = c('fips','year'))
  
  stata_src <- sprintf('
  
    cd L:/Project-Land_Use/
    qui do "scripts/simulation/03_program_calc_phats_lcc_replaceforest.do"
    
    qui calc_phat using "%s"
    
    ', estimates)
    
  df.out <- stata(stata_src, data.in = df.r, data.out = TRUE)
  
  df.p <- df.out %>% select(fips,lcc, ecoregion, year, 
                            initial_use, final_use, initial_acres, phat) %>% 
            mutate(c = log(phat/(1-phat)),
                   pnew = ifelse(phat > 0 & phat < 1 & final_use == conv.type & initial_use != conv.type,
                                  exp((c/pfactor)/(1+exp(c/pfactor)), 
                                 phat)) %>% 
            group_by(fips,lcc,ecoregion,year,initial_use) %>% 
            mutate(p_kj = ifelse(final_use == conv.type & initial_use != conv.type, phat, 0),
                   p_kj_prime = ifelse(final_use == conv.type & initial_use != conv.type, pnew, 0),
                   p_kj = sum(p_kj),
                   p_kj_prime = sum(p_kj_prime))
  
  
                   pnew = ifelse(final_use == conv.type & initial_use != conv.type,
                                 pnew
                                 )) %>% 
            
      
            pivot_wider(id_cols = c("fips","lcc","ecoregion",'year',"initial_use"),
                        names_from = "final_use",
                        names_prefix = "p.",
                        values_from = "pnew") %>%
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


# Useful functions for interpreting results ---------------------------------

run_sim <- function(df_input, returns, estimates, ints, carbon_path = NA) {
  
  sim <- df_input
  for (int in seq(1,ints)) {
    
    print(int)
    
    sim <- quiet(simulate_luc(df_input = sim, 
                              returns = returns, 
                              estimates = estimates))
    
    #Save data frame for use in carbon model
    if (!is.na(carbon_path)) {
      dir.create(carbon_path, recursive = TRUE, showWarnings = FALSE)
      write.csv(sim[[2]], sprintf("%s/timestep_%s.csv",carbon_path,int))
    }
    
    #Return data frame in same format as source dataframe
    sim <- sim[[1]]
    
  }

  if (!is.na(carbon_path)) {
    write.csv(sim[[2]], sprintf("%s/timestep_%s.csv",carbon_path,int))
  }
  
  return(sim)
}

save_county_acreages <- function(df_input, acreages_path) { 
  
  county_acreages <- df_input %>% as.data.table() %>% 
    .[ , .(acres = mean(initial_acres)), by = c("fips","ecoregion","year","lcc","initial_use")] %>% 
    .[ , .(acres = sum(acres)), by = c("fips","ecoregion","year","initial_use")]

  #Save data frame for use in carbon model
  dir.create(acreages_path, recursive = TRUE, showWarnings = FALSE)
  write.csv(county_acreages, sprintf("%s/acreages.csv",acreages_path))

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

output_path <- "processing/simulation/sims_221220/"

save_county_acreages(df, sprintf("%s/base/", output_path))

#Status Quo
status_quo <- run_sim(df, 
                      returns = returns.df, 
                      estimates = est.path, 
                      10,
                      carbon_path = sprintf("%sstatus_quo/carbon_model_input/", output_path))
save_county_acreages(status_quo, sprintf("%s/status_quo/", output_path))

#Increase crop returns
crop_sim <- run_sim(df, 
                      returns = mod_returns(returns.df, crop_fn = function(x) 1.1*x), 
                      estimates = est.path, 
                      10,
                      carbon_path = sprintf("%scrop_returns_1.1/carbon_model_input", output_path))
save_county_acreages(crop_sim, sprintf("%s/crop_returns_1.1/", output_path))
e_crop <- calc_elasticity(status_quo,crop_sim,0.1)

#Increase forest returns
forest_sim <- run_sim(df, returns = mod_returns(returns.df, forest_fn = function(x) 1.1*x), 
                    estimates = est.path, 10,
                    carbon_path = sprintf("%sforest_returns_1.1/carbon_model_input/", output_path))
save_county_acreages(forest_sim, sprintf("%s/forest_returns_1.1/", output_path))
e_forest <- calc_elasticity(status_quo,forest_sim,0.1)

forest_sim <- run_sim(df, returns = mod_returns(returns.df, forest_fn = function(x) 2*x), 
                      estimates = est.path, 10,
                      carbon_path = sprintf("%sforest_returns_2/carbon_model_input/", output_path))
save_county_acreages(forest_sim, sprintf("%s/forest_returns_2/", output_path))


#Write elasticities
e = list(e_crop, e_forest) %>% reduce(full_join, by='initial_use')
colnames(e) <- c("initial_use", "Crop", "Forest")


write.csv(e, "results/replace_urban_forest_nr/elasticity_dave_forest_nr_new.csv")


#Reduction in Crop Prices
crop_sim_dec <- run_sim(df, 
                    returns = mod_returns(returns.df, crop_fn = function(x) 0.9*x), 
                    estimates = est.path, 
                    10,
                    carbon_path = sprintf("%scrop_returns_0.9/carbon_model_input", output_path))
save_county_acreages(crop_sim_dec, sprintf("%s/crop_returns_0.9/", output_path))



