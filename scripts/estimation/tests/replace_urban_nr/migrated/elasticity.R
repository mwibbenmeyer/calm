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
setwd('../../../../') # relative paths to move directory to the root project directory

source("scripts/simulation/merge_crp_and_others/fn_modify_returns.R")
settings = unlist(read.properties("scripts/estimation/tests/replace_urban_nr/settings.txt"))

input_path <- "processing/simulation/input_data/"

se_fips <- cdlTools::fips(c("AR","AL","FL","GA","KY","LA","MO","MS","NC","SC","TN")) %>% 
  str_pad(width = 2, side = "left", pad = "0")



if (settings == 'old_forest_nr'){
  est.path <- "results\\initial_estimation\\regs_2022-07\\regs_2022-07-12_lcc_st_replace_urban_oldforestnrsmoothed.est"
  returns.df <- read_csv(sprintf("%s/returns_crprev_urbannrsub_oldforestnrsmoothed.csv",input_path), 
                         col_types = cols(fips = "c")) %>% select(c(-1))
  df <- read_csv(sprintf("%s/sim_df_crprev_urbannrsub_oldforestnrsmoothed.csv",input_path), 
                 col_types = cols(fips = "c")) %>% select(c(-1)) %>% as.data.table()
}

if (settings == 'dave_forest_nr'){
  est.path <- "results\\initial_estimation\\regs_2022-07\\regs_2022-07-12_lcc_st_replace_urban_daveforestnr.est"
  returns.df <- read_csv(sprintf("%s/returns_crprev_urbannrsub_daveforestnr.csv",input_path), 
                         col_types = cols(fips = "c")) %>% select(c(-1))
  df <- read_csv(sprintf("%s/sim_df_crprev_urbannrsub_daveforestnr.csv",input_path), 
                 col_types = cols(fips = "c")) %>% select(c(-1)) %>% as.data.table()
}

# filter for southeast
# df <- read_csv(sprintf("%s/sim_df_crprev_urbannrsub.csv",input_path), 
#                col_types = cols(fips = "c")) %>% select(c(-1)) %>% as.data.table() %>%
#   rename(initial_acres = final_acres) %>% filter(substr(fips, 1, 2) %in% se_fips)

# Function to simulate land use change given a returns vector ---------------

simulate_luc <- function(df_input, returns, estimates) {
  
  #Need to restrict to only crop because some rows in order to account for the variety of possible next-period uses
  df.a <- df_input %>% as.data.table() %>% 
            .[final_use == "Crop", .(acres = sum(initial_acres)), by = c('fips','lcc','initial_use')]

  df.r <- merge(df_input, returns, by = c('fips','year'))
  
  stata_src <- sprintf('
  
    cd L:/Project-Land_Use/
    qui do "scripts/estimation/tests/replace_urban_nr/program_calc_phats_lcc_replaceforest.do"
    
    qui calc_phat using "%s"
    
    ', estimates)
    
  df.out <- stata(stata_src, data.in = df.r, data.out = TRUE)
  
  df.p <- df.out %>% select(fips,lcc,year, initial_use, final_use, initial_acres, phat) %>% 
    pivot_wider(id_cols = c("fips","lcc",'year',"initial_use"),
                names_from = "final_use",
                names_prefix = "p",
                values_from = "phat") %>%
    mutate(across(where(is.numeric), replace_na, 0))   %>%
    merge(df.a, ., by = c('fips','lcc','initial_use'))   %>% 
    mutate(across(c(6:9), ~ .x*acres, 
                  .names = "a{substr(col,2,nchar(col))}"))
  
  df.l <- df.p %>% mutate(across(c(10:13), ~ .x, 
                                 .names = "{substr(col,2,nchar(col))}")) %>% #Rename columns prior to pivot long
    select(-initial_use, -acres) %>% # Remove old initial acres and initial use columns
    pivot_longer(cols = c('Crop','Forest','Other','Urban'),
                 names_to = "initial_use",
                 names_repair = "unique",
                 values_to = "initial_acres") %>% 
    as.data.table() %>%  
    .[ , .(initial_acres = sum(initial_acres, na.rm = TRUE)), by = c("fips","lcc","year","initial_use")] %>% 
    .[ , ":=" (Crop = 1, Forest = 1, Other = 1, Urban = 1)] %>%
    pivot_longer(cols = c('Crop','Forest','Other','Urban'),
                 names_to = "final_use",
                 values_to = "final_acres") %>% 
    select(-final_acres) %>% 
    select(fips, year, lcc, initial_use, final_use, initial_acres)
  
  return(df.l)
  
  }


# Useful functions for interpreting results ---------------------------------

run_sim <- function(df_input, returns, estimates, ints) {
  
  sim <- df_input
  for (int in seq(1,ints)) {
    sim <- quiet(simulate_luc(df_input = sim, returns = returns, estimates = estimates))
  }
  return(sim)
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
# 
# test1= run_sim(df, returns = returns.df, estimates = est.path, 10)
# test2= run_sim(df, returns = returns.df, estimates = est.path, 2)
# 
# calc_total_acres(dfl2)
# calc_total_acres(test1)
# calc_total_acres(test2)
# calc_total_acres(crop_sim)
# calc_total_acres(forest_sim)

# Run simulations and calculate elasticities ------------------------------

status_quo <- run_sim(df, returns = returns.df, estimates = est.path, 10)
crop_sim <- run_sim(df, returns = mod_returns(returns.df, crop_fn = function(x) 1.1*x), 
                   estimates = est.path, 10)
e_crop <- calc_elasticity(status_quo,crop_sim,0.1)

forest_sim <- run_sim(df, returns = mod_returns(returns.df, forest_fn = function(x) 1.1*x), 
                    estimates = est.path, 10)
e_forest <- calc_elasticity(status_quo,forest_sim,0.1)

# e <- merge(e_crop, e_forest, by = 'initial_use')
e = list(e_crop, e_forest) %>% reduce(full_join, by='initial_use')

colnames(e) <- c("initial_use", "Crop", "Forest", "Other", "Urban")

if (settings == 'old_forest_nr'){
  write.csv(e, "results/replace_urban_forest_nr/elasticity_old_forest_nr.csv")
  
}
if (settings == 'dave_forest_nr'){
  write.csv(e, "results/replace_urban_forest_nr/elasticity_dave_forest_nr.csv")
  
}




