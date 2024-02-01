####################################################
# Matt Wibbenmeyer
# May 3, 2021 
# Script to measure distances between counties
####################################################

## Load/install packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse,
               dplyr,
               readxl,
               sf,
               tidycensus,
               haven,
               stringr,
               data.table,
               RStata,
               cdlTools)


#May need to modify this based on location of Stata executable on local machine
options("RStata.StataPath" = "\"C:\\Program Files\\Stata17\\StataMP-64\"")
options("RStata.StataVersion" = 17)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # sets directory to the current directory
setwd('../../../') # relative paths to move directory to the root project directory

source("scripts/simulation/merge_crp_and_others/fn_modify_returns.R")


# Function to simulate land use change given a returns vector ---------------

simulate_luc <- function(returns, estimates) {

  df.r <- merge(df, returns, by = c('fips','year'))
  
  # Run program to calculate predicted probabilities ------------------------
  
  stata_src <- sprintf('
  
  cd L:/Project-Land_Use/
  qui do "scripts/simulation/merge_crp_and_others/program_calc_phats_lcc_newmodel1.do"
  
  qui calc_phat using "%s"
  
  ', estimates)
                      
  df.out <- stata(stata_src, data.in = df.r, data.out = TRUE)
  
  # Predict land use conversions forward -------------------------------------
  
  df.p <- df.out %>% select(fips,lcc,initial_use,final_use,phat) %>%
    mutate(fips = str_pad(fips, width = 5, side = "left", pad = "0")) %>%
    pivot_wider(id_cols = c("fips","lcc","initial_use"),
                names_from = "final_use",
                names_prefix = "p",
                values_from = "phat") %>%
    mutate(across(where(is.numeric), replace_na, 0)) %>%
    merge(df.a %>% mutate(initial_use = final_use), ., by = c('fips','lcc','initial_use')) %>% 
    select(-final_use) %>% 
    mutate(across(pCrop:pUrban, ~ .x*acres, 
                  .names = "a{substr(col,2,nchar(col))}"))
                                          
  df.l <- df.p %>% mutate(across(aCrop:aUrban, ~ .x, 
                                 .names = "{substr(col,2,nchar(col))}")) %>% #Rename columns prior to pivot long
                    mutate(initial_acres = acres) %>% 
                    pivot_longer(cols = c('Urban','Crop','Forest','Other'),
                                names_to = "final_use",
                                names_repair = "unique",
                                values_to = "final_acres") %>% 
            as.data.table()
  
  initial_acres <- df.l %>%
    distinct(fips, lcc, initial_use, initial_acres, .keep_all = TRUE) %>%
    group_by(initial_use) %>%
    summarize(initial_acres = sum(initial_acres, na.rm=TRUE)) %>%
    rename(landuse = initial_use)
  
  final_acres <- df.l %>%
    distinct(fips, lcc, initial_use, final_use, final_acres, .keep_all = TRUE) %>%
    group_by(final_use) %>%
    summarize(final_acres = sum(final_acres, na.rm=TRUE)) %>%
    rename(landuse = final_use)
  
  new_acres <- initial_acres %>%
    left_join(final_acres, by=c("landuse"))
  
  # new_acres <- df.l[ , .(final_acres = sum(final_acres, na.rm = TRUE)) , by = 'final_use']
  
  return(new_acres)

  }

# Import data -------------------------------------------------------------

input_path <- "processing/simulation/input_data/"

se_fips <- cdlTools::fips(c("AR","AL","FL","GA","KY","LA","MO","MS","NC","SC","TN")) %>% 
            str_pad(width = 2, side = "left", pad = "0")

# Import county x LCC x transition data frame
df <- read.csv(sprintf("%s/sim_df_crprev.csv",input_path)) %>% as.data.table() 
        # %>%
        # mutate(fips = str_pad(fips, width = 5, side = "left", pad = "0"))
        # %>% 
        # filter(substr(fips,1,2) %in% se_fips) %>% 
        # mutate(fips = as.numeric())

# Create data set containing total acres in each use in each county x LCC
df.a <- df[ , .(acres = sum(final_acres)), by = c('fips','lcc','final_use')] %>%
  mutate(fips = str_pad(fips, width = 5, side = "left", pad = "0"))

# Create county x land use returns matrix
rtns <- read.csv(sprintf("%s/returns_crprev.csv",input_path)) %>% select(-X)

# Path of estimates file
est <- "results\\initial_estimation\\regs_2022-07\\regs_2022-07-12_lcc_st_newmodel1.est"


# Run simulation------------------------------------------------------------

# initial_acres <- df[ , .(total_acres = sum(final_acres, na.rm = TRUE)) , by = 'final_use']

baseline <- simulate_luc(returns = rtns, estimates = est)
sim <- simulate_luc(returns = mod_returns(rtns, other_fn = function(x) 1.2*x), estimates = est)

mc <- merge(baseline, sim, by = c("landuse", "initial_acres"), suffixes = c("_base","_sim"))
  
mc$base_change <- mc$final_acres_base - mc$initial_acres
mc$sim_change <- mc$final_acres_sim - mc$initial_acres



simulate_luc(returns = rtns, estimates = est)

simulate_luc(returns = mod_returns(rtns, crop_fn = function(x) 1.2*x), estimates = est)
simulate_luc(returns = mod_returns(rtns, forest_fn = function(x) 1.2*x), estimates = est)
simulate_luc(returns = mod_returns(rtns, urban_fn = function(x) 1.2*x), estimates = est)
simulate_luc(returns = mod_returns(rtns, other_fn = function(x) 1.2*x), estimates = est)
