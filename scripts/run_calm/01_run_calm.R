if (!require("pacman")) install.packages("pacman")
devtools::install_github("robin-a-young/RStata")

pacman::p_load(tidyverse,
               dplyr,
               readxl,
               sf,
               tidycensus,
               haven,
               reshape,
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
setwd('../../') # relative paths to move directory to the root project directory

wd <- "L:/Project-Land_Use/wibbenmeyer/" # Absolute file path of working directory. Must be absolute filepath for Stata compatibility
input_path <- "../processing/calm_inputs/"
output_path <- "../processing/simulation/"

suppressMessages({
  source("scripts/run_calm/functions/01a_run_sim.R") # Functions that run simulation
  source("scripts/run_calm/functions/01c_fn_modify_returns.R") # Function that can be used to modify land use returns under alternative scenarios
  source("scripts/run_calm/functions/01d_crop_returns_functions.R") # Functions used to recalculate crop returns following each period
  source("scripts/run_calm/functions/01e_forest_returns_functions.R") # Functions used to recalculate forest returns following each period
  source("scripts/run_calm/functions/01g_carbon-model.R") # Functions used to run carbon model---translate land use change to carbon flux
})

sim_output <- run_sim(returns = returns.df, # Returns data frame. This can be modified using function in 01c_fn_modify_returns to explore alternative returns scenarios.
                      ints = 10, # Number of 5-year time intervals over which to run simulation
                      scenario_name = "status_quo" #Scenario name, used for naming results folder, and tagging results
                      ) 

cf <- run_carb(sim_output[[2]], scenario_name = "status_quo")

