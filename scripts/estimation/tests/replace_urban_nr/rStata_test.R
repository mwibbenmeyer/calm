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


setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # sets directory to the current directory
setwd('../../../../') # relative paths to move directory to the root project directory
options("RStata.StataPath" = "\"C:\\Program Files\\Stata17\\StataMP-64\"")
options("RStata.StataVersion" = 17)

# why is this returning wrong data??
# it should return a 3x2 table but it's returning the land use df???
x <- data.frame(a = rnorm(3), b = letters[1:3])

y <- stata("replace a = 2", data.in = x, data.out = TRUE)



# input_path <- "processing/simulation/input_data/"
# source("scripts/simulation/merge_crp_and_others/fn_modify_returns.R")
# 
# estimates <- "results\\initial_estimation\\regs_2022-07\\regs_2022-07-12_lcc_st_replace_urban_daveforestnr.est"
# returns.df <- read_csv(sprintf("%s/returns_crprev_urbannrsub_daveforestnr.csv",input_path), 
#                        col_types = cols(fips = "c")) %>% select(c(-1))
# df_input <- read_csv(sprintf("%s/sim_df_crprev_urbannrsub_daveforestnr.csv",input_path), 
#                col_types = cols(fips = "c")) %>% select(c(-1)) %>% as.data.table()
# 
# returns = mod_returns(returns.df, crop_fn = function(x) 1.1*x)
# 
# df.a <- df_input %>% as.data.table() %>% 
#   .[final_use == "Crop", .(acres = sum(initial_acres)), by = c('fips','lcc','initial_use')]
# 
# df.r <- merge(df_input, returns, by = c('fips','year'))
# 
# stata_src <- sprintf('
#   
#     cd L:/Project-Land_Use/
#     qui do "scripts/estimation/tests/replace_urban_nr/program_calc_phats_lcc_replaceforest.do"
#     
#     qui calc_phat using "%s"
#     
#     ', estimates)
# 
# df.out <- stata(stata_src, data.in = df.r, data.out = TRUE)




