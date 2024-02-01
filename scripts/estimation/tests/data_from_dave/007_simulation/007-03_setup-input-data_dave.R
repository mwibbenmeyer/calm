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
               data.table)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) ; setwd('../../../../../') # relative paths to move directory to the root project directory
getwd()

# Main data frame

df <- haven::read_dta("processing/combined/ddc_data_dave.dta") %>%
        filter(year == 2012) %>% 
        dplyr::select(fips,year,lcc, initial_use, final_use, final_acres, P) %>%
        mutate(fips = str_pad(fips, width = 5, pad = "0", side = "left"),
               lcc = recode(lcc, "0" = "0",
                            "1_2" = "1",
                            "3_4" = "2",
                            "5_6" = "3",
                            "7_8" = "4"),
                lcc = as.numeric(lcc))

path = "processing/simulation/input_data/"
dir.create(path, recursive = TRUE, showWarnings = FALSE)
write.csv(df, sprintf("%ssim_df_dave.csv",path))

# Returns data

returns <- read_dta("processing/combined/ddc_data_dave.dta") %>%
  as.data.table()

returns <- returns[ , .(ma_nrev_ag = mean(ma_nrev_ag),
                        ma_rent_f = mean(ma_rent_f),
                        pop_den = mean(pop_den),
                        pipc = mean(pipc),
                        mv_rent_f = mean(mv_rent_f),
                        plant_cost = mean(plant_cost),
                        corr_a_f = mean(corr_a_f),
                        mv_rev_ag = mean(mv_rev_ag),
                        wet = mean(wet)) , by = c("fips","year")] %>%
  .[ , fips := str_pad(fips,width = 5, side = "left", pad = "0") ] %>% 
  .[year == 2012]

write.csv(returns, sprintf("%sreturns_dave.csv",path))
