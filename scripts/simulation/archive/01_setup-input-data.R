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

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) ; setwd('../../') # relative paths to move directory to the root project directory
getwd()

# Main data frame

df <- haven::read_dta("processing/combined/ddc_data_urbancal.dta") %>%
        filter(year == 2015) %>% 
        dplyr::select(fips,year,lcc, initial_use, final_use, final_acres) %>%
        mutate(fips = str_pad(fips, width = 5, pad = "0", side = "left"),
               lcc = recode(lcc, "0" = "0",
                            "1_2" = "1",
                            "3_4" = "2",
                            "5_6" = "3",
                            "7_8" = "4"),
                lcc = as.numeric(lcc))

path = "processing/simulation/input_data/"
dir.create(path, recursive = TRUE, showWarnings = FALSE)
write.csv(df, sprintf("%ssim_df.csv",path))

# Returns data

returns <- read.csv("processing/combined/full_combined_returns.csv") %>%
  as.data.table()

returns <- returns[ , .(CRP_nr = mean(CRP_nr),
                        crop_nr = mean(crop_nr),
                        forest_nr = mean(forest_nr),
                        other_nr = mean(other_nr),
                        urban_nr = mean(urban_nr)) , by = c("fips","year")] %>%
  .[ , fips := str_pad(fips,width = 5, side = "left", pad = "0") ] %>% 
  .[year == 2015]

write.csv(returns, sprintf("%sreturns.csv",path))
