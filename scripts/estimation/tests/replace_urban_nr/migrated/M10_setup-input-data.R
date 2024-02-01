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

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) ; setwd('../../../../') # relative paths to move directory to the root project directory
getwd()

settings = unlist(read.properties("scripts/estimation/tests/replace_urban_nr/settings.txt"))

# Main data frame
if (settings == 'old_forest_nr'){
  df <- haven::read_dta("processing/combined/ddc_data_urbancal_crprev_urbannrsub_oldforestnrsmoothed.dta") %>%
    filter(year == 2015) %>% 
    dplyr::select(fips,year,lcc, initial_use, final_use, final_acres) %>%
    mutate(fips = str_pad(fips, width = 5, pad = "0", side = "left"),
           lcc = recode(lcc, "0" = "0",
                        "1_2" = "1",
                        "3_4" = "2",
                        "5_6" = "3",
                        "7_8" = "4"),
           lcc = as.numeric(lcc)) %>%
    as.data.table()   %>%
    .[ , .(initial_acres = sum(final_acres, na.rm = TRUE)), by = c("fips","lcc","year","final_use")] %>%
    rename(initial_use = final_use) %>%
    .[ , ":=" (Crop = 1, Forest = 1, Other = 1, Urban = 1)] %>%
    pivot_longer(cols = c('Crop','Forest','Other','Urban'),
                 names_to = "final_use",
                 values_to = "final_acres") %>%
    select(fips,lcc,year,initial_use,final_use,initial_acres)
  
  path = "processing/simulation/input_data/"
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  write.csv(df, sprintf("%ssim_df_crprev_urbannrsub_oldforestnrsmoothed.csv",path))
  
}

if (settings == 'dave_forest_nr'){
  df <- haven::read_dta("processing/combined/ddc_data_urbancal_crprev_urbannrsub_daveforestnr.dta") %>%
    filter(year == 2015) %>% 
    dplyr::select(fips,year,lcc, initial_use, final_use, final_acres) %>%
    mutate(fips = str_pad(fips, width = 5, pad = "0", side = "left"),
           lcc = recode(lcc, "0" = "0",
                        "1_2" = "1",
                        "3_4" = "2",
                        "5_6" = "3",
                        "7_8" = "4"),
           lcc = as.numeric(lcc)) %>%
    as.data.table()   %>%
    .[ , .(initial_acres = sum(final_acres, na.rm = TRUE)), by = c("fips","lcc","year","final_use")] %>%
    rename(initial_use = final_use) %>%
    .[ , ":=" (Crop = 1, Forest = 1, Other = 1, Urban = 1)] %>%
    pivot_longer(cols = c('Crop','Forest','Other','Urban'),
                 names_to = "final_use",
                 values_to = "final_acres") %>%
    select(fips,lcc,year,initial_use,final_use,initial_acres)
  
  path = "processing/simulation/input_data/"
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  write.csv(df, sprintf("%ssim_df_crprev_urbannrsub_daveforestnr.csv",path))
  
}


# Returns data
if (settings == 'old_forest_nr'){
  returns <- read_dta("processing/combined/ddc_data_urbancal_crprev_urbannrsub_oldforestnrsmoothed.dta") %>%
    as.data.table()
}
if (settings == 'dave_forest_nr'){
  returns <- read_dta("processing/combined/ddc_data_urbancal_crprev_urbannrsub_daveforestnr.dta") %>%
    as.data.table()
}


returns <- returns[ , .(crop_nr = mean(crop_nr),
                        forest_nr = mean(forest_nr),
                        other_nr = mean(other_nr),
                        urban_nr = mean(urban_nr)) , by = c("fips","year")] %>%
  .[ , fips := str_pad(fips,width = 5, side = "left", pad = "0") ] %>% 
  .[year == 2015]

if (settings == 'old_forest_nr'){
  write.csv(returns, sprintf("%sreturns_crprev_urbannrsub_oldforestnrsmoothed.csv",path))
}
if (settings == 'dave_forest_nr'){
  write.csv(returns, sprintf("%sreturns_crprev_urbannrsub_daveforestnr.csv",path))
}
