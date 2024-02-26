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

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) ; setwd('../../../') # relative paths to move directory to the root project directory

ecoregion <- read_csv("processing/misc/ecoregions/interpolated_ecocd_counties.csv")
  
df <- haven::read_dta("processing/combined/ddc_data.dta") %>%
  filter(year == 2012) %>% 
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
  dplyr::rename(initial_use = final_use) %>%
  .[ , ":=" (Crop = 1, Forest = 1, Other = 1, Urban = 1)] %>%
  pivot_longer(cols = c('Crop','Forest','Other','Urban'),
               names_to = "final_use",
               values_to = "final_acres") %>%
  select(fips,lcc,year,initial_use,final_use,initial_acres)

# Merge residuals
df <- left_join(df,
                read.csv("results/initial_estimation/regs_2024-02/resid_2022-02-22_lcc.csv") %>% 
                  dplyr::rename(lcc = LCC) %>% 
                  mutate(fips = str_pad(as.character(fips), width = 5, side = "left", pad = "0"),
                         lcc = recode(lcc, "0" = "0",
                                      "1_2" = "1",
                                      "3_4" = "2",
                                      "5_6" = "3",
                                      "7_8" = "4"),
                         lcc = as.numeric(lcc)),
                by = c("fips","year","lcc","initial_use","final_use"))

# Merge ecoregions
df <- df %>%
  left_join(ecoregion %>% select(fips, ecoregion), by="fips")

path = "processing/calm_inputs/"
dir.create(path, recursive = TRUE, showWarnings = FALSE)
write.csv(df, sprintf("%ssim_df.csv",path))


# Returns data

returns <- read_dta("processing/combined/ddc_data.dta") %>%
    as.data.table()

returns <- returns[ , .(crop_nr = mean(crop_nr),
                        forest_nr = mean(forest_nr),
                        other_nr = mean(other_nr),
                        urban_nr = mean(urban_nr)) , by = c("fips","year")] %>%
  .[ , fips := str_pad(fips,width = 5, side = "left", pad = "0") ] %>% 
  .[year == 2012]

write.csv(returns, sprintf("%sreturns.csv",path))

