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
  
df <- haven::read_dta("processing/combined/ddc_data_urbancal_crprev_urbannrsub_daveforestnr.dta") %>%
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
        rename(initial_use = final_use) %>%
        .[ , ":=" (Crop = 1, Forest = 1, Other = 1, Urban = 1)] %>%
        pivot_longer(cols = c('Crop','Forest','Other','Urban'),
                     names_to = "final_use",
                     values_to = "final_acres") %>%
        select(fips,lcc,year,initial_use,final_use,initial_acres)

# Merge residuals
df <- left_join(df,
                read.csv("results/initial_estimation/regs_2022-11/resid_2022-11-29_lcc_3digits_st_replace_urban_daveforestnr.csv") %>% 
                  rename(lcc = LCC) %>% 
                  mutate(fips = str_pad(as.character(fips), width = 5, side = "left", pad = "0"),
                         lcc = recode(lcc, "0" = "0",
                                      "1_2" = "1",
                                      "3_4" = "2",
                                      "5_6" = "3",
                                      "7_8" = "4"),
                         lcc = as.numeric(lcc)) %>% 
                  filter(year < 2015) %>% 
                  group_by(fips, lcc, initial_use, final_use) %>% 
                  summarize(resid = mean(resid, na.rm = T)) %>% 
                  mutate(resid = ifelse(is.nan(resid),NA,resid)),
                by = c("fips","lcc","initial_use","final_use"))

# Merge ecoregions
df <- df %>%
  left_join(ecoregion %>% select(fips, ecoregion), by="fips")

path = "processing/misc/dynamics_issue_brief/input_data/"
dir.create(path, recursive = TRUE, showWarnings = FALSE)
write.csv(df, sprintf("%ssim_df_crprev_urbannrsub_daveforestnr.csv",path))


# Returns data

returns <- read_dta("processing/combined/ddc_data_urbancal_crprev_urbannrsub_daveforestnr.dta") %>%
    as.data.table()

returns <- returns[ , .(crop_nr = mean(crop_nr),
                        forest_nr = mean(forest_nr),
                        other_nr = mean(other_nr),
                        urban_nr = mean(urban_nr)) , by = c("fips","year")] %>%
  .[ , fips := str_pad(fips,width = 5, side = "left", pad = "0") ] %>% 
  .[year == 2012]

write.csv(returns, sprintf("%sreturns_crprev_urbannrsub_daveforestnr.csv",path))

