####################################################
# Qinrui Xiahou
# May 18, 2022
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
               readr
)

# Set working directory to land-use 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../../../../")

# Read in data
ddc_data_urbancal<- read_dta("processing/combined/ddc_data_urbancal.dta")
nr_from_dave <- read_csv("raw_data/misc/nr_from_dave/data_for_simulation.csv") %>%
  select(fips, year, ma_rev_ag, ma_cost_ag, mv_rev_ag, ma_rent_f, mv_rent_f, corr_a_f, plant_cost, wet, pop_den, pipc, P)
ccps_cfu <- read_csv("processing/ccp/ccps_cfu.csv") %>%
  select(-`...1`, -data_source, -initial_acres, total_acres) %>%
  rename(ccp = weighted_ccp)

length(unique(nr_from_dave$fips)) #838
length(unique(ddc_data_urbancal$fips)) #3109

# Replace the CCPs
ddc_data_urbancal_newccp <- ddc_data_urbancal %>%
  left_join(ccps_cfu, by=c("fips", "year", "lcc", "initial_use", "final_use"))

temp <- ddc_data_urbancal_newccp %>% select(fips, year, lcc, initial_use, final_use, weighted_ccp, ccp)

# Merge the data
ddc_data_dave <- ddc_data_urbancal_newccp %>%
  left_join(nr_from_dave, by=c("year", "fips")) %>%
  filter(is.na(P)==FALSE) %>% # drop obs that are not in Dave's sample
  filter(!initial_use %in% c("CRP", "Other"),
         !final_use %in% c("CRP", "Other")) %>%
  filter(#initial_use != final_use,
         is.na(crop_nr)==FALSE,
         is.na(forest_nr)==FALSE,
         is.na(urban_nr)==FALSE) %>% # make sure the samples are the same using different data sources
  mutate(ma_nrev_ag = ma_rev_ag - ma_cost_ag,
         dplant_cost = plant_cost * (final_use == "Forest"),
         dwet = wet * (final_use == "Crop"),
         dpop_den = pop_den * (final_use == "Urban"),
         dpipc = pipc * (final_use == "Urban")) %>%
  select(-weighted_ccp) %>%
  rename(weighted_ccp = ccp) # replace the ccp values


length(unique(ddc_data_dave$statename)) #11
temp <- ddc_data_dave %>% group_by(initial_use, final_use) %>% summarize(obs = n())

# Save the data
write_dta(ddc_data_dave, "processing/combined/ddc_data_dave.dta")


