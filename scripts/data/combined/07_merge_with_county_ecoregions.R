####################################################
# Qinrui Xiahou
# April 26, 2022
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
               data.table
)

# Set working directory to land-use 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../../../")

# Read in data
ddc_data_urbancal<- read_dta("processing/combined/ddc_data_urbancal.dta")
ecoregion <- read_xls("raw_data/misc/ecoregions/Data/county to ecoregion.xls", sheet="county to ecoregion")

# Merge the data
ddc_data_urbancal_eco <- ddc_data_urbancal %>%
  left_join(select(ecoregion, Fips, P, S), by=c("fips"="Fips")) %>%
  mutate(P = case_when(fips=="8014" ~ 331,
                       TRUE ~ P),
         S = case_when(fips=="8014" ~ 9,
                       TRUE ~ S)) # manually fill in the newly created county fips 

# Note that FIPS 1101 - Montgomery, AL does not have ecoregion info
# Note also FIPS 8014 was formed out of three counties, among which S differs. The value is assigned with the majority rule

# Save the data
write_dta(ddc_data_urbancal_eco, "processing/combined/ddc_data_urbancal_eco.dta")
