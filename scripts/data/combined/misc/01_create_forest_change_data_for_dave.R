####################################################
# Matt Wibbenmeyer
# Create afforestation/deforestation data for Dave
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
setwd("../../../../")

# Read in data
df <- read_dta("processing/combined/ddc_data_urbancal_eco.dta") %>% as.data.table()

df2 <- df[ , .(initial_acres = sum(initial_acres, na.rm = TRUE),
      final_acres = sum(final_acres, na.rm = TRUE)) , by = c("fips","year","initial_use","final_use","P","S")] %>% 
      .[ , ':=' (initial_use_2 = ifelse(initial_use == "Forest","Forest","Non-forest"),
                 final_use_2 = ifelse(final_use == "Forest","Forest","Non-forest"))] %>% 
      .[ , ':=' (change = ifelse(initial_use_2 == "Non-forest" & final_use_2 == "Forest", "Afforestation",
                                 ifelse(initial_use_2 == "Forest" & final_use_2 == "Non-forest","Deforestation", NA)))] %>% 
      .[!is.na(change), .(acres = sum(final_acres, na.rm = TRUE)), by = c("fips","year","change","P","S")]
                 

write.csv(df2, "processing/combined/misc/county_def_aff.csv")
