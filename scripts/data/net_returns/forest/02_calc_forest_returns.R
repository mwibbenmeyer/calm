if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse,
               dplyr,
               readxl,
               sf,
               tictoc,
               tidycensus,
               haven,
               stringr,
               data.table,
               RStata,
               SimDesign, #has quiet function
               cdlTools,
               properties)


setwd(dirname(rstudioapi::getActiveDocumentContext()$path)); setwd('../../../../') # sets directory to the root directory
input_path <- "../processing/net_returns/forest/"

source("scripts/data/net_returns/forest/02a_forest_returns_functions.R")

`%ni%` <- Negate(`%in%`)

# Import data -------------------------------------------------------------

# Initial land uses by county and LCC
df.forest <- read_csv("../processing/calm_inputs/sim_df.csv",  
               col_types = cols(fips = "c")) %>% dplyr::select(c(-1)) %>% 
                mutate(ecoregion = ifelse(fips == "46113", 331, ecoregion)) %>%     
                as.data.table()

# Ecoregion data - needed for filling in counties with limited forest inventory data
ecoreg <- read_excel("../processing/calm_inputs/forest_return_inputs/county to ecoregion.xls", sheet = "county to ecoregion") %>% 
  mutate(fips = str_pad(Fips, side = "left", width = 5, pad = "0"),
         section = paste0(P,S)) %>%
  select(-Fips) %>% 
  # Adjust FIPS codes for county FIPS code changes
  mutate(fips = ifelse(fips == "12025","12086",fips),
         fips = ifelse(fips == "51560","51005",fips),
         fips = ifelse(fips == "51780","51083",fips),
         fips = ifelse(fips == "30113","30031",fips))
ecoreg <- ecoreg %>% 
  rbind(ecoreg %>% filter(fips == "01087") %>% 
          mutate(fips = "01101", County = "Montgomery")) %>%  # Add missing Montgomery county = neighboring Macon county
  rbind(ecoreg %>% filter(fips == "08013") %>% 
          mutate(fips = "08014", County = "Broomfield"))


#################################################################################
# Forest data -------------------------------------------------------------

cf_template <- CJ(fips = unique(df.forest$fips), sptype = c("hardwood", "softwood"))

cf_per_acresk <-      read.csv(sprintf("%s/sptype_cf_per_acresk.csv", input_path)) %>% 
                        as.data.table() %>% 
                        .[ , fips := str_pad(fips, width = 5, side = "left", pad = "0")] %>% 
                        .[ , X := NULL]
cf_per_acresk.complete <- cf_per_acresk %>% merge(cf_template, by = c("fips", "sptype"), all.y = T) # Make sure we have all combns of county and species type

harv_rates <-         read.csv(sprintf("%s/sptype_harv_rates.csv", input_path)) 
rem_product_share <-  read.csv(sprintf("%s/removal_product_share.csv", input_path))
hw_shares <-          read.csv(sprintf("%s/sp_product_shares_hardwood.csv", input_path)) %>% 
                        mutate(fips = str_pad(fips, width = 5, side = "left", pad = "0")) %>% 
                        as.data.table()
sw_shares <-          read.csv(sprintf("%s/sp_product_shares_softwood.csv", input_path)) %>% 
                        mutate(fips = str_pad(fips, width = 5, side = "left", pad = "0")) %>% 
                        as.data.table()
# Shares of each species (spcd) that go to sawtimber/pulpwood production
shares <- rbind(hw_shares %>% mutate(sptype = "hardwood"),
                sw_shares %>% mutate(sptype = "softwood"))


# TAMM regions, needed for adjusting forest returns following Lubowski, Plantinga, Stavins
tamm <- read_xlsx("../raw_data/misc/tamm_regions/tamm_regions.xlsx") %>% 
          rbind(data.frame(state = "MD", tamm_region = "Northeast")) # Add Maryland, which is missing
tamm$stfips <- str_pad(cdlTools::fips(tamm$state), width = 2, pad = "0", side = "left")
forest_prices <- read.csv(sprintf("%s/county_sp_product_prices.csv", input_path)) %>% 
                    mutate(fips = str_pad(fips, width = 5, side = "left", pad = "0")) %>% 
                    mutate(stfips = substr(fips,1,2)) %>% 
                    merge(tamm, by = "stfips") %>% 
                    select(fips,spcd,product,price,tamm_region) %>% 
                    as.data.table()


# Calculate species by product production per acre by county ----------------------------------------------------------

forest.returns1 <- calc_forest_returns(forest_area = calc_forest_area(df.forest), prices = forest_prices)

write.csv(forest.returns1[c(fips,forest_nr)], "processing/net_returns/forest/smoothed_forest_nr.csv")
