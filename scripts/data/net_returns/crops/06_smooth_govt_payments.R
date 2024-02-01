##################################################
## Project: Land Use
## Author: Sophie Pesek
## Date: May 13, 2021
## Script purpose: Smooth government payments data to handle missing values
## Input data: new_crop_returns.csv - the combined crop data set with FIPS info
##################################################

## Load/install packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse,
               data.table,
               haven,
               dplyr,
               readxl,
               sf,
               tidycensus,
               zoo,
               stringr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # sets directory to the current directory
setwd('../../../..') # relative paths to move directory to the root project directory

options(tigris_use_cache = TRUE) #Tell tidycensus to cache shapefiles for future sessions
#census_api_key("", overwrite = TRUE, install = TRUE) # set API key

# import county shapefile using tidycensus - b19013_001 is arbitrarily chosen
counties <- get_acs(geography = "county", year = 2010, variables = "B19013_001", geometry = TRUE) %>%
  select(-c("variable","estimate","moe"))

# load data
new_crop_returns <- read_csv("processing/net_returns/crops/new_crop_returns.csv") %>% # load crop returns data
  select(., -c(X1)) %>% # remove added columns
  filter(., year <= 2017) %>% # trim to years 1997-2017
  as.data.table()
frr_data <- read_excel("processing/net_returns/crops/FRR_FIPS.xls") %>% # load farm resource region to FIPS data
  rename(., frr = 'ERS resource region') %>%  # rename column %>%
  rename(., county_fips = 'County FIPS')

##################################################
## add government payments per acre
##################################################

# load NRI acres planted data to calculate government payments per acre --------

govt_acres <- read_csv("processing/net_returns/crops/cropland_acres.csv") # load cropland acres data
govt_acres$county_fips <- paste(govt_acres$"State ANSI", govt_acres$"County ANSI", sep="")#
govt_acres1 <- select(govt_acres, county_fips, Year, Value) %>% 
  filter(., Year >= 1997) %>%
  mutate(acres = as.numeric(str_replace_all(Value, ",", ""))) %>% 
  select(-Value)
new_crop_returns1 <- new_crop_returns[, c("county_fips", "year", "govt_payments")] # trim
new_crop_returns1 <- new_crop_returns %>% 
                      group_by(county_fips, year) %>% 
                      summarize(govt_payments = mean(govt_payments, na.rm = TRUE)) %>% 
  left_join(., govt_acres1, by = c("county_fips", "year" = "Year")) %>% # merge acres and payments data
  add_column(payments_acres = .$govt_payments/.$acres) %>% # calculate govt payments per acre of planted crops
  .[, c("county_fips", "year", "payments_acres")] # trim to relevant columns

# linearly interpolate govt payments/acre for years outside census -------------

new_crop_returns <- new_crop_returns1 %>% 
  group_by(county_fips) %>%  
  dplyr::arrange(county_fips,year) %>%
  dplyr::mutate(payments_acres = na.approx(payments_acres, na.rm = FALSE)) %>%
  arrange(county_fips,year) %>% 
  merge(frr_data, by = "county_fips") %>% 
  as.data.table()



# function to calculate smoothed government payments per acre across FRR -----------------------------

smooth_gov_payments <- function(farm_resource_region, yr) { #FRR, yr, crop
  
  # subset by FRR, year. Will have one record for each county in FRR
  df_sub <- new_crop_returns[frr == farm_resource_region & year == yr]
  
  counties <- left_join(counties, frr_data, by = c("GEOID" = "county_fips")) %>%
    filter(frr == farm_resource_region) %>%
    select(-c(State, frr))
  # merge with conversion data frame
  df_sub <- merge(df_sub, counties, by.x = 'county_fips', by.y = 'GEOID', all.y = TRUE) %>%
              as.data.table()
  # replace NA values from merged missing counties
  na.indices <- which(is.na(df_sub$payments_acres))
  df_sub <- df_sub[ , ':=' (payments_acres.temp = payments_acres)]%>%
                 .[is.na(payments_acres), ':=' (payments_acres.temp = 0)]

  # create weighting matrix based on distances among counties
  dists <- measure_dists(counties) # distances among county centroids
  weights <- apply(dists, c(1,2), function(x) (1+1*x/1000)^(-2)) # weights based on Scott (2014)
  weights[na.indices , ] <- 0
  weights2 <- weights %*% diag(1/colSums(weights)) #Create columnwise weights that add to 1
  
  # calculate smoothed CCPs using weighting matrix
  df_sub$payment_acres.w <- t(weights2)%*%df_sub$payments_acres.temp
  df_sub <- df_sub[ , weighted_payment_acres := payment_acres.w]  %>%
    .[ , payment_acres := payments_acres] %>%
    .[ , c('county_fips','year','payment_acres','weighted_payment_acres')]
  
  return(df_sub)
}

# run yield smoothing function over FRR, years, and crops ----------------------

frrs <- unique(frr_data$frr)
years <- unique(new_crop_returns$year)

smoothed_gov_payments <- do.call(rbind, do.call(rbind, do.call(rbind,  # row bind to unnest results
                                                               lapply(frrs, function(r)
                                                                 lapply(years, function(y) smooth_gov_payments(farm_resource_region = r, yr = y))))))
smoothed_gov_payments.df <- as.data.frame(t(smoothed_gov_payments))
write_csv(smoothed_gov_payments.df, "processing/net_returns/crops/smoothed_gov_payments.csv")
