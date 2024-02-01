##################################################
## Project: Land Use
## Author: Sophie Pesek
## Date: May 13, 2021
## Script purpose: Construct returns for each crop/state/year as a function of price/cost/yield/government_payments
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
               zoo)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # sets directory to the current directory
setwd('../../../..') # relative paths to move directory to the root project directory

options(tigris_use_cache = TRUE) #Tell tidycensus to cache shapefiles for future sessions
#census_api_key("", overwrite = TRUE, install = TRUE) # set API key

# import county shapefile using tidycensus - b19013_001 is arbitrarily chosen
counties <- get_acs(geography = "county", year = 2010, variables = "B19013_001", geometry = TRUE) %>%
  select(-c("variable","estimate","moe"))

# load data
new_crop_returns <- read_csv("processing/net_returns/crops/new_crop_returns.csv") %>% # load crop returns data
  select(., -c(1)) %>% # remove added columns
  filter(., year <= 2017) %>% # trim to years 1997-2017
  as.data.table()
frr_data <- read_excel("processing/net_returns/crops/FRR_FIPS.xls") %>% # load farm resource region to FIPS data
  rename(., frr = 'ERS resource region') %>%  # rename column
  rename(., county_fips = 'County FIPS')

##################################################
## smooth missing yield data
##################################################

# calculate distance dependent weighting of crop yields ------------------------

# function to measure distances between counties

measure_dists <- function(shp) {
  county_centroid <- st_centroid(shp)
  dists <- st_distance(county_centroid)
  
  return(dists)
}

# function to calculate smoothed yields across FRR -----------------------------

smooth_yields <- function(farm_resource_region, yr, crp) { #FRR, yr, crop
  
  # subset by FRR, year, and crop. Will have one record for each county in FRR
  df_sub <- new_crop_returns[frr == farm_resource_region & year == yr & crop == crp]
  
  counties <- left_join(counties, frr_data, by = c("GEOID" = "county_fips")) %>%
    filter(frr == farm_resource_region) %>%
    select(-c(State, frr))
  # merge with conversion data frame
  df_sub <- merge(df_sub, counties, by.x = 'county_fips', by.y = 'GEOID', all.y = TRUE) %>%
    as.data.table()
  # replace NA values from merged missing counties
  na.indices <- which(is.na(df_sub$yield)) # Get indices of counties with missing yields
  df_sub <- df_sub[ , yield.temp := yield] %>% 
                  .[is.na(yield.temp), ':=' (yield.temp = 0), ]
  
  # create weighting matrix based on distances among counties
  dists <- measure_dists(counties) # distances among county centroids
  weights <- apply(dists, c(1,2), function(x) (1+1*x/1000)^(-2)) # weights based on Scott (2014)
  weights[na.indices , ] <- 0 #Make weights for counties with NaN CCPs zero
  weights2 <- weights %*% diag(1/colSums(weights)) #Create columnwise weights that add to 1
  
  # calculate smoothed CCPs using weighting matrix
  df_sub$yield.w <- t(weights2)%*%df_sub$yield.temp #Calculate weighted CCPs
  df_sub <- df_sub[ , weighted_yield := yield.w]  %>%
    .[ , c('county_fips','state_fips','frr','crop','year','yield','weighted_yield')]
  
  return(df_sub)
}

# run yield smoothing function over FRR, years, and crops ----------------------

frrs <- unique(frr_data$frr)
years <- unique(new_crop_returns$year)
crops <- c("corn", "sorghum", "soybeans", "winter wheat", "durum wheat", "spring wheat", "barley", "oats", "rice", "upland cotton", "pima cotton") # list of crops

yields <- do.call(rbind, do.call(rbind, do.call(rbind,  # row bind to unnest results
                                                lapply(frrs, function(r)
                                                  lapply(years, function(y)
                                                    lapply(crops, function(c) smooth_yields(farm_resource_region = r, yr = y, crp = c)))))))

write_csv(yields, "processing/net_returns/crops/smoothed_yields.csv")
