library(haven)
library(tidyverse)
library(censusapi)
library(readr)
library(readxl)
library(stringr)
library(future.apply)


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../../../../")


# Import data -------------------------------------------------------------

# Read in raw forest returns, estimated by Dave Wear
forest_nr = read_csv("raw_data/net_returns/forest/county_forest_rent_s2.csv") %>% 
  mutate(fips = str_pad(county, 5, side = 'left', pad = '0')) %>%
  select(fips, forest_nr = s_rent)
forest_nr$fips[forest_nr$fips == '46113'] = '46102'
forest_nr$fips[forest_nr$fips == '12025'] = '12086'
forest_nr[nrow(forest_nr) + 1,] = list('01101', 281) 

# Counties 
counties <- get_acs(geography = "county", year = 2010, variables = "B19013_001", geometry = TRUE) %>%
  select(-c("variable","estimate","moe"))


# Ecoregions
ecoregion <- read_excel("raw_data/misc/ecoregions/Data/county to ecoregion.xls", 
                       sheet = "county to ecoregion", col_types = "text") %>% 
  mutate(Fips = str_pad(Fips, 5, side = "left", pad = '0')) %>% 
  mutate(ecoregion = case_when(P == '212' ~ '210', 
                               P == '221' | P == '222' ~ '220',
                               P == '231' | P == '232' | P == '234' ~ '230',
                               P == '251' | P == '255' ~ '250',
                               P == '242' ~ '240',
                               P == '411' ~ '410',
                               P == '261' | P == '262' | P == '263' ~ '260',
                               P == '311' | P == '313' | P == '315' ~ '310',
                               P == '321' | P == '322' ~ '320',
                               P == '331' | P == '332' | P == '333'| P == '334' ~ '330',
                               P == '341' | P == '342' ~ '340')) %>% select(fips = Fips, ecoregion) 

#manually input some missing definitions
ecoregion[nrow(ecoregion) + 1,] = list('12086', '410') #Miami-Dade
ecoregion[nrow(ecoregion) + 1,] = list('08014', '330') #Broomfield Colorado
ecoregion[nrow(ecoregion) + 1,] = list('01101', '230') #Montgomery Alabama
ecoregion[nrow(ecoregion) + 1,] = list('46102', '330') 

ecoregions_smooth = c('220', '250', '230', '240', '210', '330')

# Merge returns and ecoregions --------------------------------------------

df <- merge(x = forest_nr, y = ecoregion, by = 'fips', all.x = TRUE)
df <- df %>% mutate(forest_nr = ifelse(ecoregion %in% ecoregions_smooth & forest_nr == 0, NA, forest_nr))


##################################################
## smooth missing yield data
##################################################

measure_dists <- function(shp) {
  county_centroid <- st_centroid(shp)
  dists <- st_distance(county_centroid)
  
  return(dists)
}

smooth_yields <- function(ecoreg) { #FRR, yr, crop
  
  df_sub <- subset(df, ecoregion == ecoreg)
  df_sub = df_sub %>% distinct(fips, .keep_all = TRUE)
  
  eco.counties <- left_join(counties, ecoregion, by = c("GEOID" = "fips")) %>%
    filter(ecoregion == ecoreg) %>%
    select(-c(ecoregion))
  # merge with conversion data frame
  df_sub <- merge(df_sub, eco.counties, by.x = 'fips', by.y = 'GEOID', all.y = TRUE) %>%
    as.data.table()
  # replace NA values from merged missing counties
  na.indices <- which(is.na(df_sub$forest_nr)) # Get indices of counties with missing yields
  df_sub <- df_sub[ , forest_nr.temp := forest_nr] %>% 
    .[is.na(forest_nr.temp), ':=' (forest_nr.temp = 0), ]
  
  # create weighting matrix based on distances among counties
  dists <- measure_dists(eco.counties) # distances among county centroids
  weights <- apply(dists, c(1,2), function(x) (1+1*x/1000)^(-2)) # weights based on Scott (2014)
  weights[na.indices , ] <- 0 #Make weights for counties with NaN CCPs zero
  weights2 <- weights %*% diag(1/colSums(weights)) #Create columnwise weights that add to 1
  
  df_sub$forest_nr.w <- t(weights2)%*%df_sub$forest_nr.temp
  df_sub <- df_sub[ , weighted_yield := forest_nr.w]  %>%
    .[ , c('fips','forest_nr','weighted_yield')] %>% 
    .[ , ecoregion := ecoreg]
  
  if (ecoreg %in% ecoregions_smooth){
    df_sub = df_sub %>% mutate(forest_nr = coalesce(forest_nr, weighted_yield),
                               interp_forest = case_when(weighted_yield == forest_nr ~ TRUE,
                                                         weighted_yield != forest_nr ~ FALSE))
  } else {
    df_sub = df_sub %>% mutate(interp_forest = ifelse(is.na(forest_nr), TRUE, FALSE),
                               forest_nr = ifelse(is.na(forest_nr), 0, forest_nr))
  }
  
  return(df_sub)
}


ecoregs <- unique(df$ecoregion)

plan(multisession, workers = 6)
yields <- do.call(rbind,  # row bind to unnest results
            future_lapply(ecoregs, function(r) smooth_yields(ecoreg = r)))

write.csv(yields, 'processing/net_returns/forest/smoothed_forest_nr.csv')







