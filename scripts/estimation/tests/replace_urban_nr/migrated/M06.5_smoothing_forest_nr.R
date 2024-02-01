

# Smooth missing forest net return data based on ecoregions
# robin young

# Spatially interpolate forest returns within counties in these ecoregions: 
# Hot Continental, Warm Continental, Prairie, Subtropical, Marine, Savannah. 
# Replace forest returns still missing with zero returns in any of these ecoregions: 
# Mediterranean, T/S Desert, T/S Steppe, T Desert, T Steppe. 
# The justification for this is that returns are missing in these regions largely 
# because of absent forest product markets in this region.

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
               haven,
               properties)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 
setwd('../../../..') 

census_api_key("a811f383d3224c362794899a57b196696b70695c") 

counties <- get_acs(geography = "county", year = 2010, variables = "B19013_001", geometry = TRUE) %>%
  select(-c("variable","estimate","moe"))

ecoregion = read_excel("raw_data/misc/ecoregions/Data/county to ecoregion.xls", 
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
                               P == '341' | P == '342' ~ '340')) %>% select(Fips, ecoregion)

#manually input some missing definitions
ecoregion[nrow(ecoregion) + 1,] = list('12086', '210') #Miami-Dade
ecoregion[nrow(ecoregion) + 1,] = list('08014', '330') #Broomfield Colorado
ecoregion[nrow(ecoregion) + 1,] = list('01101', '230') #Montgomery Alabama
ecoregion$Fips[ecoregion$Fips == '46113'] = '46102' 

ecoregions_smooth = c('210', '220', '250', '230', '240', '410')

settings = unlist(read.properties("scripts/estimation/tests/replace_urban_nr/settings.txt"))


if (settings == 'old_forest_nr'){
  df_init = read_dta("processing/combined/ddc_data_urbancal_crprev_urbannrsub.dta")
  }

if (settings == 'dave_forest_nr'){
  df_init = read_dta("processing/combined/ddc_data_urbancal_crprev_urbannrsub_daveforestnr.dta")
  }

df_init$fips = str_pad(df_init$fips, 5, 'left', '0')

df = merge(x = df_init, y = ecoregion, by.x = 'fips', by.y = 'Fips', all.x = TRUE)

##################################################
## smooth missing yield data
##################################################


measure_dists <- function(shp) {
  county_centroid <- st_centroid(shp)
  dists <- st_distance(county_centroid)
  
  return(dists)
}

smooth_yields <- function(ecoreg, yr) { #FRR, yr, crop
  
  df_sub <- subset(df, ecoregion == ecoreg & year == yr)
  df_sub = df_sub %>% distinct(fips, .keep_all = TRUE)
  
  counties <- left_join(counties, ecoregion, by = c("GEOID" = "Fips")) %>%
    filter(ecoregion == ecoreg) %>%
    select(-c(ecoregion))
  # merge with conversion data frame
  df_sub <- merge(df_sub, counties, by.x = 'fips', by.y = 'GEOID', all.y = TRUE) %>%
    as.data.table()
  # replace NA values from merged missing counties
  na.indices <- which(is.na(df_sub$forest_nr)) # Get indices of counties with missing yields
  df_sub <- df_sub[ , forest_nr.temp := forest_nr] %>% 
                  .[is.na(forest_nr.temp), ':=' (forest_nr.temp = 0), ]
  
  # create weighting matrix based on distances among counties
  dists <- measure_dists(counties) # distances among county centroids
  weights <- apply(dists, c(1,2), function(x) (1+1*x/1000)^(-2)) # weights based on Scott (2014)
  weights[na.indices , ] <- 0 #Make weights for counties with NaN CCPs zero
  weights2 <- weights %*% diag(1/colSums(weights)) #Create columnwise weights that add to 1
  
  df_sub$forest_nr.w <- t(weights2)%*%df_sub$forest_nr.temp
  df_sub <- df_sub[ , weighted_yield := forest_nr.w]  %>%
    .[ , c('fips','forest_nr','year','weighted_yield')]
  
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
years <- unique(df$year)


yields <- do.call(rbind, do.call(rbind, do.call(rbind,  # row bind to unnest results
                                                lapply(ecoregs, function(r)
                                                lapply(years, function(y) smooth_yields(ecoreg = r, yr = y))))))

df_transpose = data.frame(t(yields))

smoothed_df = merge(df, df_transpose, by = c("fips","year"), all.x = TRUE) %>% 
  rename(forest_nr = forest_nr.x) 

interpolated_counties = smoothed_df %>% filter(interp_forest == TRUE)


smoothed_df = smoothed_df %>% mutate(forest_nr = forest_nr.y) %>%
  select(-forest_nr.y, -ecoregion, -weighted_yield)

smoothed_df$forest_nr = as.numeric(as.character(smoothed_df$forest_nr))

smoothed_df = smoothed_df %>% mutate(forestXlcc0 = case_when(lcc0 == 1 ~ forest_nr,
                                                             lcc0 == 0 ~ 0),
                                     forestXlcc1 = case_when(lcc1 == 1 ~ forest_nr,
                                                             lcc1 == 0 ~ 0),
                                     forestXlcc2 = case_when(lcc2 == 1 ~ forest_nr,
                                                             lcc2 == 0 ~ 0),
                                     forestXlcc3 = case_when(lcc3 == 1 ~ forest_nr,
                                                             lcc3 == 0 ~ 0),
                                     forestXlcc4 = case_when(lcc4 == 1 ~ forest_nr,
                                                             lcc4 == 0 ~ 0))



#some counties have missing definitions in the ecoregions spreadsheet
# na.test = which(is.na(smoothed_df$forest_nr))
# 
# test = smoothed_df %>% filter(fips == '12086')
# na.test = smoothed_df %>% filter(is.na(forest_nr))

# smoothed_df = smoothed_df %>% mutate(exog_counties = ifelse(ecoregion %in% ecoregions_smooth, 0, 1))

if (settings == 'old_forest_nr'){
  write_dta(smoothed_df, 'processing/combined/ddc_data_urbancal_crprev_urbannrsub_oldforestnrsmoothed.dta')
}

if (settings == 'dave_forest_nr'){
  write_dta(smoothed_df, 'processing/combined/ddc_data_urbancal_crprev_urbannrsub_daveforestnr.dta')
}







