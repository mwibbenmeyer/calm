
library(tidyverse)
library(readr)
library(class)
library(tidycensus)
library(sf)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd('../../..') 

census_api_key("a811f383d3224c362794899a57b196696b70695c") 

counties = read.delim('raw_data/misc/ecoregions/2018_Gaz_counties_national.txt', header = TRUE, sep = "\t") %>%
  select(GEOID, INTPTLAT, INTPTLONG) %>% mutate(GEOID = str_pad(GEOID, 5, side = "left", pad = '0'))

ecoregion = read_csv("raw_data/misc/ecoregions/All_county_ecocd.csv") %>% 
  mutate(fips = str_pad(fips, 5, side = "left", pad = '0')) %>%  select(fips, ecoregion = ecocd)

merged = merge(x = ecoregion, y = counties, by.y = 'GEOID', by.x = 'fips', all.y = TRUE) %>% 
  filter(substring(fips, 1, 2) != '02' & substring(fips, 1, 2) != '15')


train = merged %>% filter(!is.na(ecoregion))
test = merged %>% filter(is.na(ecoregion))

res = knn(train[, c('INTPTLAT', 'INTPTLONG')], test[, c('INTPTLAT', 'INTPTLONG')], train$ecoregion, k = 3)

test$ecoregion = as.character.factor(res)

merged_knn = rbind(train, test) %>% select(fips, ecoregion)


write_csv(merged_knn, 'raw_data/misc/ecoregions/interpolated_ecocd_counties.csv')