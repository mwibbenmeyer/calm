##################################################
## Project: Land Use
## Author: Matt Wibbenmeyer
## Date: August 31, 2023
## Script purpose: Get urban returns proxies
##################################################

library(haven)
library(tidyverse)
library(censusapi)
library(readr)
library(readxl)
library(stringr)

substrRight = function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) ; setwd("../../../../")

years = unique(df$year)
years <- seq(1982,2062,10)


# Download population data ------------------------------------------------

#the 1990 api sucks and i have to do this weird hack to get it to work
#just run this thing once and save it, it takes so long

# pop_1997 = data.frame()
# for (st in (1:56)[-c(3, 7, 14, 43, 52)]){
# 
#   df = getCensus(name = "1990/pep/int_charagegroups",
#                        vars = c("COUNTY", "AGEGRP", "HISP", "RACE_SEX", "POP", "YEAR"),
#                        region = "county:*",
#                        regionin = str_c("state:", str_pad(st, 2, pad = "0"))) %>%
#     select(state, county, POP, YEAR) %>% filter(YEAR == 97)
# 
# 
#   df1 = df %>% mutate(FIPS = str_c(state, county))
#   df1 = aggregate(df1$POP, by=list(FIPS=df1$FIPS), FUN = sum)
#   df2 = df[!duplicated(df$county), ] %>% select(state, county, year=YEAR)
# 
#   df3 = cbind(df2, POP = df1$x)
# 
#   pop_1997 = rbind(pop_1997, df3)
# 
# }
# 
# write.csv(pop_1997,'results/replace_urban_forest_nr/pop_1997.csv')

pop_1997 = read_csv("processing/net_returns/urban/pop_1997.csv") %>% select(2:5)
pop_1997$year = sub("^", "19", pop_1997$year)

pop_2000 = getCensus(name = "2000/pep/int_population",
                     vars = c("POP", "DATE_", "DATE_DESC"), 
                     region = "county:*",
                     DATE_ = 2,
                     DATE_ = 4,
                     DATE_ = 9)

pop_2000 = pop_2000 %>% 
  mutate(year = case_when(
    DATE_ == 2 ~ 2000,
    DATE_ == 4 ~ 2002,
    DATE_ == 9 ~ 2007,
)) %>% select(state, county, year, POP)

pop_2015 = getCensus(name = "2015/pep/population",
                     vars = c("POP", "DATE_"),
                     region = "county:*",
                     DATE_ = 5,
                     DATE_ = 8)

pop_2015 = pop_2015 %>% 
  mutate(year = case_when(
    DATE_ == 5 ~ 2012,
    DATE_ == 8 ~ 2015,
  )) %>% select(state, county, year, POP)


#pop density
landarea = read_excel("raw_data/net_returns/urban/LND01.xls") %>% 
  select(fips = STCOU, land_area = LND110200D)


pop_combine = rbind(pop_1997, pop_2000) %>% 
                rbind(pop_2015)

#deal with oglala lakoda, sd having different fips across years
df$fips[df$fips == '46113'] = '46102'
pop_combine$county[pop_combine$state == '46' & pop_combine$county == '113' ] = '102' 


# Growth rates ------------------------------------------------------------

pop_pivot = pivot_wider(pop_combine, names_from = year, values_from = POP, names_prefix = 'pop_')

pop_pivot = pop_pivot %>% mutate(aagrpct_2015 = ((pop_2015/pop_2012)^(1/5)-1)*100,
                                 aagrpct_2012 = ((pop_2012/pop_2007)^(1/5)-1)*100,
                                 aagrpct_2007 = ((pop_2007/pop_2002)^(1/5)-1)*100,
                                 aagrpct_2002 = ((pop_2002/pop_1997)^(1/5)-1)*100,
                                 GeoFIPS = str_c(state, county))


# Population density ------------------------------------------------------

pop_density = merge(x = pop_pivot, y = landarea, by.x = 'GeoFIPS', by.y = 'fips', all.x = TRUE) %>%
  select(c(1, 4:9, 14))

pop_density = pop_density %>% mutate(pop_den_2015 = pop_2015 / land_area,
                                     pop_den_2012 = pop_2012 / land_area,
                                     pop_den_2007 = pop_2007 / land_area,
                                     pop_den_2002 = pop_2002 / land_area)

pop_density = pop_density %>% select(c(1, 9:12))

pop_density = pop_density %>%
  pivot_longer(!GeoFIPS, names_to = "year", values_to = "pop_density") %>% 
  mutate(year = substrRight(year, 4))

pop_prep = pop_pivot %>% select(c(9:13))

pop_prep = pop_prep %>%
  pivot_longer(!GeoFIPS, names_to = "year", values_to = "pop_growth") %>% 
  mutate(year = substrRight(year, 4))


pop_prep = merge(pop_prep, pop_density)


# Write output data -------------------------------------------------------

write.csv(pop_prep, "processing/net_returns/urban/urban_nr_proxies.csv")

