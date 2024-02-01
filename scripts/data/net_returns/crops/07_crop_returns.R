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

# load data
new_crop_returns <- read_csv("processing/net_returns/crops/crop_returns.csv") %>% # load crop returns data
  select(., -c(...1)) %>% # remove added columns
  filter(., year <= 2017) %>% # trim to years 1997-2017
  as.data.table()
frr_data <- read_excel("processing/net_returns/crops/FRR_FIPS.xls") %>% # load farm resource region to FIPS data
  rename(., frr = 'ERS resource region') # rename column
dollar_conversion <- read_excel("processing/net_returns/crops/USD_2010_conversion.xlsx")


# convert nominal cost to $2010
new_crop_returns <- left_join(new_crop_returns, dollar_conversion, by = c("year"))
new_crop_returns <- new_crop_returns %>%
  mutate(price_2010 = price*USD_2010) %>%
  mutate(cost_2010 = cost*USD_2010) %>%
  select(-c(cost, price, USD_2010)) %>%
  rename(., cost = cost_2010) %>%
  rename(., price = price_2010)


# calculate ers crops cost trend as ratios to apply to specialty crop costs
ers_cost_trend = new_crop_returns %>% group_by(year) %>% summarize(sum_acres = sum(acres, na.rm = TRUE),
                                                                   mean_cost = sum(cost*acres/sum_acres, na.rm = TRUE)) 
ers_cost_trend = ers_cost_trend %>% mutate(ratio = mean_cost / filter(ers_cost_trend, year == 2010)$mean_cost)


specialty_crop_returns = read_csv("processing/net_returns/crops/extended_crops/specialty_nr_acres_merged.csv")

specialty_crop_returns = merge(specialty_crop_returns, ers_cost_trend, by='year')

specialty_crop_returns = specialty_crop_returns %>% mutate(ers_cost_adjusted_cost = inflation_adjusted_cost * ratio,
                                                           nr = revenue - ers_cost_adjusted_cost)

# distribute prices for oats and barley in maine to rest of new england
new_england = c('09', '23', '25', '33', '44', '50')
NE = new_crop_returns %>% filter(state_fips %in% new_england) %>%
  dplyr::group_by(crop, year) %>%
  fill(price, .direction = "downup") %>%
  dplyr::ungroup()

new_crop_returns = merge(new_crop_returns, NE %>% select(county_fips, year, crop, new_price = price), by=c('county_fips', 'year', 'crop'), all.x = 1)
new_crop_returns$price = ifelse(is.na(new_crop_returns$price), new_crop_returns$new_price, new_crop_returns$price)
new_crop_returns = new_crop_returns %>% select(-new_price)


# Import yield data ------------------------------------------------------------

yields <- read_csv("processing/net_returns/crops/smoothed_yields.csv") %>% 
            as.data.table()

# add smoothed yield data to original data frame  ------------------------------

yields <- yields[is.na(yield) & !is.na(weighted_yield), ":=" (yield = weighted_yield,
                                     smoothed.yield = "yes")] %>%
                .[is.na(smoothed.yield) & !is.na(yield), smoothed.yield := "no"]
new_crop_returns <- merge(new_crop_returns, yields[ , c('county_fips','year','yield','crop','smoothed.yield')],
                          by=c('county_fips','year','crop'), all.x = TRUE) %>%
  rename(., yield = yield.y) %>% # add new yields to original dataframe
  select(-c(yield.x))

# Import smoothed gov payments---------------------------------------------------

sgp <- read_csv("processing/net_returns/crops/smoothed_gov_payments.csv") %>%
        as.data.table()

sgp <- sgp[is.na(payment_acres) & !is.na(weighted_payment_acres), ":=" (payment_acres = weighted_payment_acres,
                                                        smoothed.gp = "yes")] %>%
          .[is.na(smoothed.gp) & !is.na(payment_acres), smoothed.gp := "no"]

# Average gov payment of Santa Cruz, AZ FIPS 04023 with other counties in AZ

measure_dists <- function(shp) {
  county_centroid <- st_centroid(shp)
  dists <- st_distance(county_centroid)
  
  return(dists)
}

census_api_key("a811f383d3224c362794899a57b196696b70695c") 
counties <- get_acs(geography = "county", year = 2010, variables = "B19013_001", geometry = TRUE) %>%
  select(-c("variable","estimate","moe"))
counties = counties %>% filter(substr(GEOID, 1, 2) == '04')

#Use the smooth yield function to smooth government payments in Arizona
smooth_yields <- function(st, yr) { #FRR, yr, crop
  
  df_sub <- subset(sgp, substr(county_fips, 1, 2) == st & year == yr)
  df_sub = df_sub %>% distinct(county_fips, .keep_all = TRUE)
  
  counties <- left_join(counties, df_sub, by = c("GEOID" = "county_fips")) %>%
    filter(substr(GEOID, 1, 2) == st)
  # replace NA values from merged missing counties
  df_sub = df_sub %>% mutate(payment_acres = ifelse(county_fips=='04023',NA,payment_acres),
                     weighted_payment_acres = ifelse(county_fips=='04023',NA,weighted_payment_acres))
  na.indices <- which(is.na(df_sub$payment_acres)) 
  df_sub <- df_sub[ , payment_acres.temp := payment_acres] %>% 
    .[ , weighted_payment_acres.temp := weighted_payment_acres] %>% 
    .[is.na(payment_acres.temp), ':=' (payment_acres.temp = 0), ] %>%
    .[is.na(weighted_payment_acres.temp), ':=' (weighted_payment_acres.temp = 0), ]
  dists <- measure_dists(counties) 
  weights <- apply(dists, c(1,2), function(x) (1+1*x/1000)^(-2)) 
  weights[na.indices , ] <- 0 
  weights2 <- weights %*% diag(1/colSums(weights)) 
  
  df_sub$payment_acres.w <- t(weights2)%*%df_sub$payment_acres.temp
  df_sub$weighted_payment_acres.w <- t(weights2)%*%df_sub$weighted_payment_acres.temp
  df_sub <- df_sub[ , payment_acres := payment_acres.w] %>% .[ , weighted_payment_acres := weighted_payment_acres.w] %>%
    .[ , c('county_fips','year','payment_acres','weighted_payment_acres')]
  
  return(df_sub)
}

years <- unique(sgp$year)

smoothed <- do.call(rbind, do.call(rbind, do.call(rbind,  # row bind to unnest results
                                                lapply('04', function(r)
                                                  lapply(years, function(y) smooth_yields(st = r, yr = y))))))
df_transpose = data.frame(t(smoothed))
df_transpose$year = as.numeric(as.character(df_transpose$year))

merge_back = df_transpose %>% select(county_fips, year, payment_acres_new = payment_acres, weighted_payment_acres_new = weighted_payment_acres)
sgp = merge(sgp, merge_back, by=c('county_fips', 'year'), all.x=TRUE)
sgp = sgp %>% mutate(payment_acres = ifelse(county_fips=='04023',payment_acres_new,payment_acres),
                     weighted_payment_acres = ifelse(county_fips=='04023',weighted_payment_acres_new,weighted_payment_acres))
sgp = sgp %>% select(-weighted_payment_acres_new, -payment_acres_new)   
sgp = transform(sgp, weighted_payment_acres = as.numeric(weighted_payment_acres), 
                payment_acres = as.numeric(payment_acres))


new_crop_returns <- merge(new_crop_returns, sgp[ , c('county_fips','year','payment_acres','smoothed.gp')],
                          by=c('county_fips','year'), all.x = TRUE) %>%
  rename(., payment_acres = payment_acres)# add new payment acres to original dataframe
  # select(-c(payment_acres.x))



##################################################
## weighted average of acres planted for each crop in a farm resource region/state for a given year
##################################################

# calculate unweighted crop returns per acre  ---------------------------------------------

new_crop_returns$returns = new_crop_returns$price*new_crop_returns$yield - new_crop_returns$cost
# create a data frame with ALL acres planted data by state, including data previously omitted because of no specified counties (i.e. "other counties" data) -----

# create data frame with state codes, crops and years
state_fips <- na.omit(data.frame(state_code = unique(new_crop_returns[,c("state_fips")])))
state_acres <- data.frame(state_fips = rep(state_fips$state_fips, each = 231)) # 11 crops x 21 years of data = 176 rows of each county
crop = c("corn", "sorghum", "soybeans", "winter wheat", "durum wheat", "spring wheat", "barley", "oats", "rice", "upland cotton", "pima cotton") # list of crops
state_acres$crop <- rep(crop, each = 21, times = 49) # repeat crops 21 times for all 49 state codes
year = c(1997:2017) # list of years
state_acres$year <- rep(year, times = 539) # 11 crops x 49 state fips = 539. repeat sequence of years for each crop in each FIPS code

# load and join acres data
cropf = c("corn", "sorghum", "soybeans", "winter_wheat", "durum_wheat", "spring_wheat", "barley", "oats", "rice", "upland_cotton", "pima_cotton") # list of formatted crops
rm(i)
for(i in cropf) {
  acres <- paste(i, "acres", sep = "_") # create an acres planted variable for each crop
  acres <- read_csv(sprintf("processing/net_returns/crops/acres/%s.csv", toString(acres))) # load acres data
  acres <- mutate_all(acres, .funs=tolower) # change all character entries to lowercase
  acres$Year <- as.numeric(acres$Year) # convert year to numeric
  acres$Value <- as.numeric(acres$Value) # convert value to numeric
  names(acres)[names(acres) == "Value"] <- "acres" # rename column
  
  crop_acres <- aggregate(acres$acres, by=list(state_fips=acres$`State ANSI`, year=acres$`Year`, crop=acres$`Commodity`), FUN=sum, na.rm=TRUE) # aggregate and sum acres data by state, year, crop
  names(crop_acres)[names(crop_acres) == "x"] <- "state_acres" # rename column
  crop_acres[crop_acres == "8"] <- "08" # fix unusual FIPS codes from spring wheat
  
  state_acres <- left_join(x = state_acres, y = crop_acres, by = c("state_fips", "year", "crop")) # merge crop data with acres
  if("state_acres.y" %in% colnames(state_acres)) {
    state_acres$state_acres <- rowSums(cbind(state_acres$state_acres.x,state_acres$state_acres.y), na.rm=TRUE) # if not the first iteration, bind the two acres columns together
    state_acres <- state_acres[, c("state_fips", "crop", "year", "state_acres")] # trim columns
  }
}


# merge in specialty crops to ERS crops

specialty_crop_returns = specialty_crop_returns %>% select(county_fips = fips, state_fips, year, crop = commodity_desc, price, cost, yield, acres, returns = nr)
specialty_crop_returns = specialty_crop_returns %>% mutate(crop = tolower(crop), state_fips = str_pad(as.character(state_fips), 2, 'left', '0'))
specialty_crop_returns = specialty_crop_returns %>% filter(case_when(crop %in% c('hay', 'haylage') ~ TRUE,
                                                                     TRUE ~ cost != 0))

new_crop_returns = bind_rows(new_crop_returns, specialty_crop_returns) %>% drop_na(county_fips)

# recode oglala lakota county SD back to shannon county SD
new_crop_returns = new_crop_returns %>% mutate(county_fips = ifelse(county_fips=='46102','46113',county_fips))
                     
# fill in frr
new_crop_returns = new_crop_returns %>%
  group_by(county_fips) %>%
  fill(frr) %>%
  fill(frr, .direction = "up")

# use lowest crop return of county-year group as hay returns with max(return, 0) if negative


new_crop_returns = new_crop_returns %>% mutate(returns = ifelse((crop == 'hay' | crop == 'haylage'),NA,returns))

hay_ret_min = new_crop_returns %>%
  filter(!is.na(returns) & returns >= 0) %>% 
  group_by(county_fips, year) %>%
  summarize(hay_returns = min(returns))
  
new_crop_returns = merge(new_crop_returns, hay_ret_min, by=c('county_fips', 'year'))
new_crop_returns = new_crop_returns %>% mutate(returns = ifelse((crop == 'hay' | crop == 'haylage'), hay_returns, returns))
new_crop_returns = new_crop_returns %>% select(-hay_returns)

# add specialty crop acres to state_acres

spec_crop_acres = aggregate(specialty_crop_returns$acres, by=list(state_fips=specialty_crop_returns$state_fips, 
                                                                  year=specialty_crop_returns$year, 
                                                                  crop=specialty_crop_returns$crop), FUN=sum, na.rm=TRUE) 
names(spec_crop_acres)[names(spec_crop_acres) == "x"] <- "state_acres" # rename column
state_acres = bind_rows(state_acres, spec_crop_acres) 

# calculate crop weights from states ---------------------------------

state_acres <- state_acres %>% as.data.table() %>%
                .[ , total_acres := sum(state_acres, na.rm = TRUE), by = c("state_fips","year")] %>%
                .[ , weight := state_acres/total_acres] %>% 
                .[ , c("crop","state_fips","weight","year")]

new_crop_returns <- left_join(new_crop_returns,
                          state_acres, by = c("crop","state_fips","year"))

# # Calculate FRR weighted average for new england to address missing acres
# 
# frr30 = new_crop_returns %>% filter(frr == 30)
# frr30_acres = frr30 %>% dplyr::group_by(crop, year) %>%
#   summarise(acres = sum(acres,na.rm=TRUE))
# 
# frr30_acres_weight = frr30_acres %>% filter(crop %in% c('barley', 'oats')) %>% as.data.table() %>%
#   .[ , total_acres := sum(acres, na.rm = TRUE), by = c("year")] %>%
#   .[ , weight := acres/total_acres]
# 
# NE_weights = new_crop_returns %>% filter(state_fips %in% new_england)
# NE_weights = merge(NE_weights %>% select(-weight), frr30_acres_weight %>% select(year, crop, weight), by = c('crop', 'year'), all.x=1)
# 
# 
# new_crop_returns = merge(new_crop_returns, NE_weights %>% select(county_fips, year, crop, new_weight = weight), by=c('county_fips', 'year', 'crop'), all.x = 1)
# new_crop_returns$weight = ifelse(is.na(new_crop_returns$weight), new_crop_returns$new_weight, new_crop_returns$weight)
# new_crop_returns = new_crop_returns %>% select(-new_weight)

# calculate weighted returns
new_crop_returns = data.table(new_crop_returns)
new_weight = new_crop_returns[,.(weighted_returns = sum(weight*returns, na.rm = TRUE)), by = c("county_fips","year")]

# join original returns data with weights --------------------------------------
new_gov_returns <- left_join(new_weight, sgp[ , c('county_fips','year','payment_acres','smoothed.gp')],
                             by = c("county_fips","year"))
new_gov_returns <- new_gov_returns[, c("county_fips", "year", "weighted_returns", "payment_acres")] %>%
                    .[ , crop_nr := ifelse(!is.na(weighted_returns) & !is.na(payment_acres), 
                                (weighted_returns + payment_acres), weighted_returns)]

colnames(new_gov_returns) <- c("county_fips","year","crop_returns","gov_returns","crop_nr")

new_gov_returns$county_fips[new_gov_returns$county_fips == '46113'] = '46102' 

# Write weighted average crop returns
write.csv(new_gov_returns, "processing/net_returns/crops/crop_returns_by_county.csv") # write csv of crop returns without government payments

# Write crop-level returns
new_crop_returns$county_fips[new_crop_returns$county_fips == '46113'] = '46102' 

write.csv(new_crop_returns, "processing/net_returns/crops/crop_level_returns.csv") # write csv of crop returns without government payments

# Lines for getting crop_nr from crop-level data set.
# new_weight = new_crop_returns[,.(weighted_returns = sum(weight*returns, na.rm = TRUE),
#                                  payment_acres = mean(payment_acres, na.rm = TRUE)), by = c("county_fips","year")] %>% 
#   .[ , crop_nr := weighted_returns + payment_acres]



