####################################################
# Matt Wibbenmeyer
# May 3, 2021 
# Script to measure distances between counties
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
               data.table,
               pbapply,
               future.apply
)

# Set working directory to land-use 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../../../")

`%ni%` <- Negate(`%in%`)
options(tigris_use_cache = TRUE) #Tell tidycensus to cache shapefiles for future sessions

#census_api_key("", overwrite = TRUE, install = TRUE) # set API key

#Function to measure distances between counties
measure_dists <- function(shp) {
  
  county_centroid <- st_centroid(shp)
  dists <- st_distance(county_centroid)
  
  return(dists)
  
}


# Import data ------------------------------------------------------------------

points <- read_dta("processing_output/countypanel_estimation_bal.dta") %>% as.data.table()
frr_data <- read_excel("processing/net_returns/crops/FRR_FIPS.xls") %>%
  rename(., resource_region = 'ERS resource region')

#Collapse points data set to county-year-lcc-land use conversion level data set
df <- points[ , .(total_acres = acresk), by = c("fips","year","initial_use","final_use","lcc")] %>%
  .[ , initial_acres := sum(total_acres), by = c("fips","year","initial_use","lcc")] %>%
  merge(., points[ , .(stateAbbrev = first(stateAbbrev)), by = 'fips'], by = "fips") #Merge state abbreviation back in
  
#Group some variables
df$initial_use[df$initial_use == "Pasture" | df$initial_use == "Range"] <- "Other" # group together uncommon uses
df$final_use[df$final_use == "Pasture" | df$final_use == "Range"] <- "Other"
df1 <- df[ , initial_acres := sum(total_acres), by = list(fips, year, lcc, initial_use)]
df1$transition <- paste(df1$initial_use, df1$final_use, sep="_") # concatenate initial and final uses to create transition class
df1 <- df1[, c("initial_use","final_use"):=NULL] #%>% # aggregate total acres and final use acres for other land uses
df1 <- df1[, .(total_acres = sum(total_acres)), by = list(fips, year, lcc, transition, initial_acres, stateAbbrev)]
df1$initial_use <- sapply(strsplit(as.character(df1$transition),'_'), "[", 1) # split concatenated transition
df1$final_use <- sapply(strsplit(as.character(df1$transition),'_'), "[", 2) 
df1[,transition:=NULL]
df1 <- as.data.table(df1)
df2 <- left_join(df1, frr_data, by = c("fips" = "County FIPS")) %>%
  select(-c(State, stateAbbrev)) %>%
  as.data.table()

# Extract geometry for all the U.S. counties using tidycensus - b19013_001 is arbitrarily chosen

states <- state.abb[state.abb %ni% c("AK","HI")]

us_counties <- get_acs(state = states, geography = "county", year = 2010, variables = "B19013_001", geometry = TRUE) %>%
  select(-c("variable","estimate","moe")) %>% 
  separate(NAME,c("county","state_name"),sep=", ")

us_counties$state <- state.abb[match(us_counties$state_name, state.name)]

# Function to calculate smoothed conditional choice probabilities within states ---------

smooth_ccps_state <- function(state,yr,lcc_value,initial,final) {

  # frr = 40
  # yr = 2002
  # state = "AL"
  # lcc_value = "1_2"
  # initial = "Crop"
  # final = "Forest"
  
  #Subset to a single initial-final use pair and by county-lcc-year. Will have one record for each county in state
  df_sub <- df1[stateAbbrev == state & year == yr & lcc == lcc_value & initial_use == initial & final_use == final]
  
  #Calculate initial acres in each fips (even those with no transition to final use)
  df_initial <- df1[stateAbbrev == state & year == yr & lcc == lcc_value & initial_use == initial, 
                    .(initial_acres = mean(initial_acres)),
                    fips]
  
  #Import county shapefile
  counties <- us_counties %>% filter(state==state)
  #Merge with conversion data frame, add NA records for missing counties
  df_sub <- merge(df_sub, counties, by.x = 'fips', by.y = 'GEOID', all.y = TRUE)
  #Merge with conversion data frame, add NA records for counties with no initial acres
  df_sub <- merge(df_sub[,"initial_acres" := NULL], df_initial, by = 'fips', all.x = TRUE)
  #Replace NA values from merged missing counties
  df_sub <- df_sub[is.na(total_acres), ':=' (total_acres = 0,
                                               year = yr,
                                               stateAbbrev = state,
                                               lcc = lcc_value,
                                               initial_use = initial,
                                               final_use = final), ]
  df_sub <- df_sub[is.na(initial_acres), ':=' (initial_acres = 0)]
  df_sub <- df_sub[ , p := total_acres/initial_acres]
  nan.indices <- which(is.nan(df_sub$p))
  df_sub <- df_sub[is.nan(df_sub$p), p := 0]
  
  #Create weighting matrix based on distances among counties
  dists <- measure_dists(counties) #Distances among county centroids
  weights <- apply(dists, c(1,2), function(x) (1+1*x/1000)^(-2)) #Weights based on Scott (2014)
  weights[nan.indices , ] <- 0 #Make weights for counties with NaN CCPs zero
  weights2 <- weights %*% diag(1/colSums(weights)) #Create columnwise weights that add to 1
  df_sub$weighted_ccp <- t(weights2)%*%df_sub$p #Calculate weighted CCPs
  
  df_sub <- df_sub[ , c('fips','year','lcc','initial_use','final_use','weighted_ccp')]
  
  return(df_sub)
}

# Function to calculate smoothed conditional choice probabilities across FRR ---------

smooth_ccps_frr <- function(frr,yr,lcc_value,initial,final) {

  # frr = 40
  # yr = 2002
  # lcc_value = "1_2"
  # initial = "Crop"
  # final = "Forest"

  #Subset to a single initial-final use pair and by county-lcc-year. Will have one record for each county in state
  df_sub <- df2[resource_region == frr & year == yr & lcc == lcc_value & initial_use == initial & final_use == final]
  
  #Calculate initial acres in each fips (even those with no transition to final use)
  df_initial <- df2[resource_region == frr & year == yr & lcc == lcc_value & initial_use == initial, 
                    .(initial_acres = mean(initial_acres)),
                    fips]
  
  #Import county shapefile
  counties1 <- us_counties %>% filter(state==state)
  counties1 <- left_join(counties1, frr_data, by = c("GEOID" = "County FIPS")) %>%
    filter(resource_region == frr) %>%
    select(-c(State, resource_region))
  
  #Merge with conversion data frame, add NA records for missing counties
  df_sub <- left_join(counties1, df_sub, by = c("GEOID" = "fips")) %>%
    rename(., fips = GEOID) %>%
    as.data.table()
  #Merge with conversion data frame, add NA records for counties with no initial acres
  df_sub <- merge(df_sub[,"initial_acres" := NULL], df_initial, by = 'fips', all.x = TRUE)
  #Replace NA values from merged missing counties
  df_sub <- df_sub[is.na(total_acres), ':=' (total_acres = 0,
                                             year = yr,
                                             resource_region = frr,
                                             lcc = lcc_value,
                                             initial_use = initial,
                                             final_use = final), ]
  df_sub <- df_sub[is.na(initial_acres), ':=' (initial_acres = 0)]
  df_sub <- df_sub[ , p := total_acres/initial_acres]
  nan.indices <- which(is.nan(df_sub$p))
  df_sub <- df_sub[is.nan(df_sub$p), p := 0]
  
  #Create weighting matrix based on distances among counties
  dists <- measure_dists(counties) #Distances among county centroids
  weights <- apply(dists, c(1,2), function(x) (1+1*x/1000)^(-2)) #Weights based on Scott (2014)
  weights[nan.indices , ] <- 0 #Make weights for counties with NaN CCPs zero
  weights2 <- weights %*% diag(1/colSums(weights)) #Create columnwise weights that add to 1
  df_sub$weighted_ccp <- t(weights2)%*%df_sub$p #Calculate weighted CCPs
  
  df_sub <- df_sub[ , c('fips','year','lcc','initial_use','final_use','weighted_ccp')]
  
  return(df_sub)
}

# Run function over states, years, and transitions -----------------------------

states <- state.name[state.abb %ni% c("AK","HI")]
frrs <- unique(frr_data$resource_region)
years <- unique(df1$year)[unique(df1$year) >= 2002]
#lcc_values <- unique(df1$lcc)[unique(df1$lcc)!="0"] #Remove 0 which denotes federal use ---- actually filters out urban
lcc_values <- unique(df1$lcc)
initial_uses <- c("Crop","Forest","Urban","CRP","Other")
final_uses <- c("Crop","Forest","Urban","CRP","Other")

#This will take a while to run so test on a single state-year combination

state_list <- list()
for (state in states) {
  state_list[[state]] <- do.call(rbind, do.call(rbind, do.call(rbind, do.call(rbind,  #Row bind to unnest results
                                                                              pblapply(years, function(y)
                                                                                future_lapply(lcc_values, function(l)
                                                                                  future_lapply(initial_uses, function(i)
                                                                                    future_lapply(final_uses, function(f) smooth_ccps_state(state = state, yr = y, lcc_value = l, initial = i, final = f)))))))))
  write.csv(state_list[[state]], paste("processing/ccp/state_results/", state, ".csv", sep=""), row.names = F, na="")
  saveRDS(state_list[[state]], paste("processing/ccp/state_results/", state, ".rds", sep=""))
}

frr_list <- list()
for (frr in frrs) {
  frr_list[[frr]] <- do.call(rbind, do.call(rbind, do.call(rbind, do.call(rbind,  #Row bind to unnest results
                                                                          pblapply(years, function(y)
                                                                            future_lapply(lcc_values, function(l)
                                                                              future_lapply(initial_uses, function(i)
                                                                                future_lapply(final_uses, function(f) smooth_ccps_frr(frr = frr, yr = y, lcc_value = l, initial = i, final = f)))))))))
  write.csv(frr_list[[frr]], paste("processing/ccp/frr_results/", frr, ".csv", sep=""), row.names = F, na="")
  saveRDS(frr_list[[frr]], paste("processing/ccp/state_results/", frr, ".rds", sep=""))
} 

# Add original and smoothed CCPs together and label the data source ------------

result_state1 <- result_state %>%
  filter(., !is.na(weighted_ccp))
result_frr1 <- result_frr %>%
  filter(., !is.na(weighted_ccp))
result_state2 <- result_state1 %>%
  filter(., weighted_ccp != 1 & weighted_ccp != 0)
result_frr2 <- result_frr %>%
  filter(., weighted_ccp != 1 & weighted_ccp != 0)
result_frr3 <- anti_join(result_frr2, result_state2, by=c("fips", "year", "lcc", "initial_use", "final_use")) %>%
  add_column(data_source = "FRR")
result_state3 <- anti_join(result_state1, result_frr3, by=c("fips", "year", "lcc", "initial_use", "final_use")) %>%
  add_column(data_source = "State")
result1 <- rbind(result_state3, result_frr3) # bind results together
result_frr4 <- anti_join(result_frr1, result1, by=c("fips", "year", "lcc", "initial_use", "final_use")) %>%
  add_column(data_source = "State")
result <- rbind(result1, result_frr4) # bind results together

write.csv(result_state, "processing/ccp/ccps_state.csv") # write csv
write.csv(result_frr, "processing/ccp/ccps_frr.csv") # write csv
write.csv(result3, "processing/ccp/ccps.csv") # write csv
