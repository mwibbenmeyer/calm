library(pbapply)
library(future.apply)
pblapply(1:10, function(x) {Sys.sleep(02); print(x)})
pblapply(1:10, function(x) print(x))


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


# vary by LCC - takes 4 minutes for two iterations
frr = 40
yr = 2002
state = "AL"
# lcc_value = "1_2"
initial = "Crop"
final = "Forest"
lccs <- c("1_2", "3_4")
test <- do.call(rbind, pblapply(lccs, function(l) smooth_ccps_state(state = state, yr = yr, lcc_value = l, initial = initial, final = final), cl=4))


# vary by year - 04m 42s
frr = 40
yrs = c(2007, 2012)
state = "AL"
# lcc_value = "1_2"
initial = "Crop"
final = "Forest"
lcc <- "1_2"
test2 <- do.call(rbind, pblapply(yrs, function(y) smooth_ccps_state(state = state, yr = y, lcc_value = lcc, initial = initial, final = final), cl=4))

# vary by state - 04m 03s
frr = 40
yr = 2002
states = c("AL","AZ")
initial = "Crop"
final = "Forest"
lcc <- "1_2"
test2 <- do.call(rbind, future_lapply(states, function(s) smooth_ccps_state(state = s, yr = yr, lcc_value = lcc, initial = initial, final = final)))

# one single iteration
frr = 40
yr = 2002
states = "AL"
initial = "Crop"
final = "Forest"
lcc <- "1_2"
test3 <- do.call(rbind, pblapply(states, function(s) smooth_ccps_state(state = s, yr = yr, lcc_value = lcc, initial = initial, final = final)))
