
# Function to calculate total production by crop, useful for adjusting crop returns 
calc_crop_prodn <- function(df) {
  
  crop_acres <- df %>% as.data.table() %>% 
    .[final_use == "Crop", .(acres = sum(initial_acres)), by = c('fips','initial_use')] %>% 
    .[initial_use == "Crop"]
  
  crop_returns <- crop_returns %>% merge(crop_acres, 
                                         # Note: crop returns is used only for weights and yield, 
                                         # so this does not need to be updated from initially imported version
                                         by.x = "county_fips", by.y = "fips",
                                         suffixes = c("",".model")) %>% 
    # Calculate county-specific crop production
    mutate(county.prodn = yield*weight*acres.model)
  
  total_production <- crop_returns %>% 
    group_by(crop) %>% 
    summarize(production = sum(county.prodn, na.rm = T))
  
  return(total_production)
  
}


# Function to update crop returns given changes in total land use -
update_crop_returns <- function(crop_returns, new, orig, elasticity = -0.661) {
  
  # Default elasticity is from Goodwin and Brester, by way of LPS 2007
  
  # Calc change in total production for each crop
  pct.change <- merge(calc_crop_prodn(new),
                      calc_crop_prodn(orig),
                      by = "crop",
                      suffixes = c(".new",".orig")) %>% 
    mutate(pct.change.q = 100*(production.new - production.orig)/production.orig,
           pct.change.p = pct.change.q/elasticity)
  
  # Based on change in production, update commodity prices
  crop_returns_2 <- crop_returns %>% 
    merge(pct.change %>% 
            select(crop,pct.change.p), by = "crop") %>% 
    mutate(price = price*((100 + pct.change.p)/100),
           returns = price*yield - cost) %>% 
    select(-pct.change.p)
  
  # Update hay returns
  hay_ret_min = crop_returns_2 %>%
    mutate(returns = ifelse((crop == 'hay' | crop == 'haylage'), NA, returns)) %>% 
    filter(!is.na(returns) & returns >= 0) %>% 
    group_by(county_fips, year) %>%
    summarize(hay_returns = min(returns, na.rm = T))
  
  crop_returns_2 <- merge(crop_returns_2, hay_ret_min, by=c('county_fips', 'year')) %>% 
    mutate(returns = ifelse((crop == 'hay' | crop == 'haylage'), hay_returns, returns)) %>% 
    select(-hay_returns)
  
  # Calculate new weighted average returns
  county_returns <- crop_returns_2 %>% as.data.table() %>% 
    # Weighted avg across crops
    .[ , weighted_returns := sum(weight*returns, na.rm = TRUE), 
       by = c("county_fips","year")] %>% 
    # Add government payments
    .[ , weighted_returns := ifelse(!is.na(weighted_returns) & !is.na(payment_acres), 
                                    (weighted_returns + payment_acres), weighted_returns), ] %>%
    # Collapse by county and year
    .[ , .(weighted_returns = mean(weighted_returns, na.rm = TRUE)),
       by = c("county_fips","year")] %>% 
    # Average from 2007-2011
    .[ , .(weighted_returns = mean(weighted_returns, na.rm = TRUE)), 
       by = c("county_fips")]
  
  #Return updated returns data set and updated crop returns data set
  updated_returns <- returns.df %>% merge(county_returns, by.x = "fips", by.y = "county_fips") %>% 
    mutate(crop_nr = weighted_returns) %>% 
    select(-weighted_returns)
  
  
  return(list(crop_returns_2, updated_returns))
  
}
