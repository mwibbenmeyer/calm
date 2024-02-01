
# Useful functions for interpreting results ---------------------------------

save_county_acreages <- function(df_input, acreages_path) { 
  
  county_acreages <- df_input %>% as.data.table() %>% 
    .[ , .(acres = mean(initial_acres)), by = c("fips","ecoregion","year","lcc","initial_use")] %>% 
    .[ , .(acres = sum(acres)), by = c("fips","ecoregion","year","initial_use")]
  
  #Save data frame for use in carbon model
  dir.create(acreages_path, recursive = TRUE, showWarnings = FALSE)
  write.csv(county_acreages, sprintf("%s/acreages.csv",acreages_path))
  
  return(county_acreages)
  
}

calc_total_acres <- function(dt) {
  dt <- dt %>% as.data.table() %>% 
    .[final_use == "Crop", #Keep to avoid double-counting acres
      .(total_acres = sum(initial_acres, na.rm = TRUE)), by = "initial_use"]
  return(dt)
}

compare_scenarios <- function(s1, s2) {
  
  s1 <- calc_total_acres(s1)
  s2 <- calc_total_acres(s2)
  
  comparison <- data.frame(initial_use = s1$initial_use,
                           diff = s1 %>% .$total_acres - s2 %>% .$total_acres)
  return(comparison)
  
}

calc_elasticity <- function(s1,s2,pct_change_P = 0.1) {
  
  # This is the elasticity formula se in Scott (2013)
  pct_change_Q <- compare_scenarios(s2,s1) %>% .$diff / calc_total_acres(s1) %>% .$total_acres
  pct_change_P <- pct_change_P
  
  elasticity <- data.frame(initial_use = compare_scenarios(s1,s2) %>% .$initial_use,
                           elasticity = pct_change_Q/pct_change_P)
  return(elasticity)
  
}
