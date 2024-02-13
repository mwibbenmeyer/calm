
# Calc county-specific forest areas 
calc_forest_area <- function(df) {
  
  forest_area <- df %>% .[final_use == "Forest", .(forest_acresk = sum(initial_acres),
                                                          ecoregion = mean(ecoregion)), by = "fips" ]
  
  return(forest_area)
  
  }


#################################################################################
# Adjust total product quantity based on change in forest area ------------

county_sp_prodn <- function(forest_area) {
  
  # forest_area is a 3108 x 3 data frame containing fips, ecoregion, and forest_acres (1000s) by county
  sptype_production <- full_join(forest_area, cf_per_acresk.complete, by = "fips", relationship = "one-to-many") %>% 
    as.data.table() %>% 
    .[ , county_cf := forest_acresk*cf_per_acresk ] %>% # Inventory of hardwood/softwood in each county (cf)
    .[ , c("fips","ecoregion","sptype","county_cf")] %>% 
    left_join(harv_rates, by = c("ecoregion","sptype")) %>% as.data.table() %>% 
    .[ , harvest_cf := county_cf*harv_rate] %>%                                 # Harvest of hardwood/softwood in each county (cf)
    left_join(rem_product_share, by = c("ecoregion", "sptype")) %>% as.data.table() %>% 
    .[ , ":=" (county_saw_mbf = share_rem_saw*harvest_cf/83.3333,               # Sawtimber production (mbf) 
               county_pulp_gt = share_rem_pulp*harvest_cf*0.07)] %>%                 # Pulpwood production (gt)
    .[ , c("fips","ecoregion","sptype","county_saw_mbf","county_pulp_gt")] %>% 
    melt(id.vars = c("fips","ecoregion","sptype"),
         measure.vars = c("county_saw_mbf","county_pulp_gt"),
         value.name = "prodn") %>% as.data.table() %>% 
    .[ , ":=" (product = recode(variable, "county_saw_mbf" = "sawtimber",
                                "county_pulp_gt" = "pulp"))] %>%
    .[ , ":=" (prodn.units = ifelse(product == "sawtimber","mbf","gt"))] %>% 
    .[ , -c("variable")] %>% 
    .[ , prodn := value]
  
  sp_prodn <- sptype_production[ , c("fips", "sptype", "product", "prodn.units", "prodn") ] %>% 
    full_join(shares[ , c("fips","spcd", "sptype","product","share.w")], 
              by = c("fips", "product","sptype")) %>% as.data.table() %>% 
    .[ , species.prodn := prodn*share.w, ] %>% 
    merge(ecoreg %>% select(fips,section), by = "fips")
  
  
  
  return(sp_prodn)
  
}

county_sp_prodn_missings <- function(forest_area) {
  
  # This function calculates production per forest acre for each product x species type. This is necessary to fill in missing production values
  # in counties that have missing forest inventory data.
  
  # ID missing counties
  missing.counties <- unique(forest_area[which(forest_area$fips %ni% cf_per_acresk$fips), ]$fips)
  
  sp_prodn <- county_sp_prodn(forest_area) %>% as.data.table() %>% 
    full_join(forest_area, by = "fips", relationship = "many-to-one") %>% 
    as.data.table() %>% 
    .[ , prodn_acresk := species.prodn/forest_acresk] %>% 
    .[ , missing := as.numeric(fips %in% missing.counties)] %>% 
    # Recode select ecoregions because inventory data are entirely missing in some ecoregions
    .[section == 3223, section := 3222] %>% 
    .[section == 3421, section := 3427] %>% 
    .[section == 3425, section := 3427] %>% 
    .[section == 3426, section := 3427] %>% 
    # Set prodn per acre in missing counties equal within nearest adjacent ecological section within same province
    .[ , mean_prodn_acresk := mean(prodn_acresk, na.rm = TRUE), by = c("spcd","product","section")] %>% 
    .[ , prodn_acresk := ifelse(missing == 1, mean_prodn_acresk, prodn_acresk)] %>% 
    .[ , c("missing","mean_prodn_acresk") := NULL ] %>% 
    .[ , species.prodn := prodn_acresk*forest_acresk]
  
  return(sp_prodn)
  
}



total_sp_prodn <- function(forest_area) {
  
  # forest_area is a 3108 x 3 data frame containing fips, ecoregion, and forest_acres (1000s) by county
  
  total_sp_prodn <- county_sp_prodn_missings(forest_area) %>% 
    .[ , .(species.prodn = sum(species.prodn, na.rm = T)), by = c("spcd", "product","prodn.units")] 
    
  return(total_sp_prodn)

  
}

calc_forest_returns <- function(forest_area, prices) {
  
  returns <- county_sp_prodn_missings(forest_area) %>%
    merge(prices %>% select(-tamm_region), by = c("fips","spcd","product"))  %>% 
    .[ , .(revenue = sum(price*species.prodn, na.rm = T)), by = c("fips","product")] %>% 
    merge(forest_area, by = "fips") %>% 
    .[ , .(forest_nr = sum(revenue/(forest_acresk*1000))), by = "fips"]
  
  return(returns)
  
}
