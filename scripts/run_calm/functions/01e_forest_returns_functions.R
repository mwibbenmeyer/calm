
# Function to calculate total production by forest, useful for adjusting forest returns 
calc_forest_prodn <- function(df) {
  
  forest_acres <- df %>% as.data.table() %>% 
    .[final_use == "Forest", .(acres = sum(initial_acres)), by = c('fips','initial_use')] %>% 
    .[initial_use == "Forest"]
  
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


# Import data -------------------------------------------------------------

# Initial land uses by county and LCC
df.forest <- read_csv(sprintf("%s/sim_df.csv",input_path), 
               col_types = cols(fips = "c")) %>% dplyr::select(c(-1)) %>% 
                mutate(ecoregion = ifelse(fips == "46113", 331, ecoregion)) %>%     
                as.data.table()

forest_area <- df.forest %>% .[final_use == "Forest", .(forest_acresk = sum(initial_acres),
                                                 ecoregion = mean(ecoregion)), by = "fips" ]

ecoreg.list <- unique(df.forest %>% as.data.frame %>% .[c("fips","ecoregion")])

# import county shapefile using tidycensus - b19013_001 is arbitrarily chosen
counties <- get_acs(geography = "county", year = 2010, variables = "B19013_001", geometry = TRUE) %>%
                select(-c("variable","estimate","moe")) %>% 
                rename(fips = GEOID) %>% 
                merge(ecoreg.list, by = "fips")

# Forest data -------------------------------------------------------------

cf_per_acresk <-      read.csv(sprintf("%s/forest_return_inputs/sptype_cf_per_acresk.csv", input_path)) %>% 
                        as.data.table() %>% 
                        .[ , fips := str_pad(fips, width = 5, side = "left", pad = "0")]
harv_rates <-         read.csv(sprintf("%s/forest_return_inputs/sptype_harv_rates.csv", input_path)) 
rem_product_share <-  read.csv(sprintf("%s/forest_return_inputs/removal_product_share.csv", input_path))
hw_shares <-          read.csv(sprintf("%s/forest_return_inputs/sp_product_shares_hardwood.csv", input_path)) %>% 
                        mutate(fips = str_pad(fips, width = 5, side = "left", pad = "0")) %>% 
                        as.data.table()
sw_shares <-          read.csv(sprintf("%s/forest_return_inputs/sp_product_shares_softwood.csv", input_path)) %>% 
                        mutate(fips = str_pad(fips, width = 5, side = "left", pad = "0")) %>% 
                        as.data.table()
shares <- rbind(hw_shares %>% mutate(sptype = "hardwood"),
                sw_shares %>% mutate(sptype = "softwood"))

forest_prices <- read.csv(sprintf("%s/forest_return_inputs/ALL_county_species_product_harvestandprice.csv", input_path)) %>% 
                  as.data.table() %>% 
                  .[ , fips := str_pad(fips, width = 5, side = "left", pad = "0")] %>%
                  melt(id.vars = c("fips","spcd"),
                       measure.vars = c("pulp_p","saw_p"),
                       value.name = c("price")) %>% 
                  .[ , product := recode(variable, "pulp_p" = "pulp",
                                         "saw_p" = "sawtimber")] %>% 
                  # Convert pulp prices from GT to cf and sawtimber prices from mbf to cf.
                  .[product == "pulp", price_cf := price*0.07] %>%
                  .[product == "sawtimber", price_cf := price/83.3333]


# Adjust total product quantity based on change in forest area ------------

county_sp_prodn <- function(forest_area) {
  
  # forest_area is a 3108 x 3 data frame containing fips, ecoregion, and forest_acres (1000s) by county
  
  sptype_production <- full_join(forest_area, cf_per_acresk, by = "fips", relationship = "one-to-many") %>% 
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
         value.name = "prodn") %>% 
    .[ , ":=" (product = recode(variable, "county_saw_mbf" = "sawtimber",
                                "county_pulp_gt" = "pulp"))] %>%
    .[ , ":=" (prodn.units = ifelse(product == "sawtimber","mbf","gt"))] %>% 
    .[ , -c("variable")]
  
  sp_prodn <- sptype_production[ , c("fips", "sptype", "product", "prodn.units", "prodn") ] %>% 
    full_join(shares[ , c("fips","spcd", "sptype","product","share.w")], 
              by = c("fips", "product","sptype")) %>% as.data.table() %>% 
    .[ , species.prodn := prodn*share.w, ] 
  
  return(sp_prodn)
  
}

total_sp_prodn <- function(forest_area) {
  
  # forest_area is a 3108 x 3 data frame containing fips, ecoregion, and forest_acres (1000s) by county
  
  total_sp_prodn <- county_sp_prodn(forest_area) %>% 
    .[ , .(species.prodn = sum(species.prodn, na.rm = T)), by = c("spcd", "product","prodn.units")] %>% 
    
    return(total_sp_prodn)
  
}


county_sp_prodn(forest_area = forest_area) 
total_sp_prodn(forest_area = forest_area) %>% group_by(product,prodn.units) %>% summarize(sum = sum(species.prodn))


# Calculate returns ----------------------------------------------------------

forest.returns1 <- merge(county_sp_prodn(forest_area), forest_prices[ , -"variable"], by = c("fips","spcd","product")) %>% 
                    .[ , .(revenue = sum(price*species.prodn, na.rm = T)), by = c("fips","product")] %>% 
                    merge(forest_area, by = "fips") %>% 
                    .[ , .(forest_nr = sum(revenue/(forest_acresk*1000))), by = "fips"]

write.csv(forest.returns[c(fips,forest_nr)], "processing/net_returns/forest/smoothed_forest_nr.csv")
