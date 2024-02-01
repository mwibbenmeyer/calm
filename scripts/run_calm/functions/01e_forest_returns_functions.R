
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

forest_prices <- read.csv(sprintf("%s/forest_return_inputs/county_species_product_harvestandprice.csv", input_path)) %>% 
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


# Adjust returns ----------------------------------------------------------

forest.returns <- merge(county_sp_prodn(forest_area), forest_prices[ , -"variable"], by = c("fips","spcd","product")) %>% 
                    .[ , .(revenue = sum(price*species.prodn, na.rm = T)), by = c("fips","product")] %>% 
                    merge(forest_area, by = "fips") %>% 
                    .[ , .(return = sum(revenue/(forest_acresk*1000))), by = "fips"]
                  # 
# 
# # CHECKS
# 
# # Check 1 - Try to reproduce total sawtimber/pulp production
# county_prodn <- county_sp_prodn(forest_area) %>% 
#   .[ , .(prodn = sum(species.prodn, na.rm = T)) , by = c("fips","product")]
# 
# test <- county_type %>% as.data.table() %>% 
#         .[ , ':=' (sawtimber = rem_saw_mbf,
#                              pulp = rem_pulp_gt)] %>% 
#   melt(value.name = "orig.prodn", variable.name = "product", measure.vars = c("sawtimber","pulp"), id.vars = c("fips","sptype")) %>% 
#   .[ , .(orig.prodn = sum(orig.prodn, na.rm = T)), by = c("fips","product")]
# 
# ggplot(data = merge(county_prodn, test, by = c("fips","product"))) + 
#   geom_point(aes(x = orig.prodn, y = prodn)) +
#   geom_function(fun = function(x) x) + 
#   facet_wrap(~product)
# 
# # Check 2 - Reproduce returns using data provided by Dave
# 
# test <- co_sp_type %>% as.data.table() %>% 
#           .[ , ":=" (saw_prodn = rem_saw_mbf,
#                      pulp_prodn = rem_pulp_gt,
#                      pulp_price = pulp_p,
#                      saw_price = saw_p)] %>% 
#           .[ , c("fips","spcd","saw_prodn","pulp_prodn","saw_price","pulp_price")] %>% 
#           melt(id.vars = c("fips","spcd"), measure = patterns("*_prodn","*_price")) %>% 
#           arrange(fips,spcd) %>% 
#           .[ , ":=" (prodn = value1, 
#                      price = value2)] %>% 
#           mutate(product = recode(as.character(variable), "1" = "sawtimber", "2" = "pulp")) %>% 
#           .[ , c("fips","spcd","product","price","prodn")]
# 
# test2 <- merge(test, shares, by = c("fips","spcd","product"), all.x = T) %>% 
#   .[ , .(val.prodn = sum(price*prodn, na.rm = T)) , by = c("fips","product","sptype")] %>% 
#   .[ , .(val.prodn = sum(val.prodn, na.rm = T)), by = c("fips")] %>% 
#   merge(forest_area, by = "fips") %>% 
#   .[ , returns := val.prodn/(forest_acresk*1000)]
# test2
# 
# ggplot(data = merge(returns, returns.df, by = "fips")) +
#   geom_point(aes(x = forest_nr, y = return)) + 
#   geom_function(fun = function(x) x)
# 
# ggplot(data = merge(counties, returns, by = "fips") %>% 
#          mutate(return = ifelse(return > 150,150,return))) + 
#   geom_sf(aes(fill = return))
