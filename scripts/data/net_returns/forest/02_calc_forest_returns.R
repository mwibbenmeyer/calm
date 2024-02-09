
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

# Some counties are missing - For these, calculate returns as average of nonmissing returns in same ecological section
missing.counties <- c(counties[which(counties$fips %ni% forest.returns1$fips), ]$fips,
                         unique(forest.returns1[is.na(forest_nr),]$fips))
                      
ecoreg <- read_excel("raw_data/misc/ecoregions/Data/county to ecoregion.xls",sheet = "county to ecoregion") %>% 
            rename(fips = Fips) %>% 
            mutate(fips = str_pad(fips, side = "left", width = 5, pad = "0"),
                   section = paste0(P,S)) %>%
            mutate(fips = ifelse(fips == "12025","12086",fips),
                   fips = ifelse(fips == "51560","51005",fips),
                   fips = ifelse(fips == "51780","51083",fips),
                   fips = ifelse(fips == "30113","30031",fips))
ecoreg <- ecoreg %>% 
            rbind(ecoreg %>% filter(fips == "01087") %>% 
                    mutate(fips = "01101", County = "Montgomery")) %>%  # Add missing Montgomery county = neighboring Macon county
            rbind(ecoreg %>% filter(fips == "08013") %>% 
                    mutate(fips = "08014", County = "Broomfield")) %>%  # Add missing Broomfield county = neighboring Boulder county
            mutate(missing = as.numeric(fips %in% missing.counties),
                   missing = ifelse(fips == "11001",1,missing))

forest.returns <- forest.returns1 %>% 
                    merge(ecoreg %>% select(fips,section,missing), all.y = T) %>% 
                    # Recode select ecoregions because returns are entirely missing in some ecoregions
                    .[section == 3223, section := 3222] %>% 
                    .[section == 3421, section := 3427] %>% 
                    .[section == 3425, section := 3427] %>% 
                    .[section == 3426, section := 3427] %>%    
                    # Set returns equal to mean return within nearest adjacent ecological section within same province
                    .[ , mean.returns := mean(forest_nr, na.rm = TRUE), by = section] %>% 
                    .[ , forest_nr := ifelse(missing == 1, mean.returns, forest_nr)]

write.csv(forest.returns[c(fips,forest_nr)], "processing/net_returns/forest/smoothed_forest_nr.csv")
