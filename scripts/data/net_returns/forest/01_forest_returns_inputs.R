if (!require("pacman")) install.packages("pacman")
devtools::install_github("robin-a-young/RStata")

pacman::p_load(tidyverse,
               dplyr,
               readxl,
               sf,
               tictoc,
               tidycensus,
               haven,
               stringr,
               data.table,
               RStata,
               SimDesign, #has quiet function
               cdlTools,
               properties)


setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # sets directory to the current directory
setwd('../../../../../') # relative paths to move directory to the root project directory

# function to measure distances between counties
measure_dists <- function(shp) {
  county_centroid <- st_centroid(shp)
  dists <- st_distance(county_centroid)
  
  return(dists)
}


# Import data -------------------------------------------------------------

# Initial land uses by county and LCC
df <- read_csv("processing/calm_inputs/sim_df.csv", 
               col_types = cols(fips = "c")) %>% dplyr::select(c(-1)) %>% as.data.table() %>% 
        .[fips == "46113", ecoregion := 331]

ecoregions <- unique(df %>% as.data.frame %>% .[c("fips","ecoregion")]) 

# import county shapefile using tidycensus - b19013_001 is arbitrarily chosen
counties <- get_acs(geography = "county", year = 2010, variables = "B19013_001", geometry = TRUE) %>%
  select(-c("variable","estimate","moe")) %>% 
  dplyr::rename(fips = GEOID) %>% 
  merge(ecoregions, by = "fips")

##################################################################################
##################################################################################
# Forest data --------------------------------------------------------------

county_type <- read.csv("processing/net_returns/forest/east_nonfed_county_type_product_harvestandprice.csv") %>% 
  rbind(read.csv("processing/net_returns/forest/PNW_nonfed_county_type_product_harvestandprice.csv")) %>% 
  rbind(read.csv("processing/net_returns/forest/RM_nonfed_county_type_product_harvestandprice.csv") %>% 
          mutate(fips = str_pad(fips, side = "left", width = 5, pad = "0")) %>%
          filter(substr(fips,1,2) != "48")) %>% # Filter out Texas observations which are erroneously included in RM file
  mutate(fips = str_pad(fips, side = "left", width = 5, pad = "0")) %>%
  mutate(fips = recode(fips, "46102" = "46113")) %>% 
  merge(ecoregions, by = "fips")

co_sp_type <- read.csv("processing/net_returns/forest/east_nonfed_county_species_product_harvestandprice.csv") %>% 
  rbind(read.csv("processing/net_returns/forest/PNW_nonfed_county_species_product_harvestandprice.csv")) %>% 
  rbind(read.csv("processing/net_returns/forest/RM_nonfed_county_species_product_harvestandprice.csv") %>% 
          mutate(fips = str_pad(fips, side = "left", width = 5, pad = "0")) %>%
          filter(substr(fips,1,2) != "48")) %>% # Filter out Texas observations which are erroneously included in RM file
  mutate(fips = str_pad(fips, side = "left", width = 5, pad = "0")) %>% 
  mutate(fips = recode(fips, "46102" = "46113")) %>% 
  merge(ecoregions, by = "fips")
write.csv(co_sp_type, "processing/net_returns/forest/ALL_county_species_product_harvestandprice.csv")

ecoregion_tot <- read.csv("processing/net_returns/forest/ecoregion_totals.csv") 

##################################################################################
# Calculate shares of removals within each species type that are sawtimber and pulpwood
share_rem <- county_type %>% 
  group_by(ecoregion, sptype) %>% 
  summarize(across(starts_with("rem_"), ~ sum(., na.rm = T))) %>% 
  # Calculate removal shares
  mutate(share_rem_saw = rem_saw_cf/(rem_saw_cf + rem_pulp_cf),
         share_rem_pulp = rem_pulp_cf/(rem_saw_cf + rem_pulp_cf)) %>% 
  # Replace NAN values with 100% shares for sawtimber
  mutate(share_rem_saw = ifelse(substr(as.character(ecoregion),1,1) %in% c("3","4") & is.nan(share_rem_saw), 1, share_rem_saw),
         share_rem_pulp = 1 - share_rem_saw) %>% 
  dplyr::select("ecoregion", "sptype", starts_with("share"))
write.csv(share_rem, "processing/net_returns/forest/removal_product_share.csv")

##################################################################################
# Calculate harvest rates by species type (annual)
harv_rates <- county_type %>% 
  group_by(ecoregion, sptype) %>%
  # Calculate total inventory as sum of inv_gs_cf
  summarize(across(c("inv_gs_cf","total_rem_cf"), ~ sum(., na.rm = T))) %>% 
  mutate(harv_rate = total_rem_cf/inv_gs_cf) 
#Set harvest rate in regions with a zero harvest rate equal to minimum rate for other regions
minval <- min(harv_rates %>% filter(harv_rate > 0 & !is.nan(harv_rate)) %>% .$harv_rate)
harv_rates <- harv_rates %>%
  mutate(harv_rate = ifelse(harv_rate == 0 | is.nan(harv_rate) | is.infinite(harv_rate), minval, harv_rate)) %>% 
  dplyr::select("ecoregion","sptype","harv_rate")
write.csv(harv_rates, "processing/net_returns/forest/sptype_harv_rates.csv")

##################################################################################
# Calculate cubic feet per acre for each species type
co_forest <- df[final_use == "Forest", .(forest_acres = sum(initial_acres, na.rm = T)), by = "fips"]
cf_per_acresk <- merge(county_type, co_forest, by = "fips") %>% 
  mutate(cf_per_acresk = inv_gs_cf/forest_acres) %>% 
  dplyr::select("fips","sptype","cf_per_acresk")
write.csv(cf_per_acresk, "processing/net_returns/forest/sptype_cf_per_acresk.csv")

##################################################################################
# Calculate species-product shares within hardwood/softwood harvests
co_sp_hardwood <- rbind(co_sp_type %>% 
                          filter(spcd >= 300) %>% 
                          group_by(fips) %>% 
                          mutate(tot_county_saw = sum(rem_saw_mbf, na.rm = T),
                                 share = rem_saw_mbf/tot_county_saw,
                                 product = "sawtimber") %>% 
                          dplyr::select(fips,ecoregion,spcd,product,share), 
                        co_sp_type %>% 
                          filter(spcd >= 300) %>% 
                          group_by(fips) %>% 
                          mutate(tot_county_pulp = sum(rem_pulp_gt, na.rm = T),
                                 share = rem_pulp_gt/tot_county_pulp,
                                 product = "pulp") %>% 
                          dplyr::select(fips,ecoregion,spcd,product,share)) 

co_sp_softwood <- rbind(co_sp_type %>% 
                          filter(spcd < 300) %>% 
                          group_by(fips) %>% 
                          mutate(tot_county_saw = sum(rem_saw_mbf, na.rm = T),
                                 share = rem_saw_mbf/tot_county_saw,
                                 product = "sawtimber") %>% 
                          dplyr::select(fips,ecoregion,spcd,product,share), 
                        co_sp_type %>% 
                          filter(spcd < 300) %>% 
                          group_by(fips) %>% 
                          mutate(tot_county_pulp = sum(rem_pulp_gt, na.rm = T),
                                 share = rem_pulp_gt/tot_county_pulp,
                                 product = "pulp") %>% 
                          dplyr::select(fips,ecoregion,spcd,product,share)) 

# Function to interpolate missing shares based on within region proximity
interpolate_shares <- function(data, prod, reg, species) { #FRR, yr, crop
  
  print(sprintf("%s, %s, %s", prod, reg, species))
  
  # subset by ecoregion and species
  df_sub <- data %>% as.data.table() %>% 
    .[product == prod & ecoregion == reg & spcd == species, c("fips","ecoregion","spcd","share")] %>% 
    distinct() %>%
    merge(counties %>% filter(ecoregion == reg), all.y = T) %>% 
    mutate(spcd = ifelse(is.na(spcd), species, spcd)) %>% 
    arrange(fips)
  
  # replace NA values from merged missing counties
  na.indices <- which(is.na(df_sub$share)) # Get indices of counties with missing yields
  df_sub$share[na.indices] <- 0
  
  # create weighting matrix based on distances among counties
  dists <- measure_dists(counties %>% filter(ecoregion == reg) %>% arrange(fips)) # distances among county centroids
  weights <- apply(dists, c(1,2), function(x) (1+1*x/1000)^(-2)) # weights based on Scott (2014)
  weights[na.indices, ] <- 0 #Make weights for counties with NaN shares zero
  weights2 <- weights %*% diag(1/colSums(weights)) #Create columnwise weights that add to 1
  
  # interpolate missing values using weighting matrix
  df_sub$share.w <- t(weights2)%*%df_sub$share #Calculate weighted CCPs
  df_sub <- df_sub[ , share.w := ifelse(!is.na(share), share, share.w)] #Keep true shares if present
  df_sub <- df_sub %>% dplyr::select(-geometry) %>% .[ , product := prod]
  
  df_sub <- df_sub %>% as.data.table() %>% .[ , c("fips","ecoregion","spcd","product", "NAME", "share", "share.w"), with = FALSE]
  
  return(df_sub)
}

tic()
hw <- do.call(rbind, lapply(c("sawtimber","pulp"), function(p)
  do.call(rbind, lapply(unique(co_sp_hardwood$spcd), function(s) 
    do.call(rbind, lapply(unique(co_sp_hardwood$ecoregion), function(r) 
      interpolate_shares(co_sp_hardwood, reg = r, species = s, prod = p)))))))
toc()
write.csv(hw, "processing/net_returns/forest/sp_product_shares_hardwood.csv")

tic()
sw <- do.call(rbind, lapply(c("sawtimber","pulp"), function(p)
  do.call(rbind, lapply(unique(co_sp_softwood$spcd), function(s) 
    do.call(rbind, lapply(unique(co_sp_softwood$ecoregion), function(r) 
      interpolate_shares(co_sp_softwood, reg = r, species = s, prod = p)))))))
toc()
write.csv(sw, "processing/net_returns/forest/sp_product_shares_softwood.csv")

##################################################################################
# Calculate forest prices data frame

fp_template <- rbind(data.frame(CJ(fips = unique(df$fips), spcd = unique(shares$spcd)), product = "pulp"),
                  data.frame(CJ(fips = unique(df$fips), spcd = unique(shares$spcd)), product = "sawtimber"))
forest_prices <- read.csv("processing/net_returns/forest/ALL_county_species_product_harvestandprice.csv") %>% 
  as.data.table() %>% 
  .[ , fips := str_pad(fips, width = 5, side = "left", pad = "0")] %>%
  melt(id.vars = c("fips","spcd","ecoregion"),
       measure.vars = c("pulp_p","saw_p"),
       value.name = c("price")) %>% 
  as.data.table() %>% 
  .[ , price := value] %>% 
  .[ , product := recode(variable, "pulp_p" = "pulp",
                         "saw_p" = "sawtimber")] %>% 
  # Convert pulp prices from GT to cf and sawtimber prices from mbf to cf.
  .[product == "pulp", price_cf := price*0.07] %>%
  .[product == "sawtimber", price_cf := price/83.3333] %>% 
  .[ , c("fips","spcd","product","ecoregion","price","price_cf"), with = FALSE]
# Fill in missing prices in counties that are completely missing in inventory data
price.missing <- unique(df[which(df$fips %ni% forest_prices$fips),]$fips) 
forest_prices <- forest_prices %>% 
  merge(fp_template, by = c("fips", "spcd", "product"), all.y = T) %>% 
  .[ , missing := as.numeric(fips %in% price.missing)] %>% 
  merge(ecoreg %>% select(fips, section), by = "fips") %>% 
  .[!is.na(spcd), ] %>% 
  # Recode select ecoregions because inventory data are entirely missing in some ecoregions
  .[section == 3223, section := 3222] %>% 
  .[section == 3421, section := 3427] %>% 
  .[section == 3425, section := 3427] %>% 
  .[section == 3426, section := 3427] %>% 
  # Set prodn per acre in missing counties equal within nearest adjacent ecological section within same province
  .[ , mean_price := mean(price, na.rm = TRUE), by = c("spcd","product","section")] %>% 
  .[ , price := ifelse(missing == 1, mean_price, price)] %>% 
  .[ , c("missing","mean_price") := NULL ]

write.csv(forest_prices, "processing/net_returns/forest/county_sp_product_prices.csv")


