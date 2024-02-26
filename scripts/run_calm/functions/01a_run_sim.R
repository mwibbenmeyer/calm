
`%ni%` <- Negate(`%in%`)

# Import data -------------------------------------------------------------

# Model coefficient estimates, output from Stata
estimates <- sprintf("%scoefficient_estimates.est", input_path)

# Initial land uses by county and LCC
df <- read_csv(sprintf("%s/sim_df.csv",input_path), 
               col_types = cols(fips = "c")) %>% dplyr::select(c(-1)) %>%
        mutate(ecoregion = ifelse(fips == "46113", 331, ecoregion)) %>%     
        as.data.table()

# TAMM regions, needed for adjusting forest returns following Lubowski, Plantinga, Stavins
tamm <- read_xlsx(sprintf("%s/tamm_regions.xlsx", input_path)) %>% 
          rbind(data.frame(state = "MD", tamm_region = "Northeast")) # Add Maryland, which is missing
tamm$stfips <- str_pad(cdlTools::fips(tamm$state), width = 2, pad = "0", side = "left")

# Returns by county and land use
returns.df <- read_csv(sprintf("%s/returns.csv",input_path), 
                       col_types = cols(fips = "c")) %>% select(c(-1)) %>% 
                  mutate(stfips = substr(fips,1,2)) %>%         
                  merge(tamm, by = "stfips") %>% 
                select(-state)

# Crop returns by county and crop, needed for adjusting crop returns
crop_returns <- read_csv(sprintf("%s/crop_level_returns.csv", input_path)) %>% 
                  filter(year %in% seq(2007,2011)) # Need only these years for simulation

# Forest returns data  ----------------------------------------------------

cf_template <- CJ(fips = unique(df$fips), sptype = c("hardwood", "softwood"))
cf_per_acresk <-      read.csv(sprintf("%s/forest_return_inputs/sptype_cf_per_acresk.csv", input_path)) %>% 
  as.data.table() %>% 
  .[ , fips := str_pad(fips, width = 5, side = "left", pad = "0")] %>% 
  .[ , X := NULL]
cf_per_acresk.complete <- cf_per_acresk %>% merge(cf_template, by = c("fips", "sptype"), all.y = T) # Make sure we have all combns of county and species type

harv_rates <-         read.csv(sprintf("%s/forest_return_inputs/sptype_harv_rates.csv", input_path)) 
rem_product_share <-  read.csv(sprintf("%s/forest_return_inputs/removal_product_share.csv", input_path))

# Shares of each species (spcd) that go to sawtimber/pulpwood production
hw_shares <-          read.csv(sprintf("%s/forest_return_inputs/sp_product_shares_hardwood.csv", input_path)) %>% 
                        mutate(fips = str_pad(fips, width = 5, side = "left", pad = "0")) %>% 
                        as.data.table()
sw_shares <-          read.csv(sprintf("%s/forest_return_inputs/sp_product_shares_softwood.csv", input_path)) %>% 
                        mutate(fips = str_pad(fips, width = 5, side = "left", pad = "0")) %>% 
                        as.data.table()
shares <-             rbind(hw_shares %>% mutate(sptype = "hardwood"),
                        sw_shares %>% mutate(sptype = "softwood"))



forest_prices <- read.csv(sprintf("%s/forest_return_inputs/county_sp_product_prices.csv", input_path)) %>% 
                  mutate(fips = str_pad(fips, width = 5, side = "left", pad = "0")) %>% 
                  mutate(stfips = substr(fips,1,2)) %>% 
                  merge(tamm, by = "stfips") %>% 
                  select(fips,spcd,product,price,tamm_region) %>% 
                  as.data.table()

# Ecoregion data - needed for filling in counties with limited forest inventory data
ecoreg <- read_excel(sprintf("%s/forest_return_inputs/county to ecoregion.xls", input_path), sheet = "county to ecoregion") %>% 
                  mutate(fips = str_pad(Fips, side = "left", width = 5, pad = "0"),
                         section = paste0(P,S)) %>%
                  select(-Fips) %>% 
                  # Adjust FIPS codes for county FIPS code changes
                  mutate(fips = ifelse(fips == "12025","12086",fips),
                         fips = ifelse(fips == "51560","51005",fips),
                         fips = ifelse(fips == "51780","51083",fips),
                         fips = ifelse(fips == "30113","30031",fips))
ecoreg <- ecoreg %>% 
                  rbind(ecoreg %>% filter(fips == "01087") %>% 
                          mutate(fips = "01101", County = "Montgomery")) %>%  # Add missing Montgomery county = neighboring Macon county
                  rbind(ecoreg %>% filter(fips == "08013") %>% 
                          mutate(fips = "08014", County = "Broomfield"))





# Function to simulate land use change given a returns vector ---------------

simulate_luc <- function(df_input, returns, crop_returns, forest_returns,
                         endog_crop_returns = TRUE, endog_forest_returns = TRUE) {
  
  #Need to restrict to only crop because some rows in order to account for the variety of possible next-period uses
  df.a <- df_input %>% as.data.table() %>% 
            .[final_use == "Crop", .(acres = sum(initial_acres)), by = c('fips','lcc','initial_use')]

  df.r <- left_join(df_input, returns, by = c('fips','year'))
  
  stata_src <- sprintf('
  
    cd %s
    do "scripts/run_calm/functions/01b_program_calc_phats_lcc.do"
    qui calc_phat using "%s"
    
    ', wd, estimates)
    
  df.out <- stata(stata_src, data.in = df.r, data.out = T)
  
  df.p <- df.out %>% select(fips,lcc, ecoregion, year, initial_use, final_use, initial_acres, phat) %>% 
    pivot_wider(id_cols = c("fips","lcc","ecoregion",'year',"initial_use"),
                names_from = "final_use",
                names_prefix = "p.",
                values_from = "phat")   %>%
    dplyr::mutate(across(where(is.numeric), replace_na, 0))   %>%
    merge(df.a, ., by = c('fips','lcc','initial_use')) %>% 
    dplyr::mutate(across(starts_with("p."), ~ .x*acres, 
                  .names = "a.{substr(col,3,nchar(col))}"))
  
  # Data frame for feeding into carbon model
  df.c <- df.p %>%
    dplyr::select(-starts_with("p."), -acres) %>% 
    #Rename columns prior to pivot long
    dplyr::mutate(across(starts_with("a."), ~ .x, 
                  .names = "{substr(col,3,nchar(col))}"))  %>% 
    pivot_longer(cols = c('Crop','Forest','Other','Urban'),
                 names_to = "final_use",
                 names_repair = "unique",
                 values_to = "acres") %>% 
    dplyr::select(-starts_with("a.")) %>% 
    group_by(ecoregion,initial_use,final_use) %>% 
    summarize(acres = sum(acres, na.rm = TRUE))
  
  # Data frame for feeding back into simulation
  df.l <- df.p %>% dplyr::mutate(across(starts_with("a."), ~ .x, 
                                 .names = "{substr(col,3,nchar(col))}")) %>% #Rename columns prior to pivot long
    dplyr::select(-initial_use, -acres) %>% # Remove old initial acres and initial use columns
    pivot_longer(cols = c('Crop','Forest','Other','Urban'),
                 names_to = "initial_use",
                 names_repair = "unique",
                 values_to = "initial_acres") %>% 
    as.data.table() %>%  
    .[ , .(initial_acres = sum(initial_acres, na.rm = TRUE)), by = c("fips","ecoregion","lcc","year","initial_use")] %>% 
    .[ , ":=" (Crop = 1, Forest = 1, Other = 1, Urban = 1)] %>%
    pivot_longer(cols = c('Crop','Forest','Other','Urban'),
                 names_to = "final_use",
                 values_to = "final_acres") %>% 
    dplyr::select(-final_acres) %>% 
    dplyr::select(fips, ecoregion, year, lcc, initial_use, final_use, initial_acres) %>% 
    merge(df.r %>% select(fips,lcc,year,initial_use,final_use,resid),
          by = c('fips','lcc','year','initial_use','final_use')) %>% 
    as.data.table()
  

  # Update crop returns
  if (endog_crop_returns == T) {
    crop.update <- update_crop_returns(returns = returns, crop_returns = crop_returns, new = df.l, orig = df_input)
    crop_level_returns.current <- crop.update[[1]]
    returns.current <- crop.update[[2]]
  } else {
    crop_level_returns.current <- crop_returns
    returns.current <- returns
  }
  
  # Update forest returns
  if (endog_forest_returns == T) {
    forest.update <- update_forest_returns(returns = returns.current, forest_prices = forest_returns, new = df.l, orig = df_input)
    forest_sp_returns.current <- forest.update[[1]]
    returns.current <- forest.update[[2]]
  } else {
    forest_sp_returns.current <- forest_prices
    returns.current <- returns.current
  }

  return(list(df.l, df.c, returns.current, crop_level_returns.current, forest_sp_returns.current))
  
  }


run_sim <- function(returns = returns.df, 
                    ints, # Number of intervals over which to run model
                    scenario_name = NA, # Path to which to write inputs for carbon model
                    endog_crop_returns = T, # Endogenize crop returns? 
                    endog_forest_returns = T # Endogenize forest returns?
                    ) {
  
  sim <- df
  returns.current <- returns
  crop_returns.current <- crop_returns
  forest_returns.current <- forest_prices
  
  carbon_input <- NULL
  
  for (int in seq(1,ints)) {
    
    print(int)
    
    sim <- quiet(simulate_luc(df_input = sim, 
                              returns = returns.current, 
                              crop_returns = crop_returns.current,
                              forest_returns = forest_returns.current,
                              endog_crop_returns = endog_crop_returns,
                              endog_forest_returns = endog_forest_returns))
    
    
    carbon_input[[length(carbon_input)+ 1]] <- sim[[2]]
    
    #Save data frame for use in carbon model
    if (!is.na(scenario_name)) {
      results_path = sprintf("%s%s/", output_path, scenario_name)
      dir.create(sprintf("%s/carbon_model_input/", results_path), recursive = TRUE, showWarnings = FALSE)
      write.csv(sim[[2]], sprintf("%s/carbon_model_input/timestep_%s.csv",results_path, int))
    }
    
    #Return data frame in same format as source dataframe
    
    #Update returns
    returns.current <- sim[[3]]
    crop_returns.current <- sim[[4]]
    forest_returns.current <- sim[[5]]

    sim <- sim[[1]]
    
    
  }

  return(list(sim, carbon_input))

  }
