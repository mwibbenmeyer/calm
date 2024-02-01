
# Import data -------------------------------------------------------------

# Model coefficient estimates, output from Stata
estimates <- sprintf("%s/coefficient_estimates.est", input_path)

# Initial land uses by county and LCC
df <- read_csv(sprintf("%s/sim_df.csv",input_path), 
               col_types = cols(fips = "c")) %>% dplyr::select(c(-1)) %>% as.data.table()

# TAMM regions, needed for adjusting forest returns following Lubowski, Plantinga, Stavins
tamm <- read_xlsx(sprintf("%s/tamm_regions.xlsx", input_path))
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

# Forest demand elasticities, from Lubowski, Plantinga, and Stavins
forest_demand_elasticities <- data.frame(regions = c("Pacific Northwest",
                                                     "Pacific Southwest",
                                                     "Rocky Mountains",
                                                     "North Central",
                                                     "South Central",
                                                     "Southeast",
                                                     "Northeast"),
                                         e_dmd_forest = c(-0.3,
                                                          -0.497,
                                                          -0.054,
                                                          -0.141,
                                                          -0.193,
                                                          -0.285,
                                                          -0.029))


# Function to simulate land use change given a returns vector ---------------

simulate_luc <- function(df_input, returns, crop_returns,
                         endog_crop_returns = TRUE, endog_forest_returns = TRUE) {
  
  #Need to restrict to only crop because some rows in order to account for the variety of possible next-period uses
  df.a <- df_input %>% as.data.table() %>% 
            .[final_use == "Crop", .(acres = sum(initial_acres)), by = c('fips','lcc','initial_use')]

  df.r <- left_join(df_input, returns, by = c('fips','year'))
  
  stata_src <- sprintf('
  
    cd L:/Project-Land_Use/
    qui do "scripts/run_calm/functions/01b_program_calc_phats_lcc.do"
    
    qui calc_phat using "%s"
    
    ', estimates)
    
  df.out <- stata(stata_src, data.in = df.r, data.out = TRUE)
  
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
          by = c('fips','lcc','year','initial_use','final_use'))
  

  # Update crop returns
  if (endog_crop_returns == T) {
    crop.update <- update_crop_returns(crop_returns = crop_returns, new = df.l, orig = df_input)
    crop_returns.current <- crop.update[[1]]
    returns.current <- crop.update[[2]]
  } else {
    crop_returns.current <- crop_returns
    returns.current <- returns
  }
  

  return(list(df.l, df.c, returns.current, crop_returns.current))
  
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
  carbon_input <- NULL
  
  for (int in seq(1,ints)) {
    
    print(int)
    
    sim <- quiet(simulate_luc(df_input = sim, 
                              returns = returns.current, 
                              crop_returns = crop_returns.current,
                              endog_crop_returns = endog_crop_returns,
                              endog_forest_returns = endog_forest_returns))
    
    
    carbon_input[[length(carbon_input)+ 1]] <- sim[[2]]
    
    #Save data frame for use in carbon model
    if (!is.na(scenario_name)) {
      results_path = sprintf("%s%s/", output_path, scenario_name)
      dir.create(results_path, recursive = TRUE, showWarnings = FALSE)
      write.csv(sim[[2]], sprintf("%s/carbon_model_input/timestep_%s.csv",results_path, int))
    }
    
    #Return data frame in same format as source dataframe
    
    #Update returns
    returns.current <- sim[[3]]
    crop_returns.current <- sim[[4]]

    sim <- sim[[1]]
    
    
  }

  return(list(sim, carbon_input))

  }
