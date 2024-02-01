####################################################
# Matt Wibbenmeyer
# January 31, 2022
# Transition table
####################################################

## Load/install packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse,
               dplyr,
               readxl,
               sf,
               tidycensus,
               haven,
               stringr,
               data.table
)

# Set working directory to land-use 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)); setwd("../../../")

savefig <- function(path,width,height) {
  lapply(c("png","pdf"), function(ftype) 
    ggplot2::ggsave(paste0(path,".",ftype),
                    device=ftype,width=width,height=height))
}

`%ni%` <- Negate(`%in%`)
options(tigris_use_cache = TRUE) #Tell tidycensus to cache shapefiles for future sessions

#census_api_key("", overwrite = TRUE, install = TRUE) # set API key

# Import data ------------------------------------------------------------------

aea <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

#Import county shapefile using tidycensus - b19013_001 is arbitrarily chosen
states <- state.abb[state.abb %ni% c("AK","HI")]
counties <- get_acs(state = states, geography = "county", year = 2010, variables = "B19013_001", geometry = TRUE) %>%
  select(-c("variable","estimate","moe")) %>%
  st_transform(aea)
ecoregions <- read.csv("raw_data/misc/ecoregions/interpolated_ecocd_counties.csv") %>% 
                mutate(fips = str_pad(fips, width = 5, pad = "0", side = "left")) %>% 
                as.data.table()

study.counties <- list(c("01063"),c("55099"),c("16009","16035")) %>% unlist()


points <- read_dta("processing/pointpanel/countypanel_estimation_bal.dta") %>% as.data.table()

df <- points[initial_use != "Rural" & final_use != "Rural"] %>% 
        .[ , .(acresk = sum(acresk)), by = c("fips","initial_use","final_use","year")] %>%
        .[initial_use %ni% c("Federal","Water") & final_use %ni% c("Federal","Water")] %>% 
        merge(ecoregions, by = 'fips')

conversion_rates <- function(countyfips) {
  
  df.init <- df[fips == countyfips & year >= 2002 & year <= 2015] %>% 
    .[ , .(initial_acres = sum(acresk)) , by = c("initial_use","year")]
  
  df.conv <- df[fips == countyfips & year >= 2002 & year <= 2015] %>% 
            .[ , .(total_acres = sum(acresk)) , by = c("initial_use","final_use","year")]
  
  conv.rates <- merge(df.conv, df.init, by = c("initial_use","year")) %>% 
          .[ , prob := total_acres/initial_acres]   %>% 
          .[ , .(mean_prob = mean(prob)), by = c("initial_use","final_use")]
  
  return(conv.rates)
  
}

area_change <- function(df, conv.rates) {
  
  df.conv <- merge(conv.rates, df, by.x = "initial_use", by.y = "final_use")
  
  out <- df.conv[ , final_acres := mean_prob*initial_acres] %>% 
              .[ , .(initial_acres = sum(final_acres, na.rm = T)), by = c("final_use","fips","year")]
  
  }

calc_land_use_change <- function(countyfips, timesteps) {
  
  conv.rates <- conversion_rates(countyfips)
  
  # Set up data set
  df.init <- df[fips == countyfips & year == 2015] %>% 
    .[ , .(initial_acres = sum(acresk, na.rm = T)), by = c("final_use", "fips","year")]  
  
  initial <- df.init %>% mutate(timestep = 0)
  final <- initial
  
  for (n in seq(1,timesteps)) {
    
    print(n)
    df.init <- area_change(df = df.init, conv.rates = conv.rates) %>% 
                mutate(timestep = n)  
    final <- rbind(final, df.init)
    
  }
  
  return(final %>% 
           mutate(year = year + timestep*5) %>% 
           mutate(timestep = ifelse(year == 2015, NA, timestep)))
  
}

calc_hist_land_use <- function(countyfips) {
  
  df.hist <- df[fips == countyfips & year >= 1987 & year <= 2015, 
     .(initial_acres = sum(acresk, na.rm = T)), 
     by = c("initial_use","year")] %>% 
    .[ , ':=' (fips = countyfips,
               final_use = initial_use,
               timestep = NA)] %>% 
    .[ , initial_use := NULL] %>% 
    .[year < 2015, year := year - 5] %>% 
    .[year == 2015, year := year - 3]
  
  return(df.hist)

}



final.df <- rbind(do.call(rbind, lapply(study.counties, function(c) calc_land_use_change(c,10))),
                  do.call(rbind, lapply(study.counties, function(c) calc_hist_land_use(c)))) %>% 
              arrange(fips,year,final_use)
dst <- "processing/misc/aflac/"
dir.create(dst, recursive = T, showWarnings = F)
write.csv(final.df, paste0(dst,"study_county_landuse_change_countytrend.csv"))

all.df <- rbind(do.call(rbind, lapply(unique(counties$GEOID), function(c) calc_land_use_change(c,10))),
                  do.call(rbind, lapply(unique(counties$GEOID), function(c) calc_hist_land_use(c)))) %>% 
  arrange(fips,year,final_use)
dst <- "processing/misc/aflac/"
dir.create(dst, recursive = T, showWarnings = F)
write.csv(all.df, paste0(dst,"study_county_landuse_change_allcounties.csv"))




