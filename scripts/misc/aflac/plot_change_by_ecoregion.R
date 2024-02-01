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

savefig <- function(filename, dst ,width,height) {
  
  dir.create(dst, showWarnings = F, recursive = T)
  
  lapply(c("png"), function(ftype) 
    ggplot2::ggsave(paste0(dst, filename,".",ftype),
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


conversion_rates <- function(region, yr) {
  
  last_yr = ifelse(yr != 2015, yr-5, yr-3)
  
  df.init <- df[ecoregion == region & year == last_yr] %>% 
    .[ , .(acres = sum(acresk)) , by = c("final_use")]
  
  df.new <- df[ecoregion == region & year == yr] %>% 
    .[ , .(acres = sum(acresk)) , by = c("final_use")] 
  
  df.total <- df[ecoregion == region & year == yr] %>% 
    .[ , .(total_acres = sum(acresk))]
  
  df2 <- merge(df.init, df.new, by = "final_use", suffixes = c("_1","_2")) %>% 
          mutate(total_acres = df.total[[1]],
                  change = (acres_2 - acres_1)/(total_acres),
                 year = yr,
                 ecoregion = region) %>%
          select(final_use, ecoregion, year, change)
  
  return(df2)
}

cr <- do.call(rbind, lapply(unique(df$ecoregion), function(e) 
  do.call(rbind, lapply(unique(df$year), function(y) conversion_rates(e, y)))))

df2 <- merge(counties, ecoregions, by.x = "GEOID", by.y = "fips") %>% 
        merge(cr %>% filter(final_use == "Forest"), by = "ecoregion")

max <- 0.01

ggplot(df2 %>% mutate(change = ifelse(abs(change) > max, max*sign(change), change))) + 
  geom_sf(aes(fill = change, color = change)) +
  scale_fill_continuous_diverging(h1 = 187, h2 = 339, c1 = 100, l1 = 25, l2 = 95, rev = TRUE) + 
  scale_color_continuous_diverging(h1 = 187, h2 = 339, c1 = 100, l1 = 25, l2 = 95, rev = TRUE) + 
  facet_wrap(~year) +
  theme_void()


dst = "results/misc/dynamics_issue_brief/"
filename = "forest_change_by_ecoregion"
savefig(filename = filename, dst = dst, width = 6.5*2, height = 4.5*2)


