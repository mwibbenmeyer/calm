####################################################
# Matt Wibbenmeyer
# July 27, 2021
# Map simulation results
####################################################

#devtools::install_github("kwstat/pals")   

## Load/install packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse,
               dplyr,
               readxl,
               sf,
               tidycensus,
               haven,
               stringr,
               data.table,
               RStata,
               cdlTools,
               nngeo,
               colorspace,
               pals)


`%ni%` <- Negate(`%in%`)

savefig <- function(filename, dst ,width,height) {
  
  dir.create(dst, showWarnings = F, recursive = T)
  
  lapply(c("png","pdf"), function(ftype) 
    ggplot2::ggsave(paste0(dst, filename,".",ftype),
                    device=ftype,width=width,height=height))
}


setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # sets directory to the current directory
setwd('../../') # relative paths to move directory to the root project directory

aea <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"


# Import data -------------------------------------------------------------

#Import county shapefile using tidycensus - b19013_001 is arbitrarily chosen
states <- state.abb[state.abb %ni% c("AK","HI")]
counties <- get_acs(state = states, geography = "county", year = 2010, variables = "B19013_001", geometry = TRUE) %>%
  select(-c("variable","estimate","moe")) %>%
  mutate(fips = str_pad(GEOID, width = 5, pad = "0", side = "left")) %>% 
  st_transform(aea) %>% 
  st_make_valid()
ecoregions <- read.csv("raw_data/misc/ecoregions/interpolated_ecocd_counties.csv") %>% 
  mutate(fips = str_pad(fips, width = 5, pad = "0", side = "left")) %>% 
  as.data.table()
ecoregions_sf <- merge(counties, ecoregions %>% select(fips, ecoregion), by = 'fips') %>% 
              group_by(ecoregion) %>%
              summarize(num.counties = sum(1))
              

points <- read_dta("processing/pointpanel/countypanel_estimation_bal.dta") %>% as.data.table()

hist <- points[initial_use != "Rural" & final_use != "Rural"] %>% 
  .[ , .(acresk = sum(acresk)), by = c("fips","initial_use","final_use","year")] %>%
  .[initial_use %ni% c("Federal","Water") & final_use %ni% c("Federal","Water")] %>% 
  .[ , final_use := ifelse(final_use %in% c("Pasture","Range","CRP"), "Other", final_use)] %>% 
  merge(ecoregions, by = 'fips') %>% 
  .[ , .(acres = sum(acresk, na.rm = T)), by = c('final_use','ecoregion','year')] %>% 
  .[year != 2015] %>% 
  .[year %in% seq(1982,2012,10)]

project.path <- "processing/misc/dynamics_issue_brief/"

sq <- do.call(rbind, lapply(seq(1,10), function(int)
  read.csv(sprintf("%ssimulations/status_quo/carbon_model_input/acreages_%s.csv", project.path, int)) %>% 
    mutate(year = 2012+int*5) %>% 
    mutate(fips = ifelse(fips == "46102","46113",fips)) %>% #Change Shannon County, SD to Oglala Lakota County, SD 
    mutate(fips = str_pad(fips, width = 5, side = "left", pad = "0")) %>% 
    group_by(ecoregion, year, initial_use) %>% 
    summarize(acres = sum(acres, na.rm = T)) %>% 
    rename(final_use = initial_use) %>% 
    filter(year %in% seq(2022,2062,10)))) %>% 
    rbind(hist) %>% 
    as.data.table()

forest <- do.call(rbind, lapply(seq(1,10), function(int)
  read.csv(sprintf("%ssimulations/forest_increase_2/carbon_model_input/acreages_%s.csv", project.path, int)) %>% 
    mutate(year = 2012+int*5) %>% 
    mutate(fips = ifelse(fips == "46102","46113",fips)) %>% #Change Shannon County, SD to Oglala Lakota County, SD 
    mutate(fips = str_pad(fips, width = 5, side = "left", pad = "0")) %>% 
    group_by(ecoregion, year, initial_use) %>% 
    summarize(acres = sum(acres, na.rm = T)) %>% 
    rename(final_use = initial_use) %>% 
   filter(year %in% seq(2022,2062,10)))) %>% 
  rbind(hist) %>% 
  as.data.table()

urban <- do.call(rbind, lapply(seq(1,9), function(int)
  read.csv(sprintf("%ssimulations/urbanization_decline/carbon_model_input/acreages_%s.csv", project.path, int)) %>% 
    mutate(year = 2012+int*5) %>% 
    mutate(fips = ifelse(fips == "46102","46113",fips)) %>% #Change Shannon County, SD to Oglala Lakota County, SD 
    mutate(fips = str_pad(fips, width = 5, side = "left", pad = "0")) %>% 
    group_by(ecoregion, year, initial_use) %>% 
    summarize(acres = sum(acres, na.rm = T)) %>% 
    rename(final_use = initial_use) %>% 
  filter(year %in% seq(2022,2062,10)))) %>% 
  rbind(hist) %>% 
  as.data.table()



# Plotting functions ------------------------------------------------------

conversion_rates <- function(dataframe, region, yr) {
  
  last_yr = yr-10

  df.init <- dataframe[ecoregion == region & year == last_yr] %>% 
    .[ , .(acres = sum(acres, na.rm =T)) , by = c("final_use")]
  
  df.new <- dataframe[ecoregion == region & year == yr] %>% 
    .[ , .(acres = sum(acres, na.rm = T)) , by = c("final_use")] 
  
  df.total <- dataframe[ecoregion == region & year == yr] %>% 
    .[ , .(total_acres = sum(acres, na.rm = T))]
  
  df2 <- merge(df.init, df.new, by = "final_use", suffixes = c("_1","_2")) %>% 
    mutate(total_acres = df.total[[1]],
           change = (acres_2 - acres_1)/(total_acres),
           year = yr,
           ecoregion = region) %>%
    select(final_use, ecoregion, year, change)
  
  return(df2)
  
}

create_change_df <- function(scen.name) {
  
  df <- get(scen.name)
  
  cr <- do.call(rbind, lapply(unique(df$ecoregion), function(e) 
    do.call(rbind, lapply(seq(1992,2062,10), function(y) conversion_rates(df, e, y)))))
  
  return(cr)
  
}

pct.change <- create_change_df("sq")


# Plotting functions ------------------------------------------------------

plot_pct_change <- function(scenario, save = F, dst, filename, max = NA) {
  
  dataframe <- create_change_df(scenario)
  
  reg.change <- merge(ecoregions_sf, dataframe, by = 'ecoregion')
  
  if (!is.na(max)) reg.change <- reg.change %>% 
                                  mutate(change = 100*change) %>% 
                                  mutate(change = ifelse(abs(change) > max, max*sign(change), change))
    
  p <- ggplot(data = reg.change) + 
    geom_sf(aes(fill = change), color = NA) +
    scale_fill_continuous_diverging(h1 = 187, h2 = 339, c1 = 100, l1 = 25, l2 = 95, rev = TRUE) + 
    facet_grid(year~final_use) + 
    theme_void() + 
    labs(fill = "Percent change in\n ecoregion area")
  
  if (!is.na(max)) {
    p <- p + labs(caption = sprintf("Note: Changes are top-coded at an absolute value of %s for visualization.",max))
  }
  
  savefig(filename = filename, dst = dst, width = 6.5*1.75, height = 4.5*1.75)
  
  return(p)
  
}

plot_diff_pct_change <- function(scenario, save = F, dst = NA, filename = NA, max = NA) {
  
  dataframe <- create_change_df(scenario)
  sq.dataframe <- create_change_df('sq')
  
  dataframe <- merge(dataframe,sq.dataframe, by = c("final_use","ecoregion","year"), suffixes = c(".scen",".sq"))
  dataframe$diff_change <- dataframe$change.scen - dataframe$change.sq
  
  reg.change <- merge(ecoregions_sf, dataframe, by = 'ecoregion')
  
  if (!is.na(max)) reg.change <- reg.change %>% 
    mutate(diff_change = 100*diff_change) %>% 
    mutate(diff_change = ifelse(abs(diff_change) > max, max*sign(diff_change), diff_change))
  
  p <- ggplot(data = reg.change %>% filter(year > 2012)) + 
    geom_sf(aes(fill = diff_change), color = NA) +
    scale_fill_continuous_diverging(h1 = 187, h2 = 339, c1 = 100, l1 = 25, l2 = 95, rev = TRUE) + 
    facet_grid(year~final_use) + 
    theme_void() + 
    labs(fill = "Difference in\npercent change")
  
  if (!is.na(max)) {
    p <- p + labs(caption = sprintf("Note: Changes are top-coded at an absolute value of %s for visualization.",max))
  }
  
  if (save == T) savefig(filename = filename, dst = dst, width = 6.5*1.75, height = 4*1.75)
  
  return(p)
  
}

plot_diff_pct_change("urban", save = F)

plot_pct_change("sq", save = T,  max = 5, dst = "results/misc/dynamics_issue_brief/",
                filename = "status_quo_change_pct_use")
plot_pct_change("forest", save = T,  max = 5, dst = "results/misc/dynamics_issue_brief/",
                filename = "forest_change_pct_use")
plot_pct_change("urban", save = T,  max = 5, dst = "results/misc/dynamics_issue_brief/",
                filename = "urban_change_pct_use")

plot_diff_pct_change("forest", save = T, dst = "results/misc/dynamics_issue_brief/",
                     filename = "forest_diff_pct_change", max = 1)
plot_diff_pct_change("urban", save = T, dst = "results/misc/dynamics_issue_brief/",
                     filename = "urban_diff_pct_change")


