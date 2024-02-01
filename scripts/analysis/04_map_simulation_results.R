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

#Import county shapefile using tidycensus - b19013_001 is arbitrarily chosen
states <- state.abb[state.abb %ni% c("AK","HI")]
counties <- get_acs(state = states, geography = "county", year = 2010, variables = "B19013_001", geometry = TRUE) %>%
  select(-c("variable","estimate","moe")) %>%
  st_transform(aea) %>% 
  st_make_valid()
  
base <- read.csv("processing/misc/dynamics_issue_brief/simulations/base/acreages.csv") %>% 
              mutate(fips = ifelse(fips == "46102","46113",fips)) %>% #Change Shannon County, SD to Oglala Lakota County, SD 
              mutate(fips = str_pad(fips, width = 5, side = "left", pad = "0")) %>% 
              merge(counties, by.x = "fips", by.y = "GEOID")

sq <- read.csv("processing/misc/dynamics_issue_brief/simulations/status_quo/acreages.csv") %>% 
  mutate(fips = ifelse(fips == "46102","46113",fips)) %>% #Change Shannon County, SD to Oglala Lakota County, SD 
  mutate(fips = str_pad(fips, width = 5, side = "left", pad = "0")) %>% 
  merge(counties, by.x = "fips", by.y = "GEOID")


urban <- read.csv("processing/misc/dynamics_issue_brief/simulations/urbanization_decline_2/acreages.csv") %>% 
              mutate(fips = ifelse(fips == "46102","46113",fips)) %>% #Change Shannon County, SD to Oglala Lakota County, SD 
              mutate(fips = str_pad(fips, width = 5, side = "left", pad = "0")) %>% 
              merge(counties, by.x = "fips", by.y = "GEOID")  

forest <- read.csv("processing/misc/dynamics_issue_brief/simulations/forest_increase_2/acreages.csv") %>% 
              mutate(fips = ifelse(fips == "46102","46113",fips)) %>% #Change Shannon County, SD to Oglala Lakota County, SD 
              mutate(fips = str_pad(fips, width = 5, side = "left", pad = "0")) %>% 
              merge(counties, by.x = "fips", by.y = "GEOID") 

plot_changes <- function(scen.name, top_code = 0.1,
                                       save = FALSE, 
                                       dst = NA, filename = NA) {
  
  df <- merge(base, get(scen.name) %>% dplyr::select(fips, initial_use, acres), 
              by = c('fips','initial_use'), 
              suffixes = c(".old",".case1")) %>% 
          group_by(fips) %>% 
          mutate(total.acres = sum(acres.old, na.rm = T),
                 change.pct.use = 100*(acres.case1/total.acres) - 100*(acres.old)/(total.acres)) %>% 
          st_as_sf()
  
  if (!is.na(top_code)) df <- df %>% mutate(change.pct.use = ifelse(abs(change.pct.use) > top_code, 
                                                         sign(change.pct.use)*top_code,
                                                         change.pct.use))
  p <- ggplot(data = df) + 
    geom_sf(aes(fill = change.pct.use),
            color = NA) +
    facet_wrap(~initial_use) + 
    scale_fill_continuous_diverging(h1 = 187, h2 = 339, c1 = 100, l1 = 25, l2 = 95, rev = TRUE) + 
    # scale_fill_viridis_c() +
    labs(fill = "Change in\npercentage county\nland use") + 
    theme_void()
  
  if (!is.na(top_code)) {
    p <- p + labs(caption = sprintf("Note: Differences are top-coded at an absolute value of %s for visualization.",top_code))
  }
  
  savefig(filename = filename, dst = dst, width = 6.5*1.75, height = 4.5*1.75)
  
  return(p)

}

plot_difference_in_changes <- function(scen.name, top_code = 0.1,
                         save = FALSE, 
                         dst = NA, filename = NA) {

  df <- merge(base, sq %>% dplyr::select(fips, initial_use, acres), 
              by = c('fips','initial_use'), 
              suffixes = c(".old",".case1")) %>% 
        merge(get(scen.name) %>% dplyr::select(fips, initial_use, acres), 
              by = c('fips','initial_use')) %>% 
              rename(acres.case2 = acres) %>% 
              ungroup() %>% group_by(fips) %>% 
              mutate(total_acres = sum(acres.old)) %>% 
              ungroup() %>% group_by(initial_use) %>% 
              mutate(pct_acres1 = acres.case1/total_acres,
                     pct_acres2 = acres.case2/total_acres,
                     diff_acres = 100*(pct_acres2 - pct_acres1)) %>% 
              st_as_sf() %>% 
              mutate(initial_use = factor(initial_use, levels = c("Crop","Forest","Urban","Other")))
  
  if (!is.na(top_code)) df <- df %>% mutate(diff_acres = ifelse(abs(diff_acres) > top_code, 
                                                                sign(diff_acres)*top_code,
                                                                diff_acres))
  
  p <- ggplot(data = df) + 
          geom_sf(aes(fill = diff_acres),
                  color = NA) +
          facet_wrap(~initial_use) +  
          scale_fill_continuous_diverging(h1 = 187, h2 = 339, c1 = 100, l1 = 25, l2 = 95, rev = TRUE) + 
          # scale_fill_viridis_c() +
          labs(fill = "Difference in change in\npercentage county land use\nacross scenarios") +
          theme_void()
  
  if (!is.na(top_code)) {
    p <- p + labs(caption = sprintf("Note: Differences are top-coded at an absolute value of %s for visualization.",top_code))
  }
  
  savefig(filename = filename, dst = dst, width = 6.5*1.75, height = 4.5*1.75)
  
  return(p)

}

plot_changes("forest", top_code = 50, save = TRUE,
            dst = "results/misc/dynamics_issue_brief/",
            filename = "forest_change_map_2")

plot_changes("urban", top_code = 50, save = TRUE,
             dst = "results/misc/dynamics_issue_brief/",
             filename = "urban_change_map_2")

plot_difference_in_changes("forest", top_code = 50, save = TRUE,
             dst = "results/misc/dynamics_issue_brief/",
             filename = "forest_diff_change_map_2")

plot_difference_in_changes("urban", top_code = 50, save = TRUE,
                           dst = "results/misc/dynamics_issue_brief/",
                           filename = "urban_diff_change_map_2")

