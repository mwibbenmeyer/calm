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

savefig <- function(path,width,height) {
  lapply(c("png","pdf"), function(ftype) 
    ggplot2::ggsave(paste0(path,".",ftype),
                    device=ftype,width=width,height=height))
}

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # sets directory to the current directory
setwd('../../../') # relative paths to move directory to the root project directory

aea <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

#Import county shapefile using tidycensus - b19013_001 is arbitrarily chosen
states <- state.abb[state.abb %ni% c("AK","HI")]
counties <- get_acs(state = states, geography = "county", year = 2010, variables = "B19013_001", geometry = TRUE) %>%
  select(-c("variable","estimate","moe")) %>%
  st_transform(aea) %>% 
  st_make_valid()
  
original <- read.csv("processing/simulation/sims_221130/base/acreages.csv") %>% 
              mutate(fips = ifelse(fips == "46102","46113",fips)) %>% #Change Shannon County, SD to Oglala Lakota County, SD 
              mutate(fips = str_pad(fips, width = 5, side = "left", pad = "0")) %>% 
              merge(counties, by.x = "fips", by.y = "GEOID")

case_1 <- read.csv("processing/simulation/sims_221130/status_quo/acreages.csv") %>% 
              mutate(fips = ifelse(fips == "46102","46113",fips)) %>% #Change Shannon County, SD to Oglala Lakota County, SD 
              mutate(fips = str_pad(fips, width = 5, side = "left", pad = "0")) %>% 
              merge(counties, by.x = "fips", by.y = "GEOID")          

case_2 <- read.csv("processing/simulation/sims_221130/crop_returns_1.1/acreages.csv") %>% 
              mutate(fips = ifelse(fips == "46102","46113",fips)) %>% #Change Shannon County, SD to Oglala Lakota County, SD 
              mutate(fips = str_pad(fips, width = 5, side = "left", pad = "0")) %>% 
              merge(counties, by.x = "fips", by.y = "GEOID")          

df <- merge(original, case_1 %>% dplyr::select(fips, initial_use, acres), 
            by = c('fips','initial_use'), 
            suffixes = c(".old",".case1")) %>% 
      merge(case_2 %>% dplyr::select(fips, initial_use, acres), 
            by = c('fips','initial_use')) %>% 
            rename(acres.case2 = acres) %>% 
            ungroup() %>% group_by(fips) %>% 
            mutate(total_acres = sum(acres.old)) %>% 
            ungroup() %>% group_by(initial_use) %>% 
            mutate(pct_acres1 = acres.case1/total_acres,
                   pct_acres2 = acres.case2/total_acres,
                   diff_acres = pct_acres2 - pct_acres1) %>% 
            st_as_sf() %>% 
            mutate(initial_use = factor(initial_use, levels = c("Crop","Forest","Urban","Other")))

top_code <- 0.02
ggplot(data = df %>% mutate(diff_acres = ifelse(abs(diff_acres) > top_code, sign(diff_acres)*top_code, diff_acres))) +
                  # mutate(pct_change = ifelse(pct_change > 0.015, 0.015,pct_change))) + 
        geom_sf(aes(fill = diff_acres),
                color = NA) +
        facet_wrap(~initial_use) + 
        scale_fill_continuous_diverging(h1 = 187, h2 = 339, c1 = 100, l1 = 25, l2 = 95, rev = TRUE) + 
        # scale_fill_viridis_c() +
        labs(fill = "Difference in change in\npercentage county land use\nacross scenarios",
             caption = sprintf("Note: Differences are top-coded at an absolute value of %s for visualization.",top_code)) +
        theme_void()

dst <- "results/model_results/simulations/sims_221130/"
dir.create(dst, recursive = TRUE, showWarnings = FALSE)
savefig(sprintf("%smap_land_use_change_crop_1.1",dst), width = 6.5*1.75, height = 4*1.75)
