# garbage collection
gc()

# load or install necessary libraries {
if (!require("pacman")) install.packages("pacman")
pacman::p_load(beepr,
               #cowplot,
               lfe,
               progress,
               tictoc,
               tidyverse,
               utils,
               rvest,
               tidycensus,
               readxl,
               tigris,
               sf,
               ggplot2,
               rvest,
               stringr,
               cdlTools,
               pbapply,
               readxl,
               ggpattern,
               pals)

# install.packages("devtools")
devtools::install_github("jrnold/ggthemes")
# CRAN version
install.packages("ggthemes")


# clear environment and set wd.
rm(list=ls(all=TRUE)) 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)); setwd("../../../")
`%ni%` <- Negate(`%in%`)

ecoregions <- read_csv("processing/ecoregions/ecoregions_key.csv") %>% 
  mutate(GEOID = str_pad(Fips, width = 5, side = "left", pad = "0"))

aea <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

#Import county shapefile using tidycensus - b19013_001 is arbitrarily chosen
states <- state.abb[state.abb %ni% c("AK","HI")]
counties <- get_acs(stat e = states, geography = "county", year = 2010, variables = "B19013_001", geometry = TRUE) %>%
  select(-c("variable","estimate","moe")) %>%
  st_transform(aea) %>% 
  st_simplify(dTolerance = 2000, preserveTopology = TRUE) %>% 
  nngeo::st_remove_holes()

counties <- merge(counties, ecoregions, by = "GEOID")


ggplot(data = counties) +  
  # geom_sf_pattern(data = counties, aes(fill = ecoregion_name, pattern = as.factor(M)),
  #                                         color = "black",
  #                                         pattern_fill = "black",
  #                                         pattern_density = 0.1,
  #                 pattern_scale = 100) +
  # scale_pattern_manual(values = c(NA,"crosshatch"))
  geom_sf(data = counties, aes(fill = ecoregion_name)) +
# scale_pattern_manual(values = c(NA,"crosshatch"))
  labs(fill = "Province",
       pattern = "Mountain") +
  theme_void()
