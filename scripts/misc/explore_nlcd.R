# load/install packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse,
               # sf,
               # maps,
               # rnaturalearth,
               # rnaturalearthdata,
               # tidycensus,
               ggplot2,
               usmap,
               patchwork,
               raster,
               tidycensus)
theme_set(theme_bw())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # sets directory to the current directory
setwd('F:/Projects/land-use/') # relative paths to move directory to the root project directory

nlcd <- raster("raw_data/nlcd/nlcd_2016_land_cover_l48_20210604/nlcd_2016_land_cover_l48_20210604.img")

#load in state map
states <- get_acs(state=c("OR"), "county", 
                  variables = "B19013_001", geometry = TRUE) %>%
            st_transform(crs = crs(nlcd))

lane <- as_Spatial(states %>% filter(GEOID == "41039"))

lane_r <- crop(nlcd,lane)

plot(nlcd)

nlcd$nlcd_2016_land_cover_l48_20210604$nlcd_2016_land_cover_l48_20210604$nlcd_2016_land_cover_l48_202106