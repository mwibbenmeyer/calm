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
               readxl)

# clear environment and set wd.
rm(list=ls(all=TRUE)) 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)); setwd("../../../")

ecoregions <- read_excel("raw_data/ecoregion/county_to_ecoregion.xls", 
                       sheet = "county to ecoregion")

mountain <- data.frame(M = c(1,2), 
                       mountain = c("Non-mountain","Mountain"))
region.name <- data.frame(div = c(21,
                                22,
                                23,
                                24,
                                25,
                                26,
                                31,
                                32,
                                33,
                                34,
                                41
                                ),
              ecoregion_name = c("Warm Continental",
                                 "Hot Continental",
                                 "Subtropical",
                                 "Marine",
                                 "Prairie",
                                 "Mediterranean",
                                 "Tropical/Subtropical Steppe",
                                 "Tropical/Subtropical Desert",
                                 "Temperate Steppe",
                                 "Temperate Desert",
                                 "Savanna"))

ecoregions <- ecoregions %>% 
                merge(mountain, by = "M", all.x = TRUE) %>% 
                mutate(div = as.numeric(substr(as.character(P),1,2))) %>% 
                merge(region.name, by = "div", all.x = TRUE)

dst = sprintf("processing/ecoregions/")
dir.create(file.path(dst), showWarnings = TRUE, recursive = TRUE)
write.csv(ecoregions, paste0(dst,"ecoregions_key.csv"))


