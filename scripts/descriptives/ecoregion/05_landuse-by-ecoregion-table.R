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
               data.table,
               gridExtra,
               grid)

# clear environment and set wd.
rm(list=ls(all=TRUE)) 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)); setwd("../../../")

ecoregions <- read_csv("processing/ecoregions/ecoregions_key.csv") %>% 
  mutate(fips = str_pad(Fips, width = 5, side = "left", pad = "0"))

land_use <- read_csv("processing/combined/full_combined_returns.csv") %>% as.data.table() 

table <- land_use %>% 
  .[ , .(acresk = sum(final_acres)), by = c('fips','year','final_use')] %>%
  .[ , county_acresk := sum(acresk), by = c('fips','year')] %>% 
  .[year == 2012] %>% 
  merge(., ecoregions, by = 'fips') %>%
  .[ , pct_landuse := acresk/county_acresk] %>% 
  .[ , .(meanpct_landuse = mean(pct_landuse, na.rm = TRUE)), by = c("ecoregion_name","final_use")] %>% 
  mutate(land_use = ifelse(final_use != "CRP", str_to_title(final_use), final_use)) %>% 
  pivot_wider(values_from = "meanpct_landuse", names_from = "final_use", id_cols = "ecoregion_name") %>% 
  mutate(across(where(is.numeric), function(x) formatC(x, digits = 3, width  = 3, format = "f")))

dev.off()
grid.table(table)


table_p <- land_use %>% 
  .[ , .(acresk = sum(final_acres)), by = c('fips','year','final_use')] %>%
  .[ , county_acresk := sum(acresk), by = c('fips','year')] %>% 
  .[year == 2012] %>% 
  merge(., ecoregions, by = 'fips') %>%
  .[ , pct_landuse := acresk/county_acresk] %>% 
  .[ , .(meanpct_landuse = mean(pct_landuse, na.rm = TRUE)), by = c("ecoregion_name","P","final_use")] %>% 
  mutate(land_use = ifelse(final_use != "CRP", str_to_title(final_use), final_use)) %>% 
  pivot_wider(values_from = "meanpct_landuse", names_from = "final_use", id_cols = "P") %>% 
  mutate(P = as.character(P)) %>% 
  mutate(across(where(is.numeric), function(x) formatC(x, digits = 3, width  = 3, format = "f"))) %>% 
  merge(ecoregions %>% 
          group_by(P) %>%
          summarize(ecoregion_name = first(ecoregion_name)) %>% 
          select("P","ecoregion_name"), 
        by = "P", all.x = TRUE) %>% 
  dplyr::select(c("P","ecoregion_name","Urban","Forest","CRP","Crop","Other"))

dev.off()
grid.table(table_p)
