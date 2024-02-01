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
               data.table)

# clear environment and set wd.
rm(list=ls(all=TRUE)) 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)); setwd("../../../")

ecoregions <- read_csv("processing/ecoregions/ecoregions_key.csv") %>% 
  mutate(fips = str_pad(Fips, width = 5, side = "left", pad = "0"))

land_use <- read_csv("processing/combined/full_combined_returns.csv") %>% as.data.table() 

land_use.missing <- land_use %>% 
            .[ , .(acresk = sum(final_acres)), by = c('fips','year','final_use')] %>%
            .[ , county_acresk := sum(acresk), by = c('fips','year')] %>% 
            .[year == 2012] %>% 
            .[ , lu.missing := as.numeric(acresk == 0)] %>% 
            merge(., ecoregions, by = 'fips') %>%  
            .[ , .(lu.missing = sum(lu.missing, na.rm = TRUE),
                   num_counties = .N), by = c("ecoregion_name","final_use")] %>% 
            .[ , Missing := 100*(lu.missing/num_counties)] %>% 
            .[ , Nonmissing := 100 - Missing] %>% 
            pivot_longer(cols = c("Missing","Nonmissing"),
                         names_to = "missing",
                         values_to = "percentage") %>% 
            as_tibble() %>% 
            mutate(land_use = ifelse(final_use != "CRP", str_to_title(final_use), final_use),
             ecoregion_name = str_replace(ecoregion_name,"/","/\n")) 

ggplot(data = land_use.missing, aes(x = "", fill = missing, y= percentage)) + 
  facet_grid(final_use ~ ecoregion_name,  switch = "y") +
  geom_bar(stat = "identity") + 
  coord_polar("y", start = 0) + 
  labs(fill = "", x = "") +
  theme_void() +
  theme(strip.text.x = element_text(angle = 45))


land_use.mtn.missing <- land_use %>% 
  .[ , .(acresk = sum(final_acres)), by = c('fips','year','final_use')] %>%
  .[ , county_acresk := sum(acresk), by = c('fips','year')] %>% 
  .[year == 2012] %>% 
  .[ , lu.missing := as.numeric(acresk == 0)] %>% 
  merge(., ecoregions, by = 'fips') %>%  
  .[ , .(lu.missing = sum(lu.missing, na.rm = TRUE),
         num_counties = .N), by = c("ecoregion_name","M","final_use")] %>% 
  .[ , Missing := 100*(lu.missing/num_counties)] %>% 
  .[ , Nonmissing := 100 - Missing] %>% 
  pivot_longer(cols = c("Missing","Nonmissing"),
               names_to = "missing",
               values_to = "percentage") %>% 
  as_tibble() %>% 
  mutate(land_use = ifelse(final_use != "CRP", str_to_title(final_use), final_use),
         ecoregion_name = str_replace(ecoregion_name,"/","/\n")) 

ggplot(data = land_use.mtn.missing %>% filter(M == 1), aes(x = "", fill = missing, y= percentage)) + 
  facet_grid(final_use ~ ecoregion_name,  switch = "y") +
  geom_bar(stat = "identity") + 
  coord_polar("y", start = 0) + 
  labs(fill = "", x = "",
       title = "Non-Mountains") +
  theme_void() +
  theme(strip.text.x = element_text(angle = 45))

ggplot(data = land_use.mtn.missing %>% filter(M == 2), aes(x = "", fill = missing, y= percentage)) + 
  facet_grid(final_use ~ ecoregion_name,  switch = "y") +
  geom_bar(stat = "identity") + 
  coord_polar("y", start = 0) + 
  labs(fill = "", x = "", title = "Mountains") +
  theme_void() +
  theme(strip.text.x = element_text(angle = 45))
