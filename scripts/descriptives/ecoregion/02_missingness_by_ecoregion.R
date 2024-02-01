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


returns <- read_csv("processing/combined/full_combined_returns.csv")

returns.missing <- returns %>% 
            mutate(across(.cols = ends_with("_nr"), function(x) as.numeric(is.na(x)))) %>% 
            group_by(fips) %>% 
            summarize(across(.cols = ends_with("_nr"), sum)) %>% 
            mutate(across(.cols = ends_with("_nr"), function(x) as.numeric(x > 0),
                          .names = "{substr(.col,1,str_length(.col)-3)}_missing")) %>% 
            dplyr::select(fips,ends_with("missing"))
            
ecoregions <- read_csv("processing/ecoregions/ecoregions_key.csv") %>% 
                mutate(fips = str_pad(Fips, width = 5, side = "left", pad = "0"))

## Crop returns coverage is 100% throughout all ecoregions
## CRP returns have high missingness in Mediterranean, Savanna, Temperate Desert, and Tropical/Subtropical desert.
## CRP returns has moderate missingness in Steppe regions, subtropical regions, prairie, marine, and hot continental regions.
## Forest returns have high missingness in Tropical/Subtropical Desert and Steppe, and moderate missingness in Mediterranean
## Temperate Desert and Steppe regions.
## Other (pasture/range) has low missingness overall, but missingness is highest in Savanna (a very small ecoregion)
## Urban returns have low missingness but some missing observations in Hot Contintental and Mediterranean regions

ecoregion.missing <- merge(returns.missing, ecoregions, by = "fips") %>% 
                      group_by(ecoregion_name,div) %>% 
                      summarize(across(ends_with("_missing"), sum),
                                num_counties = n()) %>% 
                      pivot_longer(cols = c(ends_with("_missing")), names_to = "land_use", values_to = 'Missing') %>%
                      mutate(land_use = substr(land_use,1,str_length(land_use)-8),
                             "Nonmissing" = num_counties - Missing) %>% 
                      pivot_longer(cols = c("Missing","Nonmissing"),
                                   names_to = "missing",
                                   values_to = "Count") %>% 
                      mutate(percentage = 100*(Count/num_counties),
                             land_use = ifelse(land_use != "CRP", str_to_title(land_use), land_use),
                             ecoregion_name = str_replace(ecoregion_name,"/","/\n")) %>% 
                      ungroup() 

ggplot(data = ecoregion.missing, aes(x = "", fill = missing, y= percentage)) + 
  facet_grid(land_use ~ ecoregion_name,  switch = "y") +
  geom_bar(stat = "identity") + 
  coord_polar("y", start = 0) + 
  labs(fill = "", x = "") +
  theme_void() +
  theme(strip.text.x = element_text(angle = 45))


## CRP returns are disproportionately missing in mountainous regions, while returns are disproportionately missing
## in mountainous regions.

mountain.missing <- merge(returns.missing, ecoregions, by = "fips") %>% 
  group_by(M) %>% 
  summarize(across(ends_with("_missing"), sum),
            num_counties = n()) %>% 
  pivot_longer(cols = c(ends_with("_missing")), names_to = "land_use", values_to = 'Missing') %>%
  mutate(land_use = substr(land_use,1,str_length(land_use)-8),
         "Nonmissing" = num_counties - Missing) %>% 
  pivot_longer(cols = c("Missing","Nonmissing"),
               names_to = "missing",
               values_to = "Count") %>% 
  mutate(percentage = 100*(Count/num_counties)) %>% 
  ungroup()

ggplot(data = mountain.missing, aes(x = "", fill = missing, y= percentage)) + 
  facet_grid(M ~ land_use) +
  geom_bar(stat = "identity") + 
  coord_polar("y", start = 0) + 
  labs(fill = "", x = "") +
  theme_void()



