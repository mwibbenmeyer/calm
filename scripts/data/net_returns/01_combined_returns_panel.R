##################################################
## Project: Land Use
## Author: Matt Wibbenmeyer
## Date: August 31, 2023
## Script purpose: Merge returns for various land uses into a single panel data set.
##################################################

## Load/install packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse,
               data.table,
               haven,
               dplyr,
               readxl,
               sf,
               tidycensus,
               zoo)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) ; setwd('../../../../') # sets directory to the current directory


##################################################
# Import and clean up returns variables

# Crop returns ------------------------------------------------------------

#Import crop returns, revised to include specialty crops
crop_nr <- read.csv("processing/net_returns/crops/crop_returns_by_county.csv") %>% 
              mutate(fips = str_pad(county_fips, pad = "0",side = "left", width = 5)) %>% 
              select(-county_fips) %>% 
              mutate(year_rnd = round((year+1)/5)*5 + 2) %>% 
              filter(year < 2015) %>% 
              mutate(year_rnd = ifelse(year_rnd == 2017, 2015, year_rnd)) 

#Use 2002 returns for 2002 lag. Note: this is imperfect, but we have apparently not collected returns from pre-2002.
crop_nr <- rbind(crop_nr, crop_nr %>% 
                            filter(year == 2002) %>% 
                            mutate(year_rnd = 2002)) %>% 
              arrange(fips, year_rnd)

#Average by five-year interval
crop_nr <- crop_nr %>% 
            group_by(fips, year_rnd) %>% 
            summarize(crop_nr = mean(crop_nr, na.rm = T)) %>% 
            dplyr::rename(year = year_rnd)


# Forest returns ----------------------------------------------------------

#Import Dave's forest returns, smoothed within ecoregions. Note: these returns are not time-varying
forest_nr <- read.csv('processing/net_returns/forest/smoothed_forest_nr.csv') %>% 
              mutate(fips = str_pad(fips, pad = "0",side = "left", width = 5)) %>% 
              select(fips, forest_nr)


# Urban returns -----------------------------------------------------------

#Import urban net returns proxies: population density and population growth
urban_nr <- read.csv('processing/net_returns/urban/urban_nr_proxies.csv') %>% 
              mutate(fips = str_pad(GeoFIPS, pad = "0",side = "left", width = 5),
                     urban_nr = pop_growth) %>% 
              select(-GeoFIPS, - pop_density)

# CRP returns -------------------------------------------------------------

crp_nr <- read_dta("processing/net_returns/CRP/CRPmerged.dta") %>% 
              mutate(fips = str_pad(fips, pad = "0",side = "left", width = 5)) %>% 
              mutate(fips = ifelse(fips == "12025","12086",fips)) %>% 
              filter(year < 2015) %>% 
              mutate(year_rnd = round((year+1)/5)*5 + 2) %>% 
              mutate(year_rnd = ifelse(year_rnd == 2017, 2015, year_rnd)) %>% 
              group_by(fips,year_rnd) %>% 
              summarize(CRP_nr = mean(CRP_nr, na.rm = T)) %>% 
              dplyr::rename(year = year_rnd)

            
# Pasture/range returns ---------------------------------------------------------

pasture_nr <- read_dta("processing/net_returns/NASS/pasturerents.dta") %>% 
                select(-starts_with("asd_"), -starts_with("county"), -starts_with("state"), -starts_with("multistate")) %>% 
                mutate(fips = str_pad(fips, pad = "0",side = "left", width = 5)) %>% 
                mutate(fips = ifelse(fips == "12025","12086",fips)) %>% 
                filter(year < 2015) %>% 
                mutate(year_rnd = round((year+1)/5)*5 + 2) %>% 
                mutate(year_rnd = ifelse(year_rnd == 2017, 2015, year_rnd)) %>% 
                group_by(fips,year_rnd) %>% 
                summarize(pasture_nr = mean(pasture_nr, na.rm = T)) %>% 
                dplyr::rename(year = year_rnd)

##################################################
# Combine returns variables

# Combine returns ---------------------------------------------------------

combined <- merge(crop_nr, forest_nr, by = c("fips")) %>% 
              merge(urban_nr, by = c("fips","year")) %>% 
              merge(crp_nr, by = c("fips","year")) %>% 
              merge(pasture_nr, by = c("fips","year")) %>% 
              select(-X)

##################################################
# Weight other returns

nri <- read_dta("processing/pointpanel/pointpanel_estimation_unb.dta") %>% 
        filter(initial_use %ni% c("Federal","Water","Rural")) %>% 
        group_by(fips,year,initial_use) %>% 
        summarize(acresk = sum(acresk, na.rm = T)) %>% 
        filter(year != 1982) %>% 
        pivot_wider(id_cols = c(fips,year), names_from = initial_use, values_from = acresk) %>% 
        mutate(Range = Pasture) %>%
        mutate(Crop = ifelse(is.na(Crop), 0, Crop),
               Forest = ifelse(is.na(Forest), 0, Forest),
               Urban = ifelse(is.na(Urban), 0, Urban),
               Pasture = ifelse(is.na(Pasture), 0, Pasture),
               Range = ifelse(is.na(Range), 0, Range),
               CRP = ifelse(is.na(CRP), 0, CRP))
               
nri <- nri %>% 
        mutate(total_other = CRP + Pasture + Range,
               weightCRP = CRP/total_other,
               weightPasture = Pasture/total_other,
               weightRange = Range/total_other) %>% 
        select(fips, year, total_other, weightCRP, weightPasture, weightRange)

combined_long <- merge(combined, nri, by = c("fips","year")) %>% 
              pivot_longer(cols = c(starts_with("weight")),
                           names_to = c("use"),
                           values_to = "other_weight") %>% 
              mutate(use = recode(use, "weightCRP" = "CRP",
                                      "weightPasture" = "Pasture",
                                      "weightRange" = "Range")) %>% 
              select(-CRP_nr, -pasture_nr)

other_returns <- merge(combined, nri, by = c("fips","year")) %>% 
                  mutate(range_nr = pasture_nr) %>% 
                  pivot_longer(cols = c("CRP_nr","pasture_nr","range_nr"),
                               names_to = c("use"),
                               values_to = "other_nr") %>% 
                  mutate(use = recode(use, "CRP_nr" = "CRP",
                                      "pasture_nr" = "Pasture",
                                      "range_nr" = "Range")) %>% 
                  select(fips, year, use, other_nr)

combined_other <- merge(combined_long, other_returns, by = c("fips","year","use")) %>% 
                  group_by(fips,year) %>% 
                  summarize(other_nr = weighted.mean(other_nr, other_weight, na.rm = T)) 
                

combined_final <- merge(combined, combined_other, by = c("fips","year")) %>% 
                    select(-CRP_nr, -pasture_nr, -pop_growth)


write.csv(combined_final ,"processing/net_returns/combined_returns_panel.csv")

