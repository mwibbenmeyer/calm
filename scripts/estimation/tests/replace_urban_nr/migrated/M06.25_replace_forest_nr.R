library(haven)
library(tidyverse)
library(censusapi)
library(readr)
library(readxl)
library(stringr)
library(urbnmapr)


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../../../../")

forest_nr = read_csv("raw_data/net_returns/forest/county_returns_forest.csv") %>% select(c(2, 12))
forest_nr = forest_nr %>% mutate(fips = str_pad(county, 5, 'left', '0')) %>% select(-county)

df = read_dta('processing/combined/ddc_data_urbancal_crprev_urbannrsub.dta') %>% 
  mutate(fips = str_pad(fips, 5, 'left', '0'))

df_final = merge(x = df, y = forest_nr, by.x = c('fips'),
                 by.y = c('fips'), all.x = TRUE)

df_final$forest_nr = df_final$rent
df_final = df_final %>% select(-rent)

df_final = df_final %>% mutate(forestXlcc0 = case_when(lcc0 == 1 ~ forest_nr,
                                                             lcc0 == 0 ~ 0),
                               forestXlcc1 = case_when(lcc1 == 1 ~ forest_nr,
                                                             lcc1 == 0 ~ 0),
                               forestXlcc2 = case_when(lcc2 == 1 ~ forest_nr,
                                                             lcc2 == 0 ~ 0),
                               forestXlcc3 = case_when(lcc3 == 1 ~ forest_nr,
                                                             lcc3 == 0 ~ 0),
                               forestXlcc4 = case_when(lcc4 == 1 ~ forest_nr,
                                                             lcc4 == 0 ~ 0))




write_dta(df_final, 'processing/combined/ddc_data_urbancal_crprev_urbannrsub_daveforestnr.dta')




# zero_counties = forest_nr %>% filter(rent == 0)
# zero_counties = merge(x = counties_sf,
#                      y = zero_counties, 
#                      by.x = 'county_fips', by.y = 'fips')
# 
# states_sf = get_urbn_map(map = "states", sf = TRUE)
# counties_sf = get_urbn_map(map = "counties", sf = TRUE)
# 
# mapping_data = merge(x = counties_sf,
#                      y = forest_nr, 
#                      by.x = 'county_fips', by.y = 'fips', all = TRUE)
# 
# mapping_data %>%
#   ggplot() +
#   geom_sf(mapping = aes(fill = log(rent+1)), color = NA) +
#   geom_sf(data = states_sf, fill = NA, color = "black", size = 0.25) +
#   coord_sf(datum = NA) +
#   scale_fill_gradient2(high = "darkgreen", na.value = "grey75")
# ggsave('results/replace_urban_forest_nr/newforestnr_log.png', dpi = 800, scale = 2)
# 
# mapping_data %>%
#   ggplot() +
#   geom_sf(mapping = aes(fill = rent), color = NA) +
#   geom_sf(data = states_sf, fill = NA, color = "black", size = 0.25) +
#   geom_sf(data = zero_counties, fill = NA, color = "darkblue", size = 0.25) +
#   coord_sf(datum = NA) +
#   scale_fill_gradient2(high = "darkgreen", na.value = "grey75")
# ggsave('results/replace_urban_forest_nr/newforestnr.png', dpi = 800, scale = 2)

