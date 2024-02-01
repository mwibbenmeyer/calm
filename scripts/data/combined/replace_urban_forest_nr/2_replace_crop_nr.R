library(haven)
library(tidyverse)
library(readr)
library(readxl)
library(stringr)
library(slider)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../../../../")

#calculate 5 year running average for crop returns by county
crop_nr = read_csv("processing/net_returns/crops/crop_returns_by_county.csv") %>% select(county_fips, year, new_crop_nr = crop_nr)

crop_nr = crop_nr %>% 
  as.data.frame() %>% group_by(county_fips) %>%
  arrange(year, .by_group = TRUE)

#5 year lagging average not including the year itself, ex. 2007 is average of 2002~2006
#2002 returns kept since it'd be NA otherwise
crop_nr = crop_nr %>% group_by(county_fips) %>% mutate(new_crop_nr = case_when(year != 2002 ~
  as.numeric(slide(new_crop_nr, mean, .before = 5, .after = -1)),  year == 2002 ~ new_crop_nr))

df = read_dta('processing/combined/ddc_data_urbancal_crprev_urbannrsub.dta') %>% 
  mutate(fips = str_pad(fips, 5, 'left', '0'))

df_final = merge(x = df, y = crop_nr, by.x = c('fips', 'year'),
                 by.y = c('county_fips', 'year'), all.x = TRUE)

df_final$crop_nr = df_final$new_crop_nr
df_final = df_final %>% select(-new_crop_nr)

df_final = df_final %>% mutate(cropXlcc0 = case_when(lcc0 == 1 ~ crop_nr,
                                                             lcc0 == 0 ~ 0),
                               cropXlcc1 = case_when(lcc1 == 1 ~ crop_nr,
                                                             lcc1 == 0 ~ 0),
                               cropXlcc2 = case_when(lcc2 == 1 ~ crop_nr,
                                                             lcc2 == 0 ~ 0),
                               cropXlcc3 = case_when(lcc3 == 1 ~ crop_nr,
                                                             lcc3 == 0 ~ 0),
                               cropXlcc4 = case_when(lcc4 == 1 ~ crop_nr,
                                                             lcc4 == 0 ~ 0))




write_dta(df_final, 'processing/combined/ddc_data_urbancal_crprev_urbannrsub.dta')




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

