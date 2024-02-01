library(tidyverse)
library(haven)
library(urbnmapr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 
setwd('../../../../') 


df = read_dta('processing/combined/reg_diagnostics.dta')

df = df %>% select(fips) %>% distinct() %>% mutate(in_reg = TRUE)

states_sf = get_urbn_map(map = "states", sf = TRUE)
counties_sf = get_urbn_map(map = "counties", sf = TRUE)

county_data = merge(x = counties_sf, y = df, by.x = 'county_fips', by.y = 'fips', all = TRUE)


county_data %>%
  ggplot() +
  geom_sf(mapping = aes(fill = in_reg), color = NA) +
  geom_sf(data = states_sf, fill = NA, color = "black", size = 0.25) +
  coord_sf(datum = NA) 
ggsave('scripts/estimation/tests/replace_urban_nr/reg_map3.png', dpi = 800, scale = 2)


df_dta = read_dta('processing/combined/ddc_data_urbancal_crprev_urbannrsub.dta')

test = df_dta %>% distinct(fips, .keep_all = TRUE) %>% mutate(in_reg = TRUE)
county_data = merge(x = counties_sf, y = test, by.x = 'county_fips', by.y = 'fips', all = TRUE)
county_data %>%
  ggplot() +
  geom_sf(mapping = aes(fill = in_reg), color = NA) +
  geom_sf(data = states_sf, fill = NA, color = "black", size = 0.25) +
  coord_sf(datum = NA) 
ggsave('scripts/estimation/tests/replace_urban_nr/counties_map.png', dpi = 800, scale = 2)

