

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse,
               data.table,
               haven,
               dplyr,
               readxl,
               sf,
               tidycensus,
               zoo,
               haven,
               urbnmapr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 
setwd('../../../..') 

states_sf = get_urbn_map(map = "states", sf = TRUE)
counties_sf = get_urbn_map(map = "counties", sf = TRUE)


df = read_dta('processing/combined/ddc_data_urbancal_crprev_urbannrsub.dta')
df = read_dta('processing/combined/ddc_data_urbancal_crprev_urbannrsub_oldforestnrsmoothed.dta')

df_rd = aggregate(list(forest_nr=df$forest_nr,urban_nr=df$urban_nr,crop_nr=df$crop_nr,other_nr=df$other_nr), by=list(fips=df$fips), FUN=mean)

mapping_data = merge(x = counties_sf, y = df_rd, by.x = 'county_fips', by.y = 'fips', all = TRUE)

df = df %>% mutate(interp_zero = case_when(interp_forest == TRUE & forest_nr == 0 ~ TRUE))

interpolated_counties = df %>% distinct(fips, .keep_all = TRUE) %>% 
  filter(interp_forest == TRUE) 
interpolated_counties_zero = df %>% distinct(fips, .keep_all = TRUE) %>% 
  filter(interp_zero == TRUE) 


interpolated_counties = merge(x = counties_sf, y = interpolated_counties, 
                              by.x = 'county_fips', by.y = 'fips', all.y = TRUE) %>% select(geometry)
interpolated_counties_zero = merge(x = counties_sf, y = interpolated_counties_zero, 
                              by.x = 'county_fips', by.y = 'fips', all.y = TRUE) %>% select(geometry)

mapping_data %>%
  ggplot() +
  geom_sf(mapping = aes(fill = forest_nr), color = NA) +
  geom_sf(data = states_sf, fill = NA, color = "black", size = 0.25) +
  geom_sf(data = interpolated_counties, fill = NA, color = "darkred", size = 0.25) +
  geom_sf(data = interpolated_counties_zero, fill = NA, color = "darkblue", size = 0.25) +
  coord_sf(datum = NA) +
  scale_fill_gradient2(high = "darkgreen", na.value = "gray45", name = "NR") +
  labs(title="Average county forest net returns across all years", 
       caption = "Gray: no data available, red outline: interpolated, blue outline: replaced with 0")
ggsave('scripts/estimation/tests/replace_urban_nr/forest_nr.png', dpi = 800, scale = 2)


mapping_data %>%
  ggplot() +
  geom_sf(mapping = aes(fill = urban_nr), color = NA) +
  geom_sf(data = states_sf, fill = NA, color = "black", size = 0.25) +
  coord_sf(datum = NA) +
  scale_fill_gradient2(high = "darkgreen", na.value = "gray45", name = "NR") +
  labs(title="Average county urban net return (pop growth rate) across all years", 
       caption = "Counties in gray have no data available")
ggsave('scripts/estimation/tests/replace_urban_nr/urban_nr.png', dpi = 800, scale = 2)


mapping_data %>%
  ggplot() +
  geom_sf(mapping = aes(fill = crop_nr), color = NA) +
  geom_sf(data = states_sf, fill = NA, color = "black", size = 0.25) +
  coord_sf(datum = NA) +
  scale_fill_gradient2(high = "darkgreen", na.value = "gray45", name = "NR") +
  labs(title="Average county crop net return across all years", 
       caption = "Counties in gray have no data available")
ggsave('scripts/estimation/tests/replace_urban_nr/crop_nr.png', dpi = 800, scale = 2)


mapping_data %>%
  ggplot() +
  geom_sf(mapping = aes(fill = other_nr), color = NA) +
  geom_sf(data = states_sf, fill = NA, color = "black", size = 0.25) +
  coord_sf(datum = NA) +
  scale_fill_gradient2(high = "darkgreen", na.value = "gray45", name = "NR") +
  labs(title="Average county other net return across all years", 
       caption = "Counties in gray have no data available")
ggsave('scripts/estimation/tests/replace_urban_nr/other_nr.png', dpi = 800, scale = 2)



mapping_data %>%
  ggplot() +
  geom_sf(mapping = aes(fill = log(forest_nr+1)), color = NA) +
  geom_sf(data = states_sf, fill = NA, color = "black", size = 0.25) +
  coord_sf(datum = NA) +
  scale_fill_gradient2(high = "darkgreen", na.value = "gray45", name = "log(NR+1)") +
  labs(title="Average county forest net returns across all years old forest nr") 
ggsave('results/replace_urban_forest_nr/old_forest_nr_log.png', dpi = 800, scale = 2)


crop2010 = new_gov_returns %>% filter(year == 2010)
df_rd = aggregate(list(crop_nr=new_gov_returns$crop_nr), by=list(fips=new_gov_returns$county_fips), FUN=mean)

mapping_data = merge(x = counties_sf, y = df_rd, by.x = 'county_fips', by.y = 'fips', all = TRUE)

mapping_data %>%
  ggplot() +
  geom_sf(mapping = aes(fill = crop_nr), color = NA) +
  geom_sf(data = states_sf, fill = NA, color = "black", size = 0.25) +
  coord_sf(datum = NA) +
  scale_fill_gradient2(high = "darkgreen", na.value = "gray45", name = "NR") +
  labs(title="county crop net returns across all years old crop nr") 
ggsave('results/replace_urban_forest_nr/old_crop_nr.png', dpi = 800, scale = 2)


forest_nr = read_csv("raw_data/net_returns/forest/county_forest_rent_s2.csv") %>% 
  mutate(fips = str_pad(county, 5, side = 'left', pad = '0')) %>%
  select(fips, s_rent)
forest_nr$fips[forest_nr$fips == '46113'] = '46102'
forest_nr$fips[forest_nr$fips == '12025'] = '12086'
forest_nr[nrow(forest_nr) + 1,] = list('01101', 281) 

counties_zero = forest_nr %>% filter(s_rent == 0)
counties_zero = merge(x = counties_sf, y = counties_zero, 
                      by.x = 'county_fips', by.y = 'fips', all.y = TRUE) %>% select(geometry)


mapping_data2 = merge(x = counties_sf, y = forest_nr, by.x = 'county_fips', by.y = 'fips', all = TRUE)

mapping_data2 %>%
  ggplot() +
  geom_sf(mapping = aes(fill = log(s_rent+1)), color = NA) +
  geom_sf(data = counties_zero, fill = NA, color = "darkblue", size = 0.25) +
  geom_sf(data = states_sf, fill = NA, color = "black", size = 0.25) +
  coord_sf(datum = NA) +
  scale_fill_gradient2(high = "darkgreen", na.value = "gray45", name = "log(NR+1)") +
  labs(title="new forest nr log scale") 
ggsave('results/replace_urban_forest_nr/11-3_forest_nr_log_s2.png', dpi = 800, scale = 2)


forest_nr_df = read_dta("processing/combined/ddc_data_urbancal_crprev_urbannrsub_daveforestnr.dta") 

interpolated_counties = forest_nr_df %>% distinct(fips, .keep_all = TRUE) %>% 
  filter(interp_forest == TRUE) 

forest_nr_df = aggregate(list(forest_nr=forest_nr_df$forest_nr), by=list(fips=forest_nr_df$fips), FUN=mean)


mapping_data = merge(x = counties_sf, y = forest_nr_df, by.x = 'county_fips', by.y = 'fips', all = TRUE)


interpolated_counties = merge(x = counties_sf, y = interpolated_counties, 
                              by.x = 'county_fips', by.y = 'fips', all.y = TRUE) %>% select(geometry)

mapping_data %>%
  ggplot() +
  geom_sf(mapping = aes(fill = log(forest_nr+1)), color = NA) +
  geom_sf(data = states_sf, fill = NA, color = "black", size = 0.25) +
  geom_sf(data = interpolated_counties, fill = NA, color = "red", size = 0.25) +
  coord_sf(datum = NA) +
  scale_fill_gradient2(high = "darkgreen", na.value = "gray45", name = "log(NR+1)") +
  labs(title="new forest nr log scale smoothed") 
ggsave('results/replace_urban_forest_nr/11-3_smoothed_forest_nr_log.png', dpi = 800, scale = 2)

mapping_data %>%
  ggplot() +
  geom_sf(mapping = aes(fill = forest_nr), color = NA) +
  geom_sf(data = states_sf, fill = NA, color = "black", size = 0.25) +
  geom_sf(data = interpolated_counties, fill = NA, color = "red", size = 0.25) +
  coord_sf(datum = NA) +
  scale_fill_gradient2(high = "darkgreen", na.value = "gray45", name = "NR") +
  labs(title="new forest nr smoothed") 
ggsave('results/replace_urban_forest_nr/11-3_smoothed_forest_nr.png', dpi = 800, scale = 2)
