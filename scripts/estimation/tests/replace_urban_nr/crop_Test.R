pacman::p_load(tidyverse,
               urbnmapr,
               dplyr,
               readxl,
               sf)


states_sf = get_urbn_map(map = "states", sf = TRUE)
counties_sf = get_urbn_map(map = "counties", sf = TRUE)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 
setwd('../../../..') 

new_crop_returns <- read_csv("processing/net_returns/crops/new_crop_returns.csv") %>% # load crop returns data
  # select(., -c(X1)) %>% # remove added columns
  filter(., year <= 2017)

test = new_crop_returns %>% filter(state_fips == '12')
test1 = test %>% group_by(county_fips, year) %>% summarise(total_acres = sum(acres, na.rm=1))
test2 = test %>% group_by(county_fips, year, crop) %>% left_join(test1, by=c('county_fips', 'year'))
test2 = test2 %>% select(c(1, 3, 5:10, 13)) %>% mutate(pct_of_acres = acres / total_acres)
test2 = test2 %>% drop_na(pct_of_acres)


df_rd = aggregate(list(crop_nr=new_gov_returns$crop_nr), by=list(county_fips=new_gov_returns$county_fips), FUN=mean)

# df_rd = new_gov_returns %>% filter(year == 2012)

mapping_data = merge(x = counties_sf, y = df_rd, by.x = 'county_fips', by.y = 'county_fips', all = TRUE)

mapping_data %>%
  ggplot() +
  geom_sf(mapping = aes(fill = crop_nr), color = NA) +
  geom_sf(data = states_sf, fill = NA, color = "black", size = 0.25) +
  coord_sf(datum = NA) +
  scale_fill_gradient2(high = "darkgreen", na.value = "gray45", name = "NR")

# yields_fl = new_crop_returns %>% filter(state_fips=='12')
# 
# yields_cotton = yields %>% filter(crop=='upland cotton')
# df_cotton = aggregate(list(yield=yields_cotton$yield), by=list(fips=yields_cotton$county_fips), FUN=mean)
# yields_aaa = yields %>% filter(!is.na(yield))
# df_yields = aggregate(list(yield=yields_aaa$yield), by=list(fips=yields_aaa$county_fips), FUN=sum)

df_yields = new_crop_returns %>% filter(crop=='upland cotton')
# df_rd = aggregate(list(weight=df_yields$weight), by=list(county_fips=df_yields$county_fips), FUN=mean)
df_yields_fl = new_crop_returns %>% filter(crop=='upland cotton', state_fips =='12')


yields_merge = merge(x = df_yields, y = df_cotton, by = 'fips') %>% mutate(pcnt = yield.y / yield.x)

df_yields_2012 = new_gov_returns %>% filter(year == '2012')
mapping_data = merge(x = counties_sf, y = df_yields_2012, by.x = 'county_fips', by.y = 'county_fips', all = TRUE)

mapping_data %>%
  ggplot() +
  geom_sf(mapping = aes(fill = crop_nr), color = NA) +
  geom_sf(data = states_sf, fill = NA, color = "black", size = 0.25) +
  coord_sf(datum = NA) +
  scale_fill_gradient2(high = "darkgreen", na.value = "gray45", name = "Weight")


df_yields_2015 = new_gov_returns %>% filter(year == '2015')
mapping_data = merge(x = counties_sf, y = df_yields_2015, by.x = 'county_fips', by.y = 'county_fips', all = TRUE)

mapping_data %>%
  ggplot() +
  geom_sf(mapping = aes(fill = crop_nr), color = NA) +
  geom_sf(data = states_sf, fill = NA, color = "black", size = 0.25) +
  coord_sf(datum = NA) +
  scale_fill_gradient2(high = "darkgreen", na.value = "gray45", name = "Weight")

new_england = c('09', '23', '25', '33', '44', '50')

NE = new_crop_returns %>% filter(state_fips %in% new_england)

NE_2 = new_gov_returns %>% filter(substr(county_fips, 1,2) %in% new_england)

oats = new_crop_returns %>% filter(crop == 'oats') %>% drop_na(yield)
barley = new_crop_returns %>% filter(crop == 'barley') %>% drop_na(yield)

df_oats = aggregate(list(yield = oats$yield), by=list(county_fips=oats$county_fips), FUN=mean)

oats_data = merge(x = counties_sf, y = df_oats, by.x = 'county_fips', by.y = 'county_fips', all = TRUE)

oats_data %>%
  ggplot() +
  geom_sf(mapping = aes(fill = yield), color = NA) +
  geom_sf(data = states_sf, fill = NA, color = "black", size = 0.25, ) +
  coord_sf(datum = NA) +
  scale_fill_gradient2(high = "darkgreen", na.value = "gray50", name = "Oats Yield")


df_barley = aggregate(list(yield = barley$yield), by=list(county_fips=barley$county_fips), FUN=mean)

barley_data = merge(x = counties_sf, y = df_barley, by.x = 'county_fips', by.y = 'county_fips', all = TRUE)

barley_data %>%
  ggplot() +
  geom_sf(mapping = aes(fill = yield), color = NA) +
  geom_sf(data = states_sf, fill = NA, color = "black", size = 0.25, ) +
  coord_sf(datum = NA) +
  scale_fill_gradient2(high = "darkgreen", na.value = "gray50", name = "Barley Yield")


new_crop_returns %>%
  ggplot() +
  geom_point(aes(x=year, y=returns, group=crop, color=crop),size = 1)+
  facet_wrap(~crop)

new_crop_returns %>%
  ggplot() +
  geom_point(aes(x=year, y=cost, group=crop, color=crop),size = 1)+
  facet_wrap(~crop)

new_crop_returns %>%
  ggplot() +
  geom_point(aes(x=year, y=price, group=crop, color=crop),size = 1)+
  facet_wrap(~crop)

statefips = c('06', '17', '19', '18')
new_crop_returns %>% filter(state_fips %in% statefips, crop %in% c('soybeans', 'corn')) %>%
  ggplot() +
  geom_point(aes(x=year, y=returns, group=state_fips, color=state_fips),size = 1)+
  facet_wrap(~crop)
ggsave("results/initial_descriptives/net_returns/crops/crop1.jpg") # save map

new_crop_returns %>% filter(state_fips %in% statefips, crop %in% c('soybeans')) %>%
  ggplot() +
  geom_point(aes(x=year, y=price, group=state_fips, color=state_fips),size = 1)+
  facet_wrap(~crop)
ggsave("results/initial_descriptives/net_returns/crops/crop2.jpg") # save map

new_crop_returns %>% filter(state_fips %in% statefips) %>%
  ggplot() +
  geom_point(aes(x=year, y=cost, group=state_fips, color=state_fips),size = 1)+
  facet_wrap(~crop)
ggsave("results/initial_descriptives/net_returns/crops/crop3.jpg") # save map

