
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse,
               ggplot2,
               readr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd('../../../..')


# ADJUST THE CWT UNITS
yield_table = read_csv("processing/net_returns/crops/extended_crops/yield_table.csv") %>% rename(yield = Value) %>% 
  filter(!grepl("SNAP", short_desc)) %>% 
  mutate(short_desc = gsub("-.*", "", short_desc)) %>% mutate(yield = case_when(commodity_desc == 'RICE' ~ yield/112, TRUE ~ yield)) %>%
  mutate(yield = case_when(commodity_desc == 'BEANS' ~ yield/112, TRUE ~ yield)) %>%
  mutate(yield = case_when(commodity_desc == 'SUNFLOWER' ~ yield/112, TRUE ~ yield)) %>%
  mutate(yield = case_when(commodity_desc == 'CANOLA' ~ yield/112, TRUE ~ yield)) %>%
  mutate(yield = case_when(commodity_desc == 'PEAS' ~ yield/112, TRUE ~ yield)) %>%
  mutate(yield = case_when(commodity_desc == 'LENTILS' ~ yield/112, TRUE ~ yield)) %>%
  mutate(short_desc = case_when(short_desc == 'ALMONDS, UTILIZED, SHELLED ' ~ 'ALMONDS ', TRUE ~ short_desc)) %>%
  mutate(short_desc = gsub(", FRESH MARKET", "", short_desc))

price_table = read_csv("processing/net_returns/crops/extended_crops/price_table.csv") %>% rename(price = Value) %>% filter(!grepl("SNAP", short_desc)) %>% 
  mutate(short_desc = gsub("-.*", "", short_desc)) %>% filter(!grepl("AUSTRIAN|GREEN|WRINKLED", short_desc)) %>% 
  filter(!grepl("CANNED|JUICE", short_desc)) %>% mutate(short_desc = gsub(", FRESH MARKET", "", short_desc))
merged = merge(yield_table, price_table, by = c('year', 'state_fips_code', 'commodity_desc', 'state_name', 'short_desc')) %>% 
  mutate(revenue = price * yield) 
merged = merged %>% filter(!grepl("ALFALFA", short_desc)) %>% filter(!grepl("COTTONSEED", short_desc)) %>% 
  filter(!grepl("AIR", short_desc)) %>% filter(!grepl("FIRE", short_desc)) %>% filter(!grepl("FLUE", short_desc)) %>% 
  filter(!grepl("CIGAR", short_desc)) %>% filter(yield != 0) %>% filter(!grepl("FALL|SUMMER", short_desc)) %>% filter(!grepl("TYPE", short_desc))
merged = merged %>% filter(short_desc != 'POTATOES, WINTER ')
merged = merged %>% filter(short_desc != 'POTATOES, SPRING ')
merged = merged %>% filter(short_desc != 'ORANGES, VALENCIA ')
merged = merged %>% filter(short_desc != 'ORANGES, MID & NAVEL ')

merged = merged %>% filter(commodity_desc %in% c('HAY', 'HAYLAGE', 'POTATOES', 'ALMONDS', 'GRAPES', 'ORANGES', 'SUGARCANE'))


# manually add in region-specific production costs
merged = merged %>% mutate(cost = 0)
merged = merged %>% mutate(cost = if_else(state_fips_code %in% c(9, 23, 25, 33, 36, 44, 50) & commodity_desc == 'POTATOES', 2293, cost))
merged = merged %>% mutate(cost = if_else(state_fips_code %in% c(16, 53, 38, 41, 6, 8, 32) & commodity_desc == 'POTATOES', 2343, cost))
merged = merged %>% mutate(cost = if_else(state_fips_code %in% c(6) & commodity_desc == 'ALMONDS', 2327, cost))
merged = merged %>% mutate(cost = if_else(state_fips_code %in% c(6) & commodity_desc == 'GRAPES', 4164, cost))
merged = merged %>% mutate(cost = if_else(state_fips_code %in% c(12) & commodity_desc == 'SUGARCANE', 731, cost))
merged = merged %>% mutate(cost = if_else(state_fips_code %in% c(12) & commodity_desc == 'ORANGES', 1815, cost))
merged = merged %>% mutate(cost = if_else(state_fips_code %in% c(4, 6) & commodity_desc == 'LETTUCE', 9837, cost))


# inflation adjustment
CPI = Quandl::Quandl("FRED/CPIAUCSL", collapse="annual", start_date="1995-01-01", end_date="2020-01-01")
CPI$year = as.numeric(format(CPI$Date,'%Y'))
CPI = mutate(CPI, factor = CPI[11,2]/Value) %>% select(year, factor)
merged = merge(merged, CPI, by='year')
merged = merged %>% mutate(inflation_adjusted_cost = cost / factor) %>% select(-factor)
merged = merged %>% mutate(nr = revenue - inflation_adjusted_cost)




acreages = read_csv("processing/net_returns/crops/extended_crops/combined_acreage.csv") %>% select(Year, State, County, State.ANSI, County.ANSI, Commodity, Data.Item, Value) %>%
  rename(year = Year, acres = Value, state_fips = State.ANSI, county_fips = County.ANSI, commodity_desc = Commodity) %>% 
  mutate(fips = str_c(str_pad(state_fips, 2, 'left', '0'), str_pad(county_fips, 3, 'left', '0')))

acreages = acreages[!duplicated(acreages[ , c("commodity_desc", "year", "fips")]), ] 
acreages = acreages %>%
  group_by(commodity_desc, year, fips) %>%
  top_n(n = 1, wt = acres) %>% distinct(acres, .keep_all = TRUE)


merged_acres = merge(x = acreages %>% select(year, state_fips, commodity_desc, acres, fips), y = merged %>%
                       rename(state_fips = state_fips_code), by = c("year", "state_fips", "commodity_desc"))
merged_acres = merged_acres %>%
  group_by(commodity_desc, year, fips) %>%
  top_n(n = 1, wt = acres) %>% distinct(acres, .keep_all = TRUE)

haylage = acreages %>% filter(commodity_desc == "HAYLAGE") %>% select(year, state_fips, commodity_desc, acres, fips)
merged_acres = bind_rows(merged_acres, haylage)

write.csv(merged_acres,"processing/net_returns/crops/extended_crops/specialty_nr_acres_merged.csv", row.names = FALSE)

