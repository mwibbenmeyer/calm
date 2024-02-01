if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse,
               ggplot2,
               readr,
               Quandl)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd('../../../..')

pasture_rent = read_csv("raw_data/net_returns/pasture/pasture_rent.csv") %>% select(Year, State, `State ANSI`, County, `County ANSI`, `Data Item`, Value)

filtered_rent = pasture_rent %>% filter(Year == 2012 & `State ANSI` %in% c('06', '16', '39'))
filtered_rent = filtered_rent %>% mutate(fips = str_c(`State ANSI`, `County ANSI`))

rev = read_csv("processing/net_returns/crops/extended_crops/specialty_nr_acres_merged.csv") %>% filter(commodity_desc == 'HAY' & state_fips %in% c(6, 16, 39) & year == 2012)

rev = rev %>% mutate(cost = if_else(state_fips %in% c(39) & commodity_desc == 'HAY', 350.88, cost))
rev = rev %>% mutate(cost = if_else(state_fips %in% c(6) & commodity_desc == 'HAY', 537.58, cost))
rev = rev %>% mutate(cost = if_else(state_fips %in% c(16) & commodity_desc == 'HAY', 291.7, cost))

CPI = Quandl::Quandl("FRED/CPIAUCSL", collapse="annual", start_date="1995-01-01", end_date="2020-01-01")
CPI$year = as.numeric(format(CPI$Date,'%Y'))
CPI = mutate(CPI, factor = CPI[11,2]/Value) %>% select(year, factor)
rev = merge(rev, CPI, by='year')
rev = rev %>% mutate(inflation_adjusted_cost = cost / factor) %>% select(-factor)
rev = rev %>% mutate(nr = revenue - inflation_adjusted_cost)

hay_pasture_merged = merge(rev, filtered_rent %>% select(fips, Value), by='fips')
hay_pasture_merged = hay_pasture_merged %>% mutate(hay_nr_pasture_rent_ratio = nr/Value)

rev_full = read_csv("processing/net_returns/crops/extended_crops/specialty_nr_acres_merged.csv") %>% filter(year == 2012)

# taking average of the ratio in the state to use as the ratio for other states
mean_ratio = hay_pasture_merged %>%
  group_by(state_fips, state_name) %>%
  summarise_at(vars(hay_nr_pasture_rent_ratio), list(mean_ratio = mean))

rev_pasture_merged = merge(rev_full, pasture_rent %>% filter(Year == 2012) %>% 
                             mutate(fips = str_c(`State ANSI`, `County ANSI`)) %>%
                             select(fips, pasture_rent = Value), 
                           by = 'fips')

rev_CA_ratio = rev_pasture_merged %>% mutate(nr = if_else(commodity_desc == 'HAY', 
                                                          pasture_rent*mean_ratio$mean_ratio[mean_ratio$state_fips==6], nr))
write_csv(rev_CA_ratio, "processing/net_returns/crops/extended_crops/hay_rev_CA_ratio.csv")

rev_ID_ratio = rev_pasture_merged %>% mutate(nr = if_else(commodity_desc == 'HAY', 
                                                          pasture_rent*mean_ratio$mean_ratio[mean_ratio$state_fips==16], nr))
write_csv(rev_ID_ratio, "processing/net_returns/crops/extended_crops/hay_rev_ID_ratio.csv")

rev_OH_ratio = rev_pasture_merged %>% mutate(nr = if_else(commodity_desc == 'HAY', 
                                                          pasture_rent*mean_ratio$mean_ratio[mean_ratio$state_fips==39], nr))
write_csv(rev_OH_ratio, "processing/net_returns/crops/extended_crops/hay_rev_OH_ratio.csv")

rev_avg_ratio = rev_pasture_merged %>% mutate(nr = if_else(commodity_desc == 'HAY', 
                                                          pasture_rent*mean(mean_ratio$mean_ratio), nr))
write_csv(rev_avg_ratio, "processing/net_returns/crops/extended_crops/hay_rev_avg_ratio.csv")
