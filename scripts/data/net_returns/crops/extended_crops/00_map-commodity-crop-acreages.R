if (!require("pacman")) install.packages("pacman")
devtools::install_github("robin-a-young/RStata")

pacman::p_load(tidyverse,
               dplyr,
               readxl,
               sf,
               tidycensus,
               haven,
               stringr,
               data.table,
               RStata,
               SimDesign, #has quiet function
               cdlTools,
               properties)

#May need to modify this based on location of Stata executable on local machine
options("RStata.StataPath" = "\"C:\\Program Files\\Stata17\\StataMP-64\"")
options("RStata.StataVersion" = 17)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # sets directory to the current directory
setwd('../../../../') # relative paths to move directory to the root project directory


df <- read.csv("raw_data/net_returns/crops/newcrops/acreage_table.csv") %>% 
        filter(year == 2012) %>% 
        mutate(fips = str_pad(state_fips_code, width = 2, side = "left", pad = "0"),
               commodity = as.numeric(commodity_desc %in% c("SOYBEANS","CORN","WHEAT","COTTON","SORGHUM","RICE","BARLEY","OATS","PEANUTS","SUGARBEETS","TOBACCO")))
        
df_collapse <- df %>%         
        group_by(fips,commodity) %>% 
        summarize(acres_commodity = sum(Value, na.rm = TRUE)) %>% 
        group_by(fips) %>% 
        mutate(total_acres = sum(acres_commodity, na.rm = TRUE),
               pct_commodity = acres_commodity/total_acres) %>% 
        filter(commodity == 1)

states <- get_acs(state = states, geography = "state", year = 2010, variables = "B19013_001", geometry = TRUE) %>%
  select(-c("variable","estimate","moe")) %>%
  st_transform(aea) %>% 
  st_make_valid()        

df_sf <- merge(df_collapse, states, by.x = "fips", by.y = "GEOID") %>% 
        st_as_sf()

ggplot(data = df_sf) +
  # geom_sf(aes(fill = pct_commodity, color = pct_commodity)) +
  geom_sf(aes(fill = as.factor(as.numeric(pct_commodity > 0.75)))) +
  # scale_fill_viridis_c() + 
  # scale_color_viridis_c() +
  theme_void()
