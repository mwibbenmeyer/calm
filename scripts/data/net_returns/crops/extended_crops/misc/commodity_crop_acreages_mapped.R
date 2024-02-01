
pacman::p_load(tidyverse,
               readxl,
               tidycensus,
               sf,
               usmap)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # sets directory to the current directory
setwd('../../../../..')




df_commodity <- read.csv("processing/net_returns/crops/extended_crops/acreage_table.csv") %>% 
        filter(year == 2007, !state_fips_code %in% c("98","15","2")) %>% 
        mutate(fips = str_pad(state_fips_code, width = 2, side = "left", pad = "0"))

df_commodity <- df_commodity[!duplicated(df[ , c("short_desc", "year", "fips")]), ] 
df_commodity = df_commodity %>%
  group_by(commodity_desc, year, fips) %>%
  top_n(n = 1, wt = Value) %>% distinct(Value, .keep_all = TRUE)


df_specialty <- read.csv("processing/net_returns/crops/extended_crops/specialty_nr_acres_merged.csv") %>% 
        filter(year == 2007, !state_fips %in% c("98","15","2"), !commodity_desc %in% c("CANOLA", "PEANUTS", "BEANS", "SUNFLOWER", "POTATOES",
                                                                                       "SUGARBEETS", "PEAS", "SWEET CORN", "MILLET", "TOBACCO")) %>% 
        mutate(fips = str_pad(state_fips, width = 2, side = "left", pad = "0")) %>% rename(state_fips_code = state_fips, Value = acres) %>% 
        select(commodity_desc, short_desc, state_name, state_fips_code, Value, year, fips)


df_ers = rbind(df_commodity, df_specialty) %>% mutate(commodity = as.numeric(commodity_desc %in% 
                                                                                            c(# ERS crops
                                                                                                    "SOYBEANS","CORN","WHEAT","COTTON",
                                                                                                    "SORGHUM","RICE","BARLEY","OATS",
                                                                                                    "PEANUTS","SUGARBEETS","TOBACCO"
                                                                                            )))
df_collapse_ers <- df_ers %>%         
        group_by(fips,commodity) %>% 
        summarize(acres_commodity = sum(Value, na.rm = TRUE)) %>% 
        group_by(fips) %>% 
        mutate(total_acres = sum(acres_commodity, na.rm = TRUE),
               pct_commodity = acres_commodity/total_acres) %>% 
        filter(commodity == 1)


ers = plot_usmap(data = df_collapse_ers, values = "pct_commodity") + 
        scale_fill_viridis_c(name = "Crop coverage ratio", label = scales::comma, limits = c(0.1, 1)) + 
        theme(legend.position = "right") + labs(title = 'ERS crops baseline')


df_hay = rbind(df_commodity, df_specialty) %>% mutate(commodity = as.numeric(commodity_desc %in% 
                                                                                      c(# ERS crops
                                                                                        "SOYBEANS","CORN","WHEAT","COTTON",
                                                                                        "SORGHUM","RICE","BARLEY","OATS",
                                                                                        "PEANUTS","SUGARBEETS","TOBACCO",
                                                                                        
                                                                                        "HAY", "HAYLAGE"
                                                                                      )))
df_collapse_h <- df_hay %>%         
  group_by(fips,commodity) %>% 
  summarize(acres_commodity = sum(Value, na.rm = TRUE)) %>% 
  group_by(fips) %>% 
  mutate(total_acres = sum(acres_commodity, na.rm = TRUE),
         pct_commodity = acres_commodity/total_acres) %>% 
  filter(commodity == 1)


hay = plot_usmap(data = df_collapse_h, values = "pct_commodity") + 
  scale_fill_viridis_c(name = "Crop coverage ratio", label = scales::comma, limits = c(0.1, 1)) + 
  theme(legend.position = "right") + labs(title = 'ERS crops, hay, haylage')


df_potato_hay = rbind(df_commodity, df_specialty) %>% mutate(commodity = as.numeric(commodity_desc %in% 
                                                                                        c(# ERS crops
                                                                                                "SOYBEANS","CORN","WHEAT","COTTON",
                                                                                                "SORGHUM","RICE","BARLEY","OATS",
                                                                                                "PEANUTS","SUGARBEETS","TOBACCO",
                                                                                                
                                                                                                # northeast, broadly west
                                                                                                "HAY", "HAYLAGE", "POTATOES"
                                                                                        )))
df_collapse_ph <- df_potato_hay %>%         
        group_by(fips,commodity) %>% 
        summarize(acres_commodity = sum(Value, na.rm = TRUE)) %>% 
        group_by(fips) %>% 
        mutate(total_acres = sum(acres_commodity, na.rm = TRUE),
               pct_commodity = acres_commodity/total_acres) %>% 
        filter(commodity == 1)


pha = plot_usmap(data = df_collapse_ph, values = "pct_commodity") + 
        scale_fill_viridis_c(name = "Crop coverage ratio", label = scales::comma, limits = c(0.1, 1)) + 
        theme(legend.position = "right") + labs(title = 'ERS crops, hay, haylage, potatoes')



df_phag = rbind(df_commodity, df_specialty) %>% mutate(commodity = as.numeric(commodity_desc %in% 
                                                                                        c(# ERS crops
                                                                                                "SOYBEANS","CORN","WHEAT","COTTON",
                                                                                                "SORGHUM","RICE","BARLEY","OATS",
                                                                                                "PEANUTS","SUGARBEETS","TOBACCO",
                                                                                                
                                                                                                # northeast, broadly west
                                                                                                "HAY", "HAYLAGE", "POTATOES",
                                                                                                
                                                                                                # california
                                                                                                "ALMONDS", "GRAPES"
                                                                                        )))

df_collapse_phag <- df_phag %>%         
        group_by(fips,commodity) %>% 
        summarize(acres_commodity = sum(Value, na.rm = TRUE)) %>% 
        group_by(fips) %>% 
        mutate(total_acres = sum(acres_commodity, na.rm = TRUE),
               pct_commodity = acres_commodity/total_acres) %>% 
        filter(commodity == 1)



phag = plot_usmap(data = df_collapse_phag, values = "pct_commodity") + 
        scale_fill_viridis_c(name = "Crop coverage ratio", label = scales::comma, limits = c(0.1, 1)) + 
        theme(legend.position = "right") + labs(title = 'ERS crops, hay, haylage, potatoes,\n almonds, grapes')



df_phagos = rbind(df_commodity, df_specialty) %>% mutate(commodity = as.numeric(commodity_desc %in% 
                                                                                        c(# ERS crops
                                                                                          "SOYBEANS","CORN","WHEAT","COTTON",
                                                                                          "SORGHUM","RICE","BARLEY","OATS",
                                                                                          "PEANUTS","SUGARBEETS","TOBACCO",
                                                                                          
                                                                                          # northeast, broadly west
                                                                                          "HAY", "HAYLAGE", "POTATOES",
                                                                                          
                                                                                          # california
                                                                                          "ALMONDS", "GRAPES",
                                                                                          
                                                                                          # florida
                                                                                          "ORANGES", "SUGARCANE"
                                                                                          )))

df_collapse_phagos <- df_phagos %>%         
        group_by(fips,commodity) %>% 
        summarize(acres_commodity = sum(Value, na.rm = TRUE)) %>% 
        group_by(fips) %>% 
        mutate(total_acres = sum(acres_commodity, na.rm = TRUE),
               pct_commodity = acres_commodity/total_acres) %>% 
        filter(commodity == 1)



phagos = plot_usmap(data = df_collapse_phagos, values = "pct_commodity") + 
        scale_fill_viridis_c(name = "Crop coverage ratio", label = scales::comma, limits = c(0.1, 1)) + 
        theme(legend.position = "right") + labs(title = 'ERS crops, hay, haylage, potatoes,\n almonds, grapes, oranges, sugarcane')


df_phagosl = rbind(df_commodity, df_specialty) %>% mutate(commodity = as.numeric(commodity_desc %in% 
                                                                                        c(# ERS crops
                                                                                                "SOYBEANS","CORN","WHEAT","COTTON",
                                                                                                "SORGHUM","RICE","BARLEY","OATS",
                                                                                                "PEANUTS","SUGARBEETS","TOBACCO",
                                                                                                
                                                                                                # northeast, broadly west
                                                                                                "HAY", "HAYLAGE", "POTATOES",
                                                                                                
                                                                                                # california
                                                                                                "ALMONDS", "GRAPES",
                                                                                                
                                                                                                # florida
                                                                                                "ORANGES", "SUGARCANE",
                                                                                                
                                                                                                # arizona
                                                                                                "LETTUCE"
                                                                                        )))

df_collapse_phagosl <- df_phagosl %>%         
        group_by(fips,commodity) %>% 
        summarize(acres_commodity = sum(Value, na.rm = TRUE)) %>% 
        group_by(fips) %>% 
        mutate(total_acres = sum(acres_commodity, na.rm = TRUE),
               pct_commodity = acres_commodity/total_acres) %>% 
        filter(commodity == 1)



phagosl = plot_usmap(data = df_collapse_phagosl, values = "pct_commodity") + 
        scale_fill_viridis_c(name = "Crop coverage ratio", label = scales::comma, limits = c(0.1, 1)) + 
        theme(legend.position = "right") + labs(title = 'ERS crops, hay, haylage, potatoes,\n almonds, grapes, oranges, sugarcane, lettuce')

library(ggpubr)

ggarrange(
        ers, hay, pha, phag, phagos, phagosl,
        common.legend = TRUE, legend = "bottom"
)

ggsave('cropspercentage.jpg', dpi = 800)