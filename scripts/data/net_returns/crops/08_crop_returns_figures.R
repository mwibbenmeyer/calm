##################################################
## Project: Land Use
## Author: Sophie Pesek
## Date: June 14, 2021
## Script purpose: Map weighted returns
## Input data: final_crop_returns.csv - crop dataset with returns
##################################################

# load/install packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse,
               # sf,
               # maps,
               # rnaturalearth,
               # rnaturalearthdata,
               # tidycensus,
               ggplot2,
               usmap,
               patchwork,
               scales)
theme_set(theme_bw())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # sets directory to the current directory
setwd('../../../..') # relative paths to move directory to the root project directory


returns <- read_csv("processing/net_returns/crops/crop_returns_by_county.csv")

returns <- returns %>% filter(year <= 2015) %>% 
                      mutate(year2 = round((year-1+2.5)*2,-1)/2 + 2,
                              year2 = ifelse(year2 == 2017,2015,year2),
                              year = year2) %>%
                      dplyr::select(-year2) %>% 
                      mutate(fips = county_fips) %>%
                      group_by(fips,year) %>%
                      summarize(crop_nr = mean(crop_nr))

returns$fips[returns$fips == "46113"] <- "46102" # add old FIPS code to Oglala Lakota County

county_panel <- haven::read_dta("processing_output/countypanel_estimation_bal.dta")
county_panel <- county_panel %>% group_by(fips,year,initial_use) %>%
  summarize(initial_acres = sum(acresk)) %>% 
  filter(initial_use == "Crop")

# plot map of returns -----------

p1 <- plot_usmap(data = returns %>% filter(year == 2012), values = "crop_nr", 
                 color = NA, size=0, regions = "counties", exclude = c("AK","HI")) +
  # scale_fill_gradientn(colours = c("#D2EBFF", "#88c4f4", "#2B4357"), label = scales::dollar_format()) +
  scale_fill_viridis_c(label = scales::dollar_format()) +
  labs(fill = "Returns per acre (2012)")
# ggsave("results/initial_descriptives/net_returns/crops/crop_returns_map_2012.jpg") # save map
p1

# histogram ----------

# plot histogram of returns without government payments in 2002

mean <- sum(returns[returns$year == 2012, "crop_nr"], na.rm = TRUE)/dim(returns[returns$year == 2012, "crop_nr"])[1]

p2 <- ggplot(returns %>% filter(year == 2012), aes(x=crop_nr)) + 
  geom_histogram(color="white", fill=viridis_pal()(12)[3], bins = 70) + 
  labs(x = "Returns per acre (2012)", y = "Count") +
  scale_x_continuous(labels=scales::dollar_format()) +
  geom_vline(xintercept = mean, color = "#2B4357", lwd = 0.75, linetype = "dashed" ) + 
  theme_minimal()
p2
#xlim(c(0,50))
# ggsave("results/initial_descriptives/net_returns/crops/new_gov_payments/urban_returns_hist_combined_99th.jpg")

# Returns over time ----------

df <- merge(returns, 
            county_panel %>% filter(year == 2012) %>% 
              mutate(year = 2015) %>%
              rbind(.,county_panel), 
            by = c("fips", "year")) %>%
  group_by(year) %>%
  summarize(avg.returns = weighted.mean(crop_nr, initial_acres, na.rm = TRUE))

p4 <- ggplot(df, aes(x=year, y=avg.returns)) +
  geom_line(color=viridis_pal()(12)[3], lwd = 0.75) + 
  scale_x_continuous(breaks=c(2002,2007,2012,2015)) + 
  scale_y_continuous(labels=scales::dollar_format(), limits = c(0,max(df$avg.returns)))  + 
  labs(x = "Year", 
       y = "Average returns per acre") +
  theme_minimal()

p4


# full figure ----------

layout <- "
AAAAAAABBB
AAAAAAABBB
AAAAAAACCC
AAAAAAACCC
"
p1 + p4 + p2 + 
  plot_layout(design = layout) 
#labs(caption = "Average returns per acre in the trend figure are weighted based on the NRI-reported number of acres 
# in urban use in each year. Returns for 2002 plotted in the trend figure are based on year 2000 data.")
path = "results/initial_descriptives/net_returns/crops/"
dir.create(path, recursive = TRUE, showWarnings = FALSE)
ggsave(paste0(path,"crop_returns_combined.png"), width = 9*1.4, height = 5*1.4, dpi = 600)

