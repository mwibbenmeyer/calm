##################################################
## Project: Land Use
## Author: Matt Wibbenmeyer
## Date: September 14, 2021
## Script purpose: Map urban returns
## Input data: countylevel_urban_net_returns.csv
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

# load data
# returns <- read_csv("processing/net_returns/urban/countylevel_urban_net_returns.csv") %>%
#             mutate(fips = county_fips)
# returns$fips[returns$fips == "46113"] <- "46102" # add old FIPS code to Oglala Lakota County
returns <- haven::read_dta("processing/combined/ddc_data_urbancal_crprev_urbannrsub_daveforestnr.dta")

county_panel <- haven::read_dta("processing/pointpanel/countypanel_estimation_bal.dta")
county_panel <- county_panel %>% group_by(fips,year,initial_use) %>%
                  summarize(initial_acres = sum(acresk)) %>% 
                  filter(initial_use == "Urban")

# plot map of returns -----------

p1 <- plot_usmap(data = returns %>% filter(year == 2012), values = "urban_nr", 
                 color = NA, size=0, regions = "counties", exclude = c("AK","HI")) +
  # scale_fill_gradientn(colours = c("#D2EBFF", "#88c4f4", "#2B4357"), label = scales::dollar_format()) +
  scale_fill_viridis_c(label = scales::label_number()) +
  labs(fill = "Pop growth rate (2012)")
p1
ggsave("results/initial_descriptives/net_returns/urban/urban_returns_map_2012.jpg", scale=1.5) # save map

# histogram ----------

# plot histogram of returns without government payments in 2002

mean <- sum(returns[returns$year == 2012, "urban_nr"], na.rm = TRUE)/dim(returns[returns$year == 2012, "urban_nr"])[1]

p2 <- ggplot(returns %>% filter(year == 2012, initial_use == 'Crop', final_use == 'Crop', lcc == '0'), aes(x=urban_nr)) + geom_histogram(color="white", fill=viridis_pal()(12)[3], bins = 70) + 
  labs(x = "Pop growth rate (2012)", y = "Count") +
  scale_x_continuous(labels=scales::label_number()) +
  geom_vline(xintercept = mean, color = "#2B4357", lwd = 0.75, linetype = "dashed" ) + 
  theme_minimal()
p2
#xlim(c(0,50))
ggsave("results/initial_descriptives/net_returns/urban/urban_returns_hist_2012.jpg")

# Returns over time ----------

df <- merge(returns %>% mutate(year = ifelse(year == 2000,2002,year)), 
            county_panel %>% filter(year == 2012) %>% 
              mutate(year = 2015) %>%
              rbind(.,county_panel), 
            by = c("fips", "year")) %>%
            group_by(year) %>%
            summarize(avg.returns = weighted.mean(urban_nr, initial_acres.x, na.rm = TRUE))

p4 <- ggplot(df, aes(x=year, y=avg.returns)) +
  geom_line(color=viridis_pal()(12)[3], lwd = 0.75) + 
  scale_x_continuous(breaks=c(2002,2007,2012,2015)) + 
  scale_y_continuous(labels=scales::label_number(), limits = c(0,max(df$avg.returns)))  + 
  labs(x = "Year", 
       y = "Average pop growth rate") +
  theme_minimal()

p4
ggsave("results/initial_descriptives/net_returns/urban/urban_returns_trend.jpg")


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
path = "results/initial_descriptives/net_returns/urban/"
dir.create(path, recursive = TRUE, showWarnings = FALSE)
ggsave(paste0(path,"urban_returns_combined.png"), width = 9*1.4, height = 5*1.4, dpi = 600)


