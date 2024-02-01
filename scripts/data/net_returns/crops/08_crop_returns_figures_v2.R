##################################################
## Project: Land Use
## Author: Matt Wibbenmeyer
## Date: February 9, 2022
## Script purpose: Map crop returns
## Input data: crop_returns_by_county.csv
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
returns <- read_csv("processing/net_returns/crops/crop_returns_by_county.csv")
# returns$fips[returns$county_fips == "46113"] <- "46102" # add old FIPS code to Oglala Lakota County
# returns <- haven::read_dta("processing/combined/ddc_data_urbancal_crprev_urbannrsub.dta")


returns <- returns %>%
        filter(year <= 2015) %>% 
        mutate(year2 = round((year+1)/5)*5 + 2,
                year2 = ifelse(year == 2002, year, year2),
                year2 = ifelse(year2 > 2012, 2015, year2)) %>% 
        group_by(county_fips, year2) %>% 
        summarize(crop_nr = mean(crop_nr, na.rm = T)) %>% 
        rename(year = year2,
               fips = county_fips)

county_panel <- haven::read_dta("processing/pointpanel/countypanel_estimation_bal.dta")
county_panel <- county_panel %>% group_by(fips,year,initial_use) %>%
  summarize(initial_acres = sum(acresk)) %>% 
  filter(initial_use == "Crop")

# plot map of returns -----------

# returns <- returns %>% 
#   mutate(forest_nr.f = cut(forest_nr, breaks = c(min(returns$forest_nr, na.rm =TRUE), 6.1,11.9,17.6,24.8,33.3,52.5,182) ))

p1 <- plot_usmap(data = returns %>% filter(year == 2012), 
                 values = "crop_nr", 
                 color = NA, 
                 size=0, regions = "counties", exclude = c("AK","HI")) +
  scale_fill_viridis_c(label = scales::dollar_format()) +
  # scale_fill_gradientn(colours = c("#D2EBFF", "#88c4f4", "#2B4357"), label = scales::dollar_format(),na.value="grey90") +
  # scale_fill_viridis_c(label = scales::dollar_format()) +
  labs(fill = "Returns per\nacre (2012)") 
p1
ggsave("results/initial_descriptives/net_returns/crops/crop_returns_map_2012.jpg", scale=1.5) # save map



# histogram ----------

mean <- sum(returns[returns$year == 2012, "crop_nr"], na.rm = TRUE)/dim(returns[returns$year == 2012, "crop_nr"])[1]

p2 <- ggplot(returns %>% filter(year == 2012, initial_use == 'Crop', final_use == 'Crop', lcc == '0'), aes(x=crop_nr)) + 
  geom_histogram(color="white", fill=viridis_pal()(12)[3], bins = 70) + 
  labs(x = "Returns per acre (2012)", y = "Count") +
  scale_x_continuous(labels=scales::dollar_format()) +
  geom_vline(xintercept = mean, color = "#2B4357", lwd = 0.75, linetype = "dashed" ) + 
  theme_minimal()
ggsave("results/initial_descriptives/net_returns/crops/crop_returns_hist_2012.jpg") # save map
p2


# Returns over time ----------

df <- merge(returns, 
            county_panel %>% filter(year == 2012) %>% 
              mutate(year = 2015) %>%
              rbind(.,county_panel), 
            by = c("fips", "year")) %>%
  group_by(year) %>%
  summarize(avg.returns = weighted.mean(crop_nr, initial_acres.x, na.rm = TRUE))

p4 <- ggplot(data=df, aes(x=year, y=avg.returns)) +
  geom_line(color=viridis_pal()(12)[3], lwd = 0.75) + 
  scale_x_continuous(breaks=c(2002,2007,2012), limits = c(2002,2015)) + 
  scale_y_continuous(labels=scales::dollar_format(), limits = c(0,max(df$avg.returns)))  + 
  labs(x = "Year", 
       y = "Average returns per acre") +
  theme_minimal()
ggsave("results/initial_descriptives/net_returns/crops/crop_returns_trend_2012.jpg") # save map
p4



# full figure ----------

layout <- "
AAAAAAABBB
AAAAAAABBB
AAAAAAACCC
AAAAAAACCC
"
p1 + p4 + p2 + 
  plot_layout(design = layout) + 
  # labs(caption = "Average returns per acre in the trend figure are weighted based
  # on the NRI-reported number of acres  in forest use in each year.") +
  theme(plot.caption = element_text(size = 11))
path = "results/initial_descriptives/net_returns/crops/"
dir.create(path, recursive = TRUE, showWarnings = FALSE)
ggsave(paste0(path,"crop_returns_combined.png"), width = 9*1.4, height = 5*1.4, dpi = 600)



