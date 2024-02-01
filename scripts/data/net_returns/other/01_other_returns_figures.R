############################################################
## Project: Land Use
## Author: Matt Wibbenmeyer
## Date: October 26, 2021
## Script purpose: Map Other returns
## Input data: combined_returns_panel.dta
## Note: run 02_weight_other_returns.do before this script
############################################################

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
               haven)
theme_set(theme_bw())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # sets directory to the current directory
setwd('../../../..') # relative paths to move directory to the root project directory

# load data
# returns <- read_dta("processing/net_returns/combined_returns_panel_other.dta") %>%
#             mutate(fips = str_pad(as.character(fips),width = 5, pad = "0",side = "left"))
# returns$fips[returns$fips == "46113"] <- "46102" # add old FIPS code to Oglala Lakota County
returns <- haven::read_dta("processing/combined/ddc_data_urbancal_crprev_urbannrsub_daveforestnr.dta")

county_panel <- haven::read_dta("processing/pointpanel/countypanel_estimation_bal.dta")
county_panel <- county_panel %>%   
  mutate(initial_use = ifelse(initial_use == "Pasture" | initial_use == "Range","Other",initial_use)) %>%
  group_by(fips,year,initial_use) %>%
  summarize(initial_acres = sum(acresk)) %>% 
  filter(initial_use == "Other")

# plot map of returns -----------

p1 <- plot_usmap(data = returns %>% filter(year == 2012), values = "other_nr", 
                 color = NA, size=0, regions = "counties", exclude = c("AK","HI")) +
  # scale_fill_gradientn(colours = c("#D2EBFF", "#88c4f4", "#2B4357"), label = scales::dollar_format()) +
  scale_fill_viridis_c(label = scales::dollar_format()) +
  labs(fill = "Returns per acre (2012)")
p1
ggsave("results/initial_descriptives/net_returns/other/other_returns_map_2012.jpg", scale=1.5) # save map

# histogram ----------

# plot histogram of returns without government payments in 2002

mean <- sum(returns[returns$year == 2012, "other_nr"], na.rm = TRUE)/dim(returns[returns$year == 2012, "other_nr"])[1]

p2 <- ggplot(returns %>% filter(year == 2012, initial_use == 'Crop', final_use == 'Crop', lcc == '0'), aes(x=other_nr)) + 
  geom_histogram(color="white", fill=viridis_pal()(12)[3], bins = 70) + 
  labs(x = "Returns per acre (2012)", y = "Count") +
  scale_x_continuous(labels=scales::dollar_format()) +
  geom_vline(xintercept = mean, color = "#2B4357", lwd = 0.75, linetype = "dashed" ) + 
  theme_minimal()
p2
#xlim(c(0,50))
ggsave("results/initial_descriptives/net_returns/other/other_returns_hist_2012.jpg")

# Returns over time ----------

df <- merge(returns, 
            county_panel %>% filter(year == 2012) %>% 
              mutate(year = 2015) %>%
              rbind(.,county_panel), 
            by = c("fips", "year")) %>%
  group_by(year) %>%
  summarize(avg.returns = weighted.mean(other_nr, initial_acres.x, na.rm = TRUE))

p4 <- ggplot(df, aes(x=year, y=avg.returns)) +
  geom_line(color=viridis_pal()(12)[3], lwd = 0.75) + 
  scale_x_continuous(breaks=c(2002,2007,2012,2015), limits = c(2002,2015)) + 
  scale_y_continuous(labels=scales::dollar_format(), limits = c(0,max(df$avg.returns)))  + 
  labs(x = "Year", 
       y = "Average returns per acre") +
  theme_minimal()
p4
ggsave("results/initial_descriptives/net_returns/other/other_returns_trend_2012.jpg")


# full figure ----------

layout <- "
AAAAAAABBB
AAAAAAABBB
AAAAAAACCC
AAAAAAACCC
"
p1 + p4 + p2 + 
  plot_layout(design = layout) #+ 
  # labs(caption = "Average returns per acre in the trend figure are weighted based on the NRI-reported 
  #     number of acres in other (pasture or rangeland) use in each year.") + 
  # theme(plot.caption = element_text(size = 11))
path = "results/initial_descriptives/net_returns/other/"
dir.create(path, recursive = TRUE, showWarnings = FALSE)
ggsave(paste0(path,"other_returns_combined.png"), width = 9*1.4, height = 5*1.4, dpi = 600)

