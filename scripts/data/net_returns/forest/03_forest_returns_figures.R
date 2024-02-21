# load/install packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse,
               sf,
               maps,
               rnaturalearth,
               rnaturalearthdata,
               tidycensus,
               ggplot2,
               usmap,
               patchwork,
               scales)
theme_set(theme_bw())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # sets directory to the current directory
setwd('../../../../../') # relative paths to move directory to the root project directory


returns <- read.csv("processing/net_returns/forest/smoothed_forest_nr.csv")


# plot map of returns -----------

returns <- returns %>% 
   mutate(forest_nr.log = log(forest_nr+1),
          forest_nr.capped = pmin(forest_nr, 150))

p1 <- plot_usmap(data = returns %>% mutate(fips = ifelse(fips == "46113", "46102", fips)), 
                 values = "forest_nr.capped", 
                 color = NA, 
                 size=0, regions = "counties", exclude = c("AK","HI")) +
  scale_fill_viridis_c() +
  theme(plot.caption = element_text(hjust = 0)) +
  # scale_fill_gradientn(colours = c("#D2EBFF", "#88c4f4", "#2B4357"), label = scales::dollar_format(),na.value="grey90") +
  # scale_fill_viridis_c(label = scales::dollar_format()) +
  labs(fill = "Returns per\nacre, 2012", caption = 'Scale capped at $150 for visualization')
p1
ggsave("results/initial_descriptives/net_returns/forest/forest_returns_map_2012.jpg", scale=1.5) # save map

p1 + geom_sf(data = ecoregion_sf, fill = NA, color = "black", lwd = 1)

# plot map of returns with ecoregions -----------==

p1b <- ggplot(data = returns %>% filter(year == 2012 & 
                                          lcc == "1_2" & initial_use == "Crop" & final_use == "Crop")) + 
        geom_sf(aes(fill = forest_nr.log), color = NA) + 
        scale_fill_viridis_c() + 
        geom_sf(data = ecoregion_sf,
                fill = NA, color = "black", lwd = 0.5)
p1b

# histogram ----------

mean_return <- mean(returns$forest_nr, na.rm = T)

p2 <- ggplot(returns, aes(x=forest_nr)) + 
  geom_histogram(color="white", fill=viridis_pal()(12)[3], bins = 70) + 
  labs(x = "Returns per acre (2012)", y = "Count") +
  scale_x_continuous(labels=scales::dollar_format()) +
  geom_vline(xintercept = mean_return, color = "#2B4357", linewidth = 0.75, linetype = "dashed" ) + 
  theme_minimal() +
  theme(plot.margin = unit(c(1,1,1,1), "cm"))
ggsave("results/initial_descriptives/net_returns/forest/forest_returns_hist_2012.jpg") # save map
p2


# Returns over time ----------

p4 <- ggplot() + theme_void()



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
path = "results/initial_descriptives/net_returns/forest/"
dir.create(path, recursive = TRUE, showWarnings = FALSE)
ggsave(paste0(path,"forest_returns_combined.png"), width = 9*1.4, height = 5*1.4, dpi = 600)



