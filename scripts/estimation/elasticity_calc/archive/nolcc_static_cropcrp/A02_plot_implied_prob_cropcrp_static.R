##############################################################
# Qinrui Xiahou
# Nov 4, 2021
# Script to plot ccps on a U.S. map
# Data input: implied_prob_cropcrp.dta from value fn iteration
##############################################################

## Load/install packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse,
               usmap,
               scales,
               ggplot2,
               foreign,
               haven,
               lwgeom,
               RColorBrewer,
               colorspace,
               sf,
               rnaturalearth,
               rnaturalearthdata,
               maps,
               grDevices,
               patchwork,
               imager)
theme_set(theme_bw())

# Set working directory to land-use
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../../../")

world <- ne_countries(scale = "medium", returnclass = "sf") # load world data
class(world)
counties <- st_as_sf(map("county", plot = FALSE, fill = TRUE)) # load county data

#Load implied probabilities data
imp_prob <- read_dta("processing/elasticity/implied_prob_cropcrp_static.dta")
imp_prob <- imp_prob %>%
  rename(Crop = p1, CRP = p2) %>%
  pivot_longer(c(Crop, CRP), names_to = "final_use", values_to = "imp_prob")


# Iterate and create graphs
#years <- c(2002, 2007, 2012, 2015)
years <- 2015
#lcc_values <- c("0", "1_2", "3_4", "5_6", "7_8")
# lcc_values <- "3_4"
initial_uses <- c("Crop", "CRP")
# initial_uses <- "Crop"
final_uses <- c("Crop", "CRP")
# final_uses <- "Forest"

for(i in years){
  # for(j in lcc_values){
  for(k in initial_uses){
    for(l in final_uses){
      
      # i = 2002
      # j = "3_4"
      # k = "Crop"
      # l = "Forest"
      # #Subset data by each combination
      imp_prob1 <- imp_prob[imp_prob$year %in% i,] # subset data by each year
      #imp_prob1 <- imp_prob1[imp_prob1$lcc %in% j,] # subset data by lcc values
      imp_prob1 <- imp_prob1[imp_prob1$initial_use %in% k,] # subset data by initial use
      imp_prob1 <- imp_prob1[imp_prob1$final_use %in% l,] # subset data by final use
      
      plot_usmap(data = imp_prob1, values = "imp_prob", color = "#E1F0FC", size=0.2, regions = "counties", exclude = c("AK","HI")) +
        scale_fill_viridis_c() +
        labs(title = sprintf("Conversions from %s to %s in %s (static)", k, l, toString(i)), fill = "Implied probability of conversion")
      ggsave(sprintf("results/initial_descriptives/implied_probabilities/static/nolcc_cropcrp/imp_prob_%s_%s_%s.jpg", toString(i), k, l), height = 8, width = 12) # save map
      
      assign(paste0('plot_imp_prob_static_', i, "_", k, "_", l),
             plot_usmap(data = imp_prob1, values = "imp_prob", color = "#E1F0FC", size=0.2, regions = "counties", exclude = c("AK","HI")) +
               scale_fill_viridis_c() +
               labs(title = sprintf("Conversions from %s to %s in %s (static)", k, l, toString(i)), fill = "Implied probability of conversion")) 
      paste0('plot_imp_prob_static_', i, "_", k, "_", l) %>% 
        save(list=., file = sprintf("results/initial_descriptives/implied_probabilities/static/nolcc_cropcrp/imp_prob_%s_%s_%s.RData", toString(i), k, l))
      
      }
#   }
  }
}





# Comparison with CCPs
for(k in initial_uses){
  for(l in final_uses){
    load(sprintf("results/initial_descriptives/implied_probabilities/static/nolcc_cropcrp/imp_prob_%s_%s_%s.RData", toString(2015), k, l))
    load(sprintf("results/initial_descriptives/implied_probabilities/dynamic/imp_prob_%s_%s_%s.RData", toString(2015), k, l))
    load(sprintf("results/initial_descriptives/combined/maps_ccp_continuous/Rfiles/ccp_%s_%s_%s.RData", "2015_1_2", k, l))
    load(sprintf("results/initial_descriptives/combined/maps_ccp_continuous/Rfiles/ccp_%s_%s_%s.RData", "2015_3_4", k, l))
  }
}

## between dynamic and static models
layout <- "
AAA
AAA
BBB
BBB
"

plot_imp_prob_2015_Crop_Crop + plot_imp_prob_static_2015_Crop_Crop + plot_layout(design = layout)
ggsave("results/initial_descriptives/implied_probabilities/static/nolcc_cropcrp/dyst_2015_Crop_Crop.jpg", width = 12, height = 16) 

plot_imp_prob_2015_Crop_CRP + plot_imp_prob_static_2015_Crop_CRP + plot_layout(design = layout)
ggsave("results/initial_descriptives/implied_probabilities/static/nolcc_cropcrp/dyst_2015_Crop_CRP.jpg", width = 12, height = 16) 

plot_imp_prob_2015_CRP_Crop + plot_imp_prob_static_2015_CRP_Crop + plot_layout(design = layout)
ggsave("results/initial_descriptives/implied_probabilities/static/nolcc_cropcrp/dyst_2015_CRP_Crop.jpg", width = 12, height = 16) 

plot_imp_prob_2015_CRP_CRP + plot_imp_prob_static_2015_CRP_CRP + plot_layout(design = layout)
ggsave("results/initial_descriptives/implied_probabilities/static/nolcc_cropcrp/dyst_2015_CRP_CRP.jpg", width = 12, height = 16) 


## between model and actual results
layout <- "
AAAAABBB
AAAAABBB
AAAAACCC
AAAAACCC
"

plot_imp_prob_static_2015_Crop_Crop + plot_ccp_2015_1_2_Crop_Crop + plot_ccp_2015_3_4_Crop_Crop + plot_layout(design = layout)
ggsave("results/initial_descriptives/implied_probabilities/static/nolcc_cropcrp/comp_2015_Crop_Crop_static.jpg", width = 18, height = 10) 

plot_imp_prob_static_2015_Crop_CRP + plot_ccp_2015_1_2_Crop_CRP + plot_ccp_2015_3_4_Crop_CRP + plot_layout(design = layout)
ggsave("results/initial_descriptives/implied_probabilities/static/nolcc_cropcrp/comp_2015_Crop_CRP_static.jpg", width = 18, height = 10) 

plot_imp_prob_static_2015_CRP_Crop + plot_ccp_2015_1_2_CRP_Crop + plot_ccp_2015_3_4_CRP_Crop + plot_layout(design = layout)
ggsave("results/initial_descriptives/implied_probabilities/static/nolcc_cropcrp/comp_2015_CRP_Crop_static.jpg", width = 18, height = 10) 

plot_imp_prob_static_2015_CRP_CRP + plot_ccp_2015_1_2_CRP_CRP + plot_ccp_2015_3_4_CRP_CRP + plot_layout(design = layout)
ggsave("results/initial_descriptives/implied_probabilities/static/nolcc_cropcrp/comp_2015_CRP_CRP_static.jpg", width = 18, height = 10) 

# p1 + p2 + p3 + plot_layout(design = layout)
# 
# library(cowplot)
# library(ggplot2)
# wrap_plots(list(plot, plot, plot), nrow = 2, width = 18, height = 10)





# Correlation with CCP
ccps <- read.csv("processing/ccp/ccps.csv") %>% # load CCP data 
  select(-c(X)) # remove column
ccps$ccp_weights <- cut((ccps$weighted_ccp), breaks=c(-1, 0.0000000001, 0.9999999999, 1)) # cut the data into levels

prob_plus_ccps <- imp_prob %>%
  mutate(fips = as.character(fips)) %>%
  left_join(ccps, by=c("fips", "year", "initial_use", "final_use"))

cor <- prob_plus_ccps %>%
  filter(is.na(lcc)==FALSE) %>%
  group_by(initial_use, final_use, lcc) %>%
  summarize(cor(weighted_ccp, imp_prob, use="complete.obs"))
