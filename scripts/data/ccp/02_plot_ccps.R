####################################################
# Sophie Pesek
# May 27, 2021
# Script to plot ccps on a U.S. map
####################################################

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
               maps)
theme_set(theme_bw())

# Set working directory to land-use
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../../../")

world <- ne_countries(scale = "medium", returnclass = "sf") # load world data
class(world)
counties <- st_as_sf(map("county", plot = FALSE, fill = TRUE)) # load county data

#Load CCP data
ccps <- read.csv("processing/ccp/ccps.csv") %>% # load CCP data 
  select(-c(X)) # remove column
ccps$fips[ccps$fips == "46113"] <- "46102" # add old FIPS code to Oglala Lakota County
ccps <- result2
# Create manual levels
#ccps$ccp_weights <- cut((ccps$weighted_ccp), breaks=c(-1, 0.0001, 0.0005, 0.001, 0.005, 0.01, 0.05, 0.1, 0.5, 1)) # cut the data into levels
#levels(ccps$ccp_weights) = c("0-0.0001", "0.0001-0.0005", "0.0005-0.001", "0.001-0.005", "0.005-0.01", "0.01-0.05", "0.05-0.1", "0.1-0.5", "0.5-1", exclud=NULL) # create new scale
ccps$ccp_weights <- cut((ccps$weighted_ccp), breaks=c(-1, 0.0000000001, 0.9999999999, 1)) # cut the data into levels
levels(ccps$ccp_weights) = c("0-0.0000000001", "0.0000000001-0.9999999999", "0.9999999999-1", exclude=NULL) # create new scale


# #Iterate and create graphs
years <- c(2002, 2007, 2012, 2015)
#years <- 2015
lcc_values <- c("0", "1_2", "3_4", "5_6", "7_8")
# lcc_values <- "3_4"
initial_uses <- c("Crop", "Forest", "Urban", "CRP", "Other")
# initial_uses <- "Crop"
final_uses <- c("Crop", "Forest", "Urban", "CRP", "Other")
# final_uses <- "Forest"
#
 rm(i, j, k, l)
 for(i in years){
   for(j in lcc_values){
     for(k in initial_uses){
       for(l in final_uses){

        # i = 2002
        # j = "3_4"
        # k = "Crop"
        # l = "Forest"
        # #Subset data by each combination
         ccps1 <- ccps[ccps$year %in% i,] # subset data by each year
         ccps1 <- ccps1[ccps1$lcc %in% j,] # subset data by lcc values
         ccps1 <- ccps1[ccps1$initial_use %in% k,] # subset data by initial use
         ccps1 <- ccps1[ccps1$final_use %in% l,] # subset data by final use
        #x = median(ccps1$weighted_ccp)
        #y = x - 3*sd(ccps1$weighted_ccp)
         
         plot_usmap(data = ccps1, values = "weighted_ccp", color = "#E1F0FC", size=0.2, regions = "counties", exclude = c("AK","HI")) +
           #scale_fill_manual(values=rev(c("0-0.0000000001" = "#440154FF", "0.0000000001-0.9999999999" = "#29AF7FFF", "0.9999999999-1" = "#FDE725FF"))) +
           scale_fill_viridis_c() +
           labs(title = sprintf("Conversions from %s to %s in LCC %s in %s", k, l, j, toString(i)), fill = "Rate of conversion (smoothed)")
         ggsave(sprintf("results/initial_descriptives/combined/maps_ccp_continuous/ccp_%s_%s_%s_%s.jpg", toString(i), j, k, l), height = 8, width = 12) # save map

         assign(paste0('plot_ccp_', i, "_", j, "_", k, "_", l),
                plot_usmap(data = ccps1, values = "weighted_ccp", color = "#E1F0FC", size=0.2, regions = "counties", exclude = c("AK","HI")) +
                  #scale_fill_manual(values=rev(c("0-0.0000000001" = "#440154FF", "0.0000000001-0.9999999999" = "#29AF7FFF", "0.9999999999-1" = "#FDE725FF"))) +
                  scale_fill_viridis_c() +
                  labs(title = sprintf("Conversions from %s to %s in LCC %s in %s", k, l, j, toString(i)), fill = "Rate of conversion (smoothed)"))
         paste0('plot_ccp_', i, "_", j, "_", k, "_", l) %>% 
         save(list=., file = sprintf("results/initial_descriptives/combined/maps_ccp_continuous/Rfiles/ccp_%s_%s_%s_%s.RData", toString(i), j, k, l))
       }
     }
   }
 }
