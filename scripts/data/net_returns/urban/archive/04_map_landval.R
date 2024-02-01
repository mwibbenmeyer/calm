04
# garbage collection
gc()

# load or install necessary libraries {
if (!require("pacman")) install.packages("pacman")
pacman::p_load(beepr,
               #cowplot,
               lfe,
               progress,
               tictoc,
               tidyverse,
               utils,
               rvest,
               tidycensus,
               readxl,
               tigris,
               sf,
               ggplot2,
               rvest,
               stringr,
               cdlTools,
               pbapply,
               readxl,
               scales)

# clear environment and set wd.
rm(list=ls(all=TRUE)) 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../../../")


rff_base <- rgb(4/255, 39/255, 60/255)
rff_midblue <- rgb(139/255, 196/255, 244/255)
rff_midblue_12.5 <- rgb(240/255, 247/255, 253/255)
rff_midblue_50 <- rgb(195/255,225/255, 249/255)
rff_red <- rgb(255/255, 102/255, 99/255)
rff_green <- rgb(80/255, 177/255, 97/255)
rff_brown <- rgb(116/255, 100/255, 94/255)


dir.create("results/initial_descriptives/landval", showWarnings = TRUE)

land_val_discrete_plot <- function(yr) {
  dat <- read_sf(sprintf("processing_output/net_returns/urban/landval/landval_%s.shp",yr)) 
  dat <- dat[, c("cntyfps", "landval", "geometry")]
  dat$cntyfps <- as.numeric(dat$cntyfps)
  
  datval <- dat
  datval$cut <- cut_number(datval$landval, n = 8, breaks = c(0, 2500, 5000, 10000,15000,
                                                             25000, 50000, Inf),
                           labels = c("0 - 2,500", "2,500 - 5,000", "5,000 - 10,000", "10,000 - 15,000", "15,000 - 25,000",
                                      "25,000 - 50,000", "50,000+"))
  
  landval_map <- ggplot() + 
    geom_sf(data = datval, aes(fill = cut)) + 
    scale_fill_brewer(palette = "YlGnBu") + 
    guides(fill = guide_legend(title = paste0(yr, ": Land Value ($ per acre)")))
  #scale_fill_brewer(palette = "YlGnBu")
  
  ggsave(paste0("results/initial_descriptives/landval/landval_", yr, ".pdf"), landval_map, dpi = 72)
}

land_val_continuous_plot <- function(yr) {
  dat <- read_sf(sprintf("processing_output/net_returns/urban/landval/landval_%s.shp",yr)) 
  dat <- dat[, c("cntyfps", "landval", "geometry")]
  dat$cntyfps <- as.numeric(dat$cntyfps)
  
  landval_map <- ggplot() + 
    geom_sf(data = dat, aes(fill = landval)) + 
    scale_fill_gradient(low = "yellow", high = "blue") + 
    guides(fill = guide_legend(title = paste0(yr, ": Land Value ($ per acre)")))
  
  ggsave(paste0("results/initial_descriptives/landval/landval_continuous_", yr, ".pdf"), landval_map, dpi = 72)
}

ggplot() + geom_sf(data = sf, aes(fill = landval)) + 
  scale_fill_gradient(low = "yellow", high = "blue")

lapply(c(2000, 2007, 2012, 2015), land_val_discrete_plot)
lapply(c(2000, 2007, 2012, 2015), land_val_continuous_plot)
