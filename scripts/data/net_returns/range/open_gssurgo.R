# clear environment and set wd.
rm(list=ls(all=TRUE)) 
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
               rgdal,
               ggplot2,
               rvest,
               stringr,
               cdlTools,
               pbapply)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../../../")

fgdb <- "raw_data/net_returns/range/gssurgo/gSSURGO_CONUS.gdb"

# List all feature classes in a file geodatabase
subset(ogrDrivers(), grepl("GDB", name))
fc_list <- ogrListLayers(fgdb)

