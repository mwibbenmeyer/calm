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
               ggplot,
               maps,
               rvest)



# clear environment and set wd.
rm(list=ls(all=TRUE)) 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../../../")

################################################################
#Download PUMS data by state
#This function will work for years 2007, 2012 and 2015

download_pums_data <- function(year) {
  url = ifelse(year > 2007,
               sprintf("https://www2.census.gov/programs-surveys/acs/data/pums/%s/5-Year/",year),
               sprintf("https://www2.census.gov/programs-surveys/acs/data/pums/%s/3-Year/",year))
  
  if(year == 2000) {url <- sprintf("https://www2.census.gov/programs-surveys/acs/data/pums/%s/",year)}
  html <- read_html(url)
  
  links <-html %>% 
    html_nodes("a") %>% 
    html_attr("href")
  housing_files <- links[grepl("csv_h",links)]
  
  housing_files <- housing_files[housing_files %in% 
                                   c("csv_hus.zip","csv_hpr.zip","csv_hak.zip","csv_hdc.zip","csv_hhi.zip") == FALSE]
  
  dst = sprintf("raw_data/net_returns/urban/pums/%s/",year)
  dir.create(file.path(dst), showWarnings = TRUE, recursive = TRUE)
  
  download_by_state <- function(filename,dst,url) {
    zip_url = paste0(url,filename)
    output = paste0(dst,filename)
    download.file(zip_url,output)
  }      
  
  lapply(housing_files,download_by_state,dst,url)
  
}

################################################################
#Loop over years 2000, 2007, 2012, 2015
lapply(c(2000, 2007,2012,2015), download_pums_data)
