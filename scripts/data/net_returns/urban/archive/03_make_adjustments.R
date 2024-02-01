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
               readxl)

# clear environment and set wd.
rm(list=ls(all=TRUE)) 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../../../")

#Download Survey of Construction data
download_soc <- function(year) {
  
  dst = sprintf("raw_data/net_returns/urban/soc/%s/",year)
  dir.create(file.path(dst), showWarnings = FALSE, recursive = TRUE)
  
  yr = substr(toString(year),3,4)
  output = paste0(dst,sprintf("soc%s.zip",yr))
  download.file(sprintf("https://www.census.gov/construction/chars/xls/soc%s.zip",yr),output)
  
  unzip(sprintf("raw_data/net_returns/urban/soc/%s/soc%s.zip",year,yr),
        exdir = sprintf("raw_data/net_returns/urban/soc/%s/soc%s",year,yr))
  file.remove(sprintf("raw_data/net_returns/urban/soc/%s/soc%s.zip",year,yr))
  
}

lapply(c(2000, 2007,2012,2015), download_soc)


#Survey of construction data



#Read in CPI data
cpi <- read.csv("raw_data/CPI/cpi_2000-2015.csv")

landval_calc <- function(yr) {
  soc_path <- paste0("raw_data/net_returns/urban/soc/", yr, "/soc", 
                     substr(yr, 3, 4), "/soc", substr(yr, 3, 4), 
                     ".xls")
  soc <- read_excel(soc_path)
  
  pctlotv <- soc %>% filter(LOTV != 0) %>%
    mutate(pctlotv = LOTV/SLPR) %>%
    group_by(DIV) %>%
    summarize(pctlotv = median(pctlotv),
              slpr = median(SLPR, na.rm = TRUE),
              lotv = median(LOTV, na.rm = TRUE),
              lotsize_sqft = median(AREA, na.rm = TRUE),
              lotsize_acres = lotsize_sqft/43560)
  
  #Calculate CPI value to get 2010$
  index_2010 <- cpi$index[cpi$year == 2010]/cpi$index[cpi$year == yr]
  
  sf <- read_sf(sprintf("processing/net_returns/urban/county_valp/%s/county_valp_%s.shp",yr,yr)) %>%
    mutate(stfips = substr(countyfips,1,2),
           state = fips(stfips, to = "Abbreviation")) %>% 
    #Create Census division column for merge
    mutate(DIV = case_when(state %in% c("CT","MA","ME","NH","RI","VT") ~ 1,
                           state %in% c("NJ","NY","PA") ~ 2,
                           state %in% c("IL","IN","MI","OH","WI") ~ 3,
                           state %in% c("IA","KS","MN","MO","ND","NE","SD") ~ 4,
                           state %in% c("DE","FL","GA","MD","NC","SC","VA","WV") ~ 5,
                           state %in% c("AL","KY","MS","TN") ~ 6,
                           state %in% c("AR","LA","OK","TX") ~ 7,
                           state %in% c("AZ","CO","ID","MT","NM","NV","UT","WY") ~ 8,
                           state %in% c("CA","OR","WA") ~ 9)) %>%
    merge(pctlotv, by = "DIV") %>% 
    #Scale property value by percent that is attributed to land 
    mutate(landval = 0.05*index_2010*pctlotv*valp/lotsize_acres)
  
  out <- sprintf("processing/net_returns/urban/landval/landval_%s.shp", yr)
  st_write(sf, out, delete_dsn = TRUE)
}

lv_dir <- "processing/net_returns/urban/landval/"
dir.create(lv_dir)

lapply(c(2000, 2007, 2012, 2015), landval_calc)

combine_returns <- function(year) {
  dat <- read_sf(sprintf("processing/net_returns/urban/landval/landval_%s.shp",year)) %>%
    select(cntyfps, landval) %>%
    mutate(year = year) %>%
    rename(county_fips = cntyfps,
           net_returns = landval) %>%
    data.frame(.) %>% select(!geometry)
  
  return(dat)
}


net_returns_df <- bind_rows(lapply(c(2000,2007,2012,2015), combine_returns))

# quick and dirty way of fixing county fips issue for year 2000
for (i in which(nchar(net_returns_df$county_fips) == 7)) {
  net_returns_df$county_fips[i] <- paste0(substr(netreturns$county_fips[i], 1, 2),
                                      substr(netreturns$county_fips[i], 4, 6))
}

write_csv(net_returns_df, "processing/net_returns/urban/landval/countylevel_urban_net_returns.csv")

