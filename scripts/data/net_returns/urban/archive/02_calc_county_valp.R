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
               pbapply)

# clear environment and set wd.
rm(list=ls(all=TRUE)) 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../../../")

##########################################################
#Set up dictionaries and small functions needed for main function
{
  #From the PUMS data dictionary, the list of values corresponding to years built in past 5 years
  #https://www.census.gov/programs-surveys/acs/technical-documentation/pums/documentation.html
  
  ybl_dict = list("2000" = c("1", "2"),
                  "2007" = c("1","2"),
                  "2012"= c("12","13","14","15","16"),
                  "2015"= c("15","16","17","18","19"))
  puma_dict = list("2000" = "PUMA",
                   "2007" = "PUMA",
                   "2012"= "PUMA00",
                   "2015"= "PUMA10")
  keepvar_dict = list("2000" = c("STATEFIP","PUMA","VALUEH","BUILTYR"),
                      "2007" = c("ST","PUMA","VAL","YBL"),
                      "2012" = c("ST","PUMA00","PUMA10","VALP","YBL"),
                      "2015" = c("ST","PUMA00","PUMA10","VALP","YBL"))
  censusyear_dict = list("2000" = "2000",
                         "2007" = "2009", #Needed because tidycensus doesn't have ACS pre-2009
                         "2012"= "2012",
                         "2015"= "2015")
  recode_val <- function(df) {
    df <- df %>% mutate(VALP = case_when(
      VAL ==  1 ~ 5000,
      VAL == 2 ~ 12500,
      VAL == 3 ~ 17500,
      VAL == 4 ~ 22500,
      VAL == 5 ~ 27500,
      VAL == 6 ~ 32500,
      VAL == 7 ~ 37500,
      VAL == 8 ~ 45000,
      VAL == 9 ~ 55000,
      VAL == 10 ~ 65000,
      VAL == 11 ~ 75000,
      VAL == 12 ~ 85000,
      VAL == 13 ~ 95000,
      VAL == 14 ~ 112500,
      VAL == 15 ~ 137500,
      VAL == 16 ~ 167500,
      VAL == 17 ~ 187500,
      VAL == 18 ~ 225000,
      VAL == 19 ~ 275000,
      VAL == 20 ~ 350000,
      VAL == 21 ~ 450000,
      VAL == 22 ~ 625000,
      VAL == 23 ~ 875000,
      VAL == 24 ~ 1000000))
  }
}

##########################################################
#Function to get PUMS data and convert to county level
censuskey <- readLines("~/Documents/census_apikey.txt")[1]
get_county_valp <- function(st,year) {
  
  #PUMS data
  #view(pums_variables)
  puma_var = puma_dict[toString(year)][[1]]
  keepvars = keepvar_dict[toString(year)][[1]]
  lowerSt <- str_to_lower(st)
  yr <- substr(toString(year),3,4)
  
  {
    if(year != 2000) {
      pums <- read.csv(unz(
        sprintf("raw_data/net_returns/urban/pums/%s/csv_h%s.zip",year,lowerSt),
        sprintf("ss%sh%s.csv",yr,lowerSt)), header = TRUE, sep = ",") %>% 
        select(keepvars) %>%
        mutate(PUMA = eval(as.symbol(puma_var)))
      
    }
    else {
      pums <- read.csv(gzfile("raw_data/net_returns/urban/pums_5pct/2000/usa_00004.csv.gz")) %>%
        filter(STATEFIP == fips(st)) %>%
        mutate(VALUEH = na_if(VALUEH, 9999999)) %>%
        select(keepvars)
      
      
    }
  }
  
  #Recode property value variable for 2007 when variable is categorical
  if (year == 2007) {pums <- recode_val(pums)}
  
  ybl_vals = ybl_dict[toString(year)][[1]]   
  {
    if (year != 2000) {
      med_valp <- pums %>% 
        #Filter for homes built in the past five years
        filter(YBL %in% ybl_vals) %>%
        #Create PUMA ID variable 
        mutate(PUMA = str_pad(PUMA, 5, pad = "0", side = "left"),
               GEOID = paste0(sprintf("%02d",ST),PUMA)) %>% 
        filter(PUMA != "000-9") %>%
        group_by(GEOID) %>% summarize(med_valp = median(VALP, na.rm = TRUE))    
      
    }
    else{
      med_valp <- pums %>% 
        filter(BUILTYR %in% ybl_vals) %>%
        mutate(PUMA = str_pad(PUMA, 5, pad = "0", side = "left"), 
               GEOID = paste0(sprintf("%02d", STATEFIP), PUMA)) %>%
        filter(PUMA != "000-9") %>%
        group_by(GEOID) %>% summarize(med_valp = median(VALUEH, na.rm=TRUE))
    }
  }
  
  #Use tidycensus to get puma sf if after 2010, use locally stored 2000 pumas otherwise
  if (year > 2012) {
    puma_sf <- pumas(state = st,class = "sf") %>% mutate(GEOID = GEOID10)
  } else {
    stfips <- fips(st,to = "FIPS")
    puma_sf <- read_sf("raw_data/net_returns/urban/puma_2000/ipums_puma_2000/ipums_puma_2000.shp") %>% 
      filter(as.numeric(STATEFIP) == stfips) %>%
      mutate(GEOID = paste0(STATEFIP,PUMA))
  }
  
  puma_sf <- puma_sf %>% merge(med_valp, by = "GEOID")
  
  #Get county data
  censusyear = strtoi(censusyear_dict[toString(year)][[1]])
  lowerState <- str_to_lower(fips(st, to = "Name"))
  {
    if(censusyear != 2000) {
      counties <- get_acs(year = censusyear, geography = "county", variables = "B25075_001",
                          state = st, geometry = TRUE, key = censuskey) %>%
        select(GEOID,geometry)
      
      #Get tract data 
      #vars <- load_variables(2015, "acs5", cache = TRUE)
      tracts <- get_acs(year = censusyear, geography = "tract", variables = "B25075_001",
                        state = st, geometry = TRUE, key = censuskey) %>%
        mutate(properties = estimate,
               countyfips = substr(GEOID,1,5),
               trfips = GEOID) %>%
        st_make_valid()
      
    }
    else{
      src_crs <- get_acs(year = 2012, geography = "county", variables = "B25075_001",
                         state = st, geometry = TRUE, key = censuskey) %>%
        select(GEOID, geometry)
      
      cnty_gis <- read_sf("raw_data/nhgis0002_shape/nhgis0002_shapefile_tl2000_us_county_2000/US_county_2000.shp") %>%
        select(GISJOIN2, geometry) %>%
        rename(GEOID = GISJOIN2) %>%
        st_transform(st_crs(src_crs))
      
      rm(src_crs)
      
      counties <- read.csv("raw_data/nhgis0001_csv/nhgis0001_ds146_2000_county.csv") %>%
        filter(STATEA == fips(st)) %>%
        select(GISJOIN) %>%
        mutate(GISJOIN = gsub("G","", GISJOIN)) %>%
        rename(GEOID = GISJOIN) %>%
        merge(cnty_gis, by = "GEOID")
      
      trct_gis <- read_sf("raw_data/nhgis0002_shape/nhgis0002_shapefile_tl2000_us_tract_2000/US_tract_2000.shp") %>%
        select(GISJOIN2, geometry) %>%
        rename(GEOID = GISJOIN2) %>%
        st_transform(st_crs(cnty_gis))
      
      tracts <- read.csv("raw_data/nhgis0001_csv/nhgis0001_ds146_2000_tract.csv") %>%
        filter(STATEA == fips(st)) %>%
        rename(GEOID = GISJOIN) %>%
        rename(properties = FKI001) %>%
        mutate(GEOID = gsub("G", "", GEOID)) %>%
        mutate(countyfips = substr(GEOID, 1, 7),
               trfips = GEOID) %>%
        mutate(NAME = paste0(NAME,", ", COUNTY, " County, ", STATE)) %>%
        select(GEOID, NAME, properties, countyfips, trfips)
      tracts <- merge(trct_gis, tracts, by = "GEOID") %>%
        st_make_valid()
      
    }
  }
  
  
  #Intersect Census tracts with Pumas
  int <- st_intersection(tracts,st_make_valid(puma_sf %>% st_transform(st_crs(tracts))))
  int$area <- st_area(int)
  units(int$area) <- NULL
  
  #Calculate average value of newly developed properties in each county
  county_valp <- int %>% 
    #Calculate number of households/properties in each tract/puma intersection
    group_by(trfips) %>%
    mutate(tract_area = sum(area)) %>% ungroup() %>%
    mutate(pct_int_area = area/tract_area,
           int_props = properties*pct_int_area) %>%
    #Calculate number of households/properties in each county
    group_by(countyfips) %>%
    mutate(county_props = sum(int_props)) %>% ungroup() %>%
    #Calculate number of households/properties in each county/puma combination
    group_by(countyfips,GEOID) %>%
    summarize(county_puma_props = sum(int_props),
              county_props = mean(county_props),
              med_valp = mean(med_valp, na.rm=T)) %>%
    #Calculate share of county households in given puma
    mutate(pct_puma_county = county_puma_props/county_props,
           county_valp = pct_puma_county*med_valp) %>%
    #Use this to calculate a weighted average of property values in puma p across all counties
    group_by(countyfips) %>% 
    summarize(valp = sum(county_valp, na.rm=T))
  
  return(county_valp)
  
}

##########################################################
#Loop over states and years and save output
stlist <- lapply(seq(1,56),fips,"ABBREVIATION") %>% unlist()
stlist <- stlist[stlist %in% c("AK",NA,"DC","HI") == FALSE]

for (year in c(2000, 2007, 2012, 2015)) {
  
  df <- do.call(rbind, lapply(stlist,get_county_valp,year))
  
  dst = sprintf("processing_outputnet_returns/urban/county_valp/%s/",year)
  dir.create(file.path(dst), showWarnings = TRUE, recursive = TRUE)
  df_poly <- st_collection_extract(df, "POLYGON")
  
  st_write(df_poly, paste0(dst,sprintf("county_valp_%s.shp",year)), delete_dsn = TRUE)
  
}
