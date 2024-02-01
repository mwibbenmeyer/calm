# clear environment and set wd.
rm(list=ls(all=TRUE)) 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../../../")

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
               data.table)


##this function pulls the relevant zillow information and merges 
##properties from 2010 with WUI and WFHZ info

options(stringsAsFactors = FALSE) ## Do not load strings as factors

base <- read.table(unz("../raw_data/ztrax/current/01.zip", "Main.txt"), nrows = 1e3)
main <- read.table(unz("../raw_data/ztrax/current/01.zip","ZTrans/Main.txt"), nrows = 1e5)

layoutZAsmt <- read_excel("../Raw_Data/ztrax/historical/LayoutAsmtHistory.xlsx", sheet = 2)
col_namesMain <- layoutZAsmt[layoutZAsmt$TableName == 'utMain', 'FieldName']

keepcols = c(1:5,7,17:20,25:26,33:36)

base <- fread("../raw_data/ztrax/current/01/ZTrans/Main.txt",
              col.names = col_namesMain$FieldName[keepcols],
              select= keepcols,
              sep = '|',
              header = FALSE,
              stringsAsFactors = FALSE,             
              quote = "") %>% as.data.table()




base <- setNames(base, col_namesMain2$FieldName)

base <- as.data.table(base)

base <- base[ , list(RowID, ImportParcelID, 
                     FIPS, State, County, 
                     PropertyFullStreetAddress,
                     PropertyCity, PropertyState, PropertyZip,
                     PropertyAddressLatitude, PropertyAddressLongitude, PropertyAddressCensusTractAndBlock, 
                     NoOfBuildings,
                     LotSizeAcres, LotSizeSquareFeet,
                     TaxAmount, TaxYear)]

bldg <- read.table("Building.txt",
                   sep = '|',
                   header = FALSE,
                   stringsAsFactors = FALSE,             
                   skipNul = TRUE,                            
                   comment.char="",                          
                   quote = "",                                
) 

bldg <- setNames(bldg, col_namesBldg$FieldName)

bldg <- as.data.table(bldg)

bldg <- bldg[ , list(RowID, NoOfUnits, BuildingOrImprovementNumber, 
                     YearBuilt, TotalRooms,
                     HeatingTypeorSystemStndCode,
                     PropertyLandUseStndCode, Comments, OccupancyStatusStndCode)]

vls <- read.table("Value.txt",
                  sep = '|',
                  header = FALSE,
                  stringsAsFactors = FALSE,             
                  comment.char="",                          
                  quote = "",                                
) 

vls <- setNames(vls, col_namesValue$FieldName)

vls <- as.data.table(vls)

vls <- vls[vls$AssessmentYear==2010,]

merge<- merge.data.table(base, vls, by="RowID")

zillow<- merge.data.table(merge, bldg, by="RowID")

zillow <- zillow %>% drop_na("PropertyAddressLongitude", "PropertyAddressLatitude")

#translate zillow to sf object and clean
props4 <- st_as_sf(zillow, coords = c("PropertyAddressLongitude", "PropertyAddressLatitude"), 
                   crs = 4269, agr = "constant")

#reproject WUI so they share the projection
wui<- st_transform(wui, crs = 4269 , type = "EPSG")

###################################
#identify if properties are in WUI#
###################################

prop <- st_join(st_make_valid(props4), st_make_valid(wui), join=st_within)

###################################
##Merge Fire Hazard Severity Zone##
###################################

prop$WFHZ <- raster::extract(wfz,prop)

###Filtering###
propall<- merge(prop, landusecodes, by.x="PropertyLandUseStndCode", by.y="StndCode", all.x=TRUE)

propall$WUI <- ifelse(grepl("Interface",propall$WUI00CLASS,fixed = TRUE), "interface",
                       ifelse(grepl("Intermix",propall$WUI00CLASS,fixed = TRUE), "intermix",
                               ifelse(grepl("NoVeg",propall$WUI00CLASS,fixed = TRUE), "noveg",
                                       propall$WUI00CLASS)))
propall<- propall[grepl("Residential", propall$Classification, fixed=TRUE),]
