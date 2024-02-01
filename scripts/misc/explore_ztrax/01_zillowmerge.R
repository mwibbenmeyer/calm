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

## These lines set several options
options(scipen = 999) # Do not print scientific notation
options(stringsAsFactors = FALSE) ## Do not load strings as factors


layoutZAsmt <- read_excel("../Raw_Data/ztrax/historical/LayoutAsmtHistory.xlsx", sheet = 1)

col_namesMain <- layoutZAsmt[layoutZAsmt$TableName == 'utMain', 'FieldName']
col_namesBldg <- layoutZAsmt[layoutZAsmt$TableName == 'utBuilding', 'FieldName']
col_namesValue <- layoutZAsmt[layoutZAsmt$TableName == 'utValue', 'FieldName']
col_namesMain2 <- col_namesMain[c(1:5,22:30,34,39,40,69:71,78,79,82,83,84),]

#####################################################################
# Pull address, geographic, lot size, and tax data from main table

base <- fread(unz("ZAsmt/Main.txt",
              select= c(1:5,22:30,34,39,40,69:71,78,79,82,83,84),
              sep = '|',
              header = FALSE,
              stringsAsFactors = FALSE,             
              quote = "", 
)                                          

base <- setNames(base, col_namesMain2$FieldName)

base <- as.data.table(base)

base <- base[ , list(RowID, ImportParcelID, LoadID, 
                     FIPS, State, County, 
                     PropertyFullStreetAddress,
                     PropertyHouseNumber, PropertyHouseNumberExt, PropertyStreetPreDirectional, PropertyStreetName, PropertyStreetSuffix, PropertyStreetPostDirectional,
                     PropertyCity, PropertyState, PropertyZip,
                     PropertyBuildingNumber, PropertyAddressUnitDesignator, PropertyAddressUnitNumber,
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
                     YearBuilt, EffectiveYearBuilt, YearRemodeled,
                     NoOfStories, StoryTypeStndCode, TotalRooms, TotalBedrooms, 
                     FullBath, ThreeQuarterBath, HalfBath, QuarterBath,
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


#########################
####Start Subsetting#####
#########################

base1 <- base[1:500000,]
base2 <- base[500001:1000000,]
base3 <- base[1000001:1500000,]
base4 <- base[1500001:2000000,]
base5 <- base[2000001:2500000,]
base6 <- base[2500001:3000000,]
base7 <- base[3000001:5000000,]
base8 <- base[5000001:7000000,]
base9 <- base[7000001:9000000,]
base10 <- base[9000001:11000000]
base11<- base[11000001:13000000]

######################
####start Merging#####
######################

merge1 <- merge.data.table(base1, bldg, by= "RowID")
merge2 <- merge.data.table(base2, bldg, by= "RowID")
merge3 <- merge.data.table(base3, bldg, by= "RowID")
merge4 <- merge.data.table(base4, bldg, by= "RowID")
merge5 <- merge.data.table(base5, bldg, by= "RowID")
merge6 <- merge.data.table(base6, bldg, by= "RowID")
merge7 <- merge.data.table(base7, bldg, by= "RowID")
merge15 <- merge.data.table(base8, bldg, by= "RowID")
merge16 <- merge.data.table(base9, bldg, by= "RowID")
merge17 <- merge.data.table(base10, bldg, by= "RowID")
merge18 <- merge.data.table(base11, bldg, by= "RowID")

######################
####value Merging#####
######################

merge8 <- merge.data.table(merge1, vls, by= "RowID")
merge9 <- merge.data.table(merge2, vls, by= "RowID")
merge10 <- merge.data.table(merge3, vls, by= "RowID")
merge11 <- merge.data.table(merge4, vls, by= "RowID")
merge12 <- merge.data.table(merge5, vls, by= "RowID")
merge13 <- merge.data.table(merge6, vls, by= "RowID")
merge14 <- merge.data.table(merge7, vls, by= "RowID")
merge19 <- merge.data.table(merge15, vls, by= "RowID")
merge20 <- merge.data.table(merge16, vls, by= "RowID")
merge21 <- merge.data.table(merge17, vls, by= "RowID")
merge22 <- merge.data.table(merge18, vls, by= "RowID")

###################
###bind merges#####
###################

zillow <- rbind(merge8, merge9, merge10,  merge11, merge12, merge13 , merge14, merge19 , merge20, merge21, merge22)

rm(list = ls(pattern = "merge"))
rm(list = ls(pattern = "base"))

##save data table###
write.table(zillow, file = "L:/Project-Fire_Perimeter/Zillow_Molly/ProcessCurrent/merged06.csv", sep = "|", row.names = F)


