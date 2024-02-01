##################################################
## Project: Land Use
## Author: Sophie Pesek
## Date: April 26, 2021
## Script purpose: Correct units of sorghum price and rice yield
## Input data: NASS price and yield data
##################################################

install.packages("tidyverse")
library(tidyverse)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # sets directory to the current directory
setwd('../../../..') # relative paths to move directory to the root project directory

# sorghum price ---------------------------------

sorghum_price <- read_csv("raw_data/net_returns/crops/price/sorghum_price_raw.csv") # load data

sorghum_price$Value <- as.integer(sorghum_price$Value)*0.56 # multiply $/cwt by 0.56 cwt/bu to get $/bu and replace column
sorghum_price$`Data Item` <- "SORGHUM, GRAIN - PRICE RECEIVED, MEASURED IN $ / BU" # change unit labels

dst = "processing/net_returns/crops/price/" # set destination
dir.create(dst,recursive = TRUE, showWarnings = FALSE) # create folders if they don't exist
write.csv(sorghum_price, sprintf("%s/sorghum_price.csv", dst)) # write csv

# rice yield ---------------------------------

rice_yield <- read_csv("raw_data/net_returns/crops/yield/rice_yield_raw.csv") # load data

rice_yield$Value <- as.integer(rice_yield$Value)*0.01 # multiply lb/acre by 0.01 cwt/lb to get cwt/acre and replace column
rice_yield$`Data Item` <- "RICE - YIELD, MEASURED IN CWT / ACRE" # change unit labels

dst = "processing/net_returns/crops/yield/" # set destination
dir.create(dst,recursive = TRUE, showWarnings = FALSE) # create folders if they don't exist
write.csv(rice_yield, sprintf("%s/rice_yield.csv", dst)) # write csv

# wheat price, yield, acres ---------------------------------

# price

winter_wheat_price <- read_csv("raw_data/net_returns/crops/price/winter_wheat_price_raw.csv") # load data
winter_wheat_price$`Commodity` <- "winter wheat" # create winter wheat version
durum_wheat_price <- read_csv("raw_data/net_returns/crops/price/winter_wheat_price_raw.csv") # load data
durum_wheat_price$`Commodity` <- "durum wheat" # create durum wheat version
spring_wheat_price <- read_csv("raw_data/net_returns/crops/price/spring_wheat_price_raw.csv") # load data
spring_wheat_price$`Commodity` <- "spring wheat" # create spring wheat version

dst = "processing/net_returns/crops/price/" # set destination
dir.create(dst,recursive = TRUE, showWarnings = FALSE) # create folders if they don't exist
write.csv(winter_wheat_price, sprintf("%s/winter_wheat_price.csv", dst)) # write csv
write.csv(durum_wheat_price, sprintf("%s/durum_wheat_price.csv", dst)) # write csv
write.csv(spring_wheat_price, sprintf("%s/spring_wheat_price.csv", dst)) # write csv

# yield

winter_wheat_yield <- read_csv("raw_data/net_returns/crops/yield/winter_wheat_yield_raw.csv") # load data
winter_wheat_yield$`Commodity` <- "winter wheat" # create winter wheat version
durum_wheat_yield <- read_csv("raw_data/net_returns/crops/yield/durum_wheat_yield_raw.csv") # load data
durum_wheat_yield$`Commodity` <- "durum wheat" # create durum wheat version
spring_wheat_yield <- read_csv("raw_data/net_returns/crops/yield/spring_wheat_yield_raw.csv") # load data
spring_wheat_yield$`Commodity` <- "spring wheat" # create spring wheat version

dst = "processing/net_returns/crops/yield/" # set destination
dir.create(dst,recursive = TRUE, showWarnings = FALSE) # create folders if they don't exist
write.csv(winter_wheat_yield, sprintf("%s/winter_wheat_yield.csv", dst)) # write csv
write.csv(durum_wheat_yield, sprintf("%s/durum_wheat_yield.csv", dst)) # write csv
write.csv(spring_wheat_yield, sprintf("%s/spring_wheat_yield.csv", dst)) # write csv

# acres

winter_wheat_acres <- read_csv("raw_data/net_returns/crops/acres/winter_wheat_acres_raw.csv") # load data
winter_wheat_acres$`Commodity` <- "winter wheat" # create winter wheat version
durum_wheat_acres <- read_csv("raw_data/net_returns/crops/acres/durum_wheat_acres_raw.csv") # load data
durum_wheat_acres$`Commodity` <- "durum wheat" # create durum wheat version
spring_wheat_acres <- read_csv("raw_data/net_returns/crops/acres/spring_wheat_acres_raw.csv") # load data
spring_wheat_acres$`Commodity` <- "spring wheat" # create spring wheat version

dst = "processing/net_returns/crops/acres/" # set destination
dir.create(dst,recursive = TRUE, showWarnings = FALSE) # create folders if they don't exist
write.csv(winter_wheat_acres, sprintf("%s/winter_wheat_acres.csv", dst)) # write csv
write.csv(durum_wheat_acres, sprintf("%s/durum_wheat_acres.csv", dst)) # write csv
write.csv(spring_wheat_acres, sprintf("%s/spring_wheat_acres.csv", dst)) # write csv

# census acres

winter_wheat_acres <- read_csv("raw_data/net_returns/crops/acres_census/winter_wheat_acres_c_raw.csv") # load data
winter_wheat_acres$`Commodity` <- "winter wheat" # create winter wheat version
durum_wheat_acres <- read_csv("raw_data/net_returns/crops/acres_census/durum_wheat_acres_c_raw.csv") # load data
durum_wheat_acres$`Commodity` <- "durum wheat" # create durum wheat version
spring_wheat_acres <- read_csv("raw_data/net_returns/crops/acres_census/spring_wheat_acres_c_raw.csv") # load data
spring_wheat_acres$`Commodity` <- "spring wheat" # create spring wheat version

dst = "processing/net_returns/crops/acres_census/" # set destination
dir.create(dst,recursive = TRUE, showWarnings = FALSE) # create folders if they don't exist
write.csv(winter_wheat_acres, sprintf("%s/winter_wheat_acres_c.csv", dst)) # write csv
write.csv(durum_wheat_acres, sprintf("%s/durum_wheat_acres_c.csv", dst)) # write csv
write.csv(spring_wheat_acres, sprintf("%s/spring_wheat_acres_c.csv", dst)) # write csv

# cotton price, yield, acres ---------------------------------

# price

pima_cotton_price <- read_csv("raw_data/net_returns/crops/price/pima_cotton_price_raw.csv") # load data
pima_cotton_price$`Commodity` <- "pima cotton" # create pima cotton version
upland_cotton_price <- read_csv("raw_data/net_returns/crops/price/upland_cotton_price_raw.csv") # load data
upland_cotton_price$`Commodity` <- "upland cotton" # create upland cotton version

dst = "processing/net_returns/crops/price/" # set destination
dir.create(dst,recursive = TRUE, showWarnings = FALSE) # create folders if they don't exist
write.csv(pima_cotton_price, sprintf("%s/pima_cotton_price.csv", dst)) # write csv
write.csv(upland_cotton_price, sprintf("%s/upland_cotton_price.csv", dst)) # write csv

# yield

pima_cotton_yield <- read_csv("raw_data/net_returns/crops/yield/pima_cotton_yield_raw.csv") # load data
pima_cotton_yield$`Commodity` <- "pima cotton" # create pima cotton version
upland_cotton_yield <- read_csv("raw_data/net_returns/crops/yield/upland_cotton_yield_raw.csv") # load data
upland_cotton_yield$`Commodity` <- "upland cotton" # create upland cotton version

dst = "processing/net_returns/crops/yield/" # set destination
dir.create(dst,recursive = TRUE, showWarnings = FALSE) # create folders if they don't exist
write.csv(pima_cotton_yield, sprintf("%s/pima_cotton_yield.csv", dst)) # write csv
write.csv(upland_cotton_yield, sprintf("%s/upland_cotton_yield.csv", dst)) # write csv

# acres

pima_cotton_acres <- read_csv("raw_data/net_returns/crops/acres/pima_cotton_acres_raw.csv") # load data
pima_cotton_acres$`Commodity` <- "pima cotton" # create pima cotton version
upland_cotton_acres <- read_csv("raw_data/net_returns/crops/acres/upland_cotton_acres_raw.csv") # load data
upland_cotton_acres$`Commodity` <- "upland cotton" # create upland cotton version

dst = "processing/net_returns/crops/acres/" # set destination
dir.create(dst,recursive = TRUE, showWarnings = FALSE) # create folders if they don't exist
write.csv(pima_cotton_acres, sprintf("%s/pima_cotton_acres.csv", dst)) # write csv
write.csv(upland_cotton_acres, sprintf("%s/upland_cotton_acres.csv", dst)) # write csv

# census acres

pima_cotton_acres <- read_csv("raw_data/net_returns/crops/acres_census/pima_cotton_acres_c_raw.csv") # load data
pima_cotton_acres$`Commodity` <- "pima cotton" # create pima cotton version
upland_cotton_acres <- read_csv("raw_data/net_returns/crops/acres_census/upland_cotton_acres_c_raw.csv") # load data
upland_cotton_acres$`Commodity` <- "upland cotton" # create upland cotton version

dst = "processing/net_returns/crops/acres_census/" # set destination
dir.create(dst,recursive = TRUE, showWarnings = FALSE) # create folders if they don't exist
write.csv(pima_cotton_acres, sprintf("%s/pima_cotton_acres_c.csv", dst)) # write csv
write.csv(upland_cotton_acres, sprintf("%s/upland_cotton_acres_c.csv", dst)) # write csv
