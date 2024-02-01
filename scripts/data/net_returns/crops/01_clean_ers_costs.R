##################################################
## Project: Land Use
## Author: Sophie Pesek
## Date: Mar 31, 2021
## Script purpose: Correct units and clean crop cost data
## Input data: ERS Cost Return data
##################################################

library(readxl)
library(tidyverse)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # sets directory to the current directory
setwd('../../../..') # relative paths to move directory to the root project directory

# corn ---------------------------------

corn <- read_excel("raw_data/net_returns/crops/cost/CornCostReturn.xlsx", sheet = "Data Sheet (machine readable)") # load data

cost <- filter(corn, Item == "Total, operating costs") # select only operating costs ($/planted acre)
labor_cost <- filter(corn, Item == "Hired labor")
# yield <- filter(corn, Item == "Yield") # select only yield (bushels/planted acre)
# new_value <- select(cost, Value)*(1/select(yield, Value)) # multiply to get new cost ($/bushel)
# cost <- cbind(cost, new_col = new_value) # add new column to data frame
corn_cost = select(cost, 1:3, 9, 11:12, 15:17) # select only relevant columns
corn_labor_cost = select(labor_cost, 1:3, 9, 11:12, 15:17) # select only relevant columns
corn_cost = merge(corn_cost, corn_labor_cost %>% 
  select(RegionId, Year, Value), by = c('RegionId', 'Year')) %>%
  mutate(Value = Value.x + Value.y) %>% select(-Value.x, -Value.y)

# corn_cost = select(cost, 1:3, 11:12, 15, 17, 22) # select only relevant columns

dst = "processing/net_returns/crops/cost/" # set destination
dir.create(dst,recursive = TRUE, showWarnings = FALSE) # create folders if they don't exist
write.csv(corn_cost, sprintf("%s/corn_cost.csv", dst)) # write csv

# sorghum ---------------------------------

sorghum <- read_excel("raw_data/net_returns/crops/cost/SorghumCostReturn.xlsx", sheet = "Data Sheet (machine readable)") # load data

cost <- filter(sorghum, Item == "Total, operating costs") # select only costs ($/planted acre)
labor_cost <- filter(sorghum, Item == "Hired labor")

# yield <- filter(sorghum, Item == "Yield") # select only yield (bushels/planted acre)
# new_value <- select(cost, Value)*(1/select(yield, Value)) # multiply to get new cost ($/bushel)
# cost <- cbind(cost, new_col = new_value) # add new column to data frame
# sorghum_cost = select(cost, 1:3, 11:12, 15, 17, 22) # select only relevant columns
sorghum_cost = select(cost, 1:3, 9, 11:12, 15:17) # select only relevant columns
sorghum_labor_cost = select(labor_cost, 1:3, 9, 11:12, 15:17) # select only relevant columns
sorghum_cost = merge(sorghum_cost, sorghum_labor_cost %>% 
                    select(RegionId, Year, Value), by = c('RegionId', 'Year')) %>%
  mutate(Value = Value.x + Value.y) %>% select(-Value.x, -Value.y)

write.csv(sorghum_cost, sprintf("%s/sorghum_cost.csv", dst)) # write csv

# soybeans ---------------------------------

soybeans <- read_excel("raw_data/net_returns/crops/cost/SoybeansCostReturn.xlsx", sheet = "Data Sheet (machine readable)") # load data

cost <- filter(soybeans, Item == "Total, operating costs") # select only costs ($/planted acre)
labor_cost <- filter(soybeans, Item == "Hired labor")

# yield <- filter(soybeans, Item == "Yield") # select only yield (bushels/planted acre)
# new_value <- select(cost, Value)*(1/select(yield, Value)) # multiply to get new cost ($/bushel)
# cost <- cbind(cost, new_col = new_value) # add new column to data frame
# soybeans_cost = select(cost, 1:3, 11:12, 15, 17, 22) # select only relevant columns
soybeans_cost = select(cost, 1:3, 9, 11:12, 15:17) # select only relevant columns
soybeans_labor_cost = select(labor_cost, 1:3, 9, 11:12, 15:17) # select only relevant columns
soybeans_cost = merge(soybeans_cost, soybeans_labor_cost %>% 
                       select(RegionId, Year, Value), by = c('RegionId', 'Year')) %>%
  mutate(Value = Value.x + Value.y) %>% select(-Value.x, -Value.y)

soybeans_cost$`Commodity` <- "soybeans" # rename crop entires to be plural

write.csv(soybeans_cost, sprintf("%s/soybeans_cost.csv", dst)) # write csv

# wheat ---------------------------------

wheat <- read_excel("raw_data/net_returns/crops/cost/WheatCostReturn.xlsx", sheet = "Data Sheet (machine readable)") # load data

cost <- filter(wheat, Item == "Total, operating costs") # select only costs ($/planted acre)
labor_cost <- filter(wheat, Item == "Hired labor")

# yield <- filter(wheat, Item == "Yield") # select only yield (bushels/planted acre)
# new_value <- select(cost, Value)*(1/select(yield, Value)) # multiply to get new cost ($/bushel)
# cost <- cbind(cost, new_col = new_value) # add new column to data frame
# wheat_cost = select(cost, 1:3, 11:12, 15, 17, 22) # select only relevant columns
wheat_cost = select(cost, 1:3, 9, 11:12, 15:17) # select only relevant columns
wheat_labor_cost = select(labor_cost, 1:3, 9, 11:12, 15:17) # select only relevant columns
wheat_cost = merge(wheat_cost, wheat_labor_cost %>% 
                       select(RegionId, Year, Value), by = c('RegionId', 'Year')) %>%
  mutate(Value = Value.x + Value.y) %>% select(-Value.x, -Value.y)

wheat_cost[wheat_cost=="Wheat"]<-"winter wheat" # create winter wheat version
write.csv(wheat_cost, sprintf("%s/winter_wheat_cost.csv", dst)) # write csv

wheat_cost[wheat_cost=="winter wheat"]<-"durum wheat" # create durum wheat version
write.csv(wheat_cost, sprintf("%s/durum_wheat_cost.csv", dst)) # write csv

wheat_cost[wheat_cost=="durum wheat"]<-"spring wheat" # create other spring wheat version
write.csv(wheat_cost, sprintf("%s/spring_wheat_cost.csv", dst)) # write csv

# barley ---------------------------------

barley <- read_excel("raw_data/net_returns/crops/cost/BarleyCostReturn.xlsx", sheet = "Data Sheet (machine readable)") # load data

cost <- filter(barley, Item == "Total, operating costs") # select only costs ($/planted acre)
labor_cost <- filter(barley, Item == "Hired labor")

# yield <- filter(barley, Item == "Yield") # select only yield (bushels/planted acre)
# new_value <- select(cost, Value)*(1/select(yield, Value)) # multiply to get new cost ($/bushel)
# cost <- cbind(cost, new_col = new_value) # add new column to data frame
# barley_cost = select(cost, 1:3, 11:12, 15, 17, 22) # select only relevant columns
barley_cost = select(cost, 1:3, 9, 11:12, 15:17) # select only relevant columns
barley_labor_cost = select(labor_cost, 1:3, 9, 11:12, 15:17) # select only relevant columns
barley_cost = merge(barley_cost, barley_labor_cost %>% 
                       select(RegionId, Year, Value), by = c('RegionId', 'Year')) %>%
  mutate(Value = Value.x + Value.y) %>% select(-Value.x, -Value.y)

write.csv(barley_cost, sprintf("%s/barley_cost.csv", dst)) # write csv

# oats ---------------------------------

oats <- read_excel("raw_data/net_returns/crops/cost/OatsCostReturn.xlsx", sheet = "Data Sheet (machine readable)") # load data

cost <- filter(oats, Item == "Total, operating costs") # select only costs ($/planted acre)
labor_cost <- filter(oats, Item == "Hired labor")

# yield <- filter(oats, Item == "Yield") # select only yield (bushels/planted acre)
# new_value <- select(cost, Value)*(1/select(yield, Value)) # multiply to get new cost ($/bushel)
# cost <- cbind(cost, new_col = new_value) # add new column to data frame
# oats_cost = select(cost, 1:3, 11:12, 15, 17, 22) # select only relevant columns
oats_cost = select(cost, 1:3, 9, 11:12, 15:17) # select only relevant columns
oats_labor_cost = select(labor_cost, 1:3, 9, 11:12, 15:17) # select only relevant columns
oats_cost = merge(oats_cost, oats_labor_cost %>% 
                       select(RegionId, Year, Value), by = c('RegionId', 'Year')) %>%
  mutate(Value = Value.x + Value.y) %>% select(-Value.x, -Value.y)

write.csv(oats_cost, sprintf("%s/oats_cost.csv", dst)) # write csv

# rice ---------------------------------

rice <- read_excel("raw_data/net_returns/crops/cost/RiceCostReturn.xlsx", sheet = "Data Sheet (machine readable)") # load data

cost <- filter(rice, Item == "Total, operating costs") # select only costs ($/planted acre)
labor_cost <- filter(rice, Item == "Hired labor")

# yield <- filter(rice, Item == "Yield") # select only yield (bushels/planted acre)
# new_value <- select(cost, Value)*(1/select(yield, Value)) # multiply to get new cost ($/bushel)
# cost <- cbind(cost, new_col = new_value) # add new column to data frame
# rice_cost = select(cost, 1:3, 11:12, 15, 17, 22) # select only relevant columns
rice_cost = select(cost, 1:3, 9, 11:12, 15:17) # select only relevant columns
rice_labor_cost = select(labor_cost, 1:3, 9, 11:12, 15:17) # select only relevant columns
rice_cost = merge(rice_cost, rice_labor_cost %>% 
                       select(RegionId, Year, Value), by = c('RegionId', 'Year')) %>%
  mutate(Value = Value.x + Value.y) %>% select(-Value.x, -Value.y)

write.csv(rice_cost, sprintf("%s/rice_cost.csv", dst)) # write csv

# cotton ---------------------------------

cotton <- read_excel("raw_data/net_returns/crops/cost/CottonCostReturn.xlsx", sheet = "Data Sheet (machine readable)") # load data

cost <- filter(cotton, Item == "Total, operating costs") # select only costs ($/planted acre)
labor_cost <- filter(cotton, Item == "Hired labor")

# yield <- filter(cotton, Item == "Yield") # select only yield (bushels/planted acre)
# new_value <- select(cost, Value)*(1/select(yield, Value)) # multiply to get new cost ($/bushel)
# cost <- cbind(cost, new_col = new_value) # add new column to data frame
# cotton_cost = select(cost, 1:3, 11:12, 15, 17, 22) # select only relevant columns
cotton_cost = select(cost, 1:3, 9, 11:12, 15:17) # select only relevant columns
cotton_labor_cost = select(labor_cost, 1:3, 9, 11:12, 15:17) # select only relevant columns
cotton_cost = merge(cotton_cost, cotton_labor_cost %>% 
                       select(RegionId, Year, Value), by = c('RegionId', 'Year')) %>%
  mutate(Value = Value.x + Value.y) %>% select(-Value.x, -Value.y)

cotton_cost[cotton_cost=="Cotton"]<-"pima cotton" # create pima cotton version
write.csv(cotton_cost, sprintf("%s/pima_cotton_cost.csv", dst)) # write csv

cotton_cost[cotton_cost=="pima cotton"]<-"upland cotton" # create upland cotton version
write.csv(cotton_cost, sprintf("%s/upland_cotton_cost.csv", dst)) # write csv

