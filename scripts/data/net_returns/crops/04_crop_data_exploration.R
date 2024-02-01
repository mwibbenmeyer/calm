##################################################
## Project: Land Use
## Author: Sophie Pesek
## Date: May 3, 2021
## Script purpose: Explore combined crop yield data to determine how much is missing
## Input data: crop_returns.csv - the combined crop data set
##################################################

## Load/install packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse,
               data.table,
               ggplot2,
               usmap,
               sf,
               rnaturalearth,
               rnaturalearthdata,
               maps,
               tidycensus,
               tm,
               reshape2,
               rgeos
                )

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # sets directory to the current directory
setwd('../../../..') # relative paths to move directory to the root project directory

world <- ne_countries(scale = "medium", returnclass = "sf") # load world data
class(world)
counties <- st_as_sf(map("county", plot = FALSE, fill = TRUE)) # load county data
crop_returns <- read_csv("processing/net_returns/crop_returns.csv") %>% # load crop returns data
  as.data.table() 
  # rename(., fips = county_fips) 

corn_2002 <- crop_returns[crop_returns$crop == "corn" & crop_returns$year == "2002",]
ploty <- plot_usmap(data = corn_2002, values = "acres", regions = "counties", exclude = c("AK","HI")) +
  scale_fill_viridis_c(label = scales::comma) +
  labs(title = "Corn in 2002", fill = "Acres")
ggsave("results/initial_descriptives/net_returns/crops/maps/experiment.jpg")

##################################################
## plot missing data from crop_returns
##################################################

crop = c("corn", "sorghum", "soybeans", "winter wheat", "durum wheat", "spring wheat", "barley", "oats", "rice", "upland cotton", "pima cotton") # list of crops
year = c(2002:2020)
rm(i)
rm(j)
for(i in crop) {
  for(j in year) {
    c <- crop_returns[crop_returns$year == j & crop_returns$crop == i,]
    c <- c[, c("county_fips", "price", "cost", "yield", "acres")]
    c <- melt(c, id.vars = c('county_fips'))  # melt data
    c <- c[!is.na(c$value), ]                  # remove NA
    c <- with(c, aggregate(c, by = list(variable), FUN = length )) # compute length by grouping variable
    
    ggplot(c, aes( x = Group.1, y = value/3112, fill = Group.1 )) + 
      geom_bar(stat="identity") + ggtitle(sprintf("Data for crop %s in year %s", i, toString(j))) +
      xlab("Returns data") + ylab("Portion of total counties with data") + ylim(0,1)
    
    ggsave(sprintf("results/initial_descriptives/net_returns/crops/plots/%s_%s.png", i, toString(j)))
  }
}
  
##################################################
## map missing data from crop_returns
##################################################

# create FIPS and county ID variables from tidycensus data

data(fips_codes) # load FIPS county data
fips_codes$county_fips = paste(fips_codes$state_code, fips_codes$county_code, sep="") # add a 5-digit FIPS code
fips_codes$county <- removePunctuation(fips_codes$county) # remove punctuation
fips_codes$county <- sub("District of Columbia", "Washington", fips_codes$county) # handle washington, d.c.
fips_codes$county <- sub("LaMoure", "La Moure", fips_codes$county) # handle la moure, sd
fips_codes$county <- sub("DeKalb", "De Kalb", fips_codes$county) # handle de kalb, mo (and in, il, al, ga, tn)
fips_codes$county <- sub("LaPorte", "La Porte", fips_codes$county) # handle la porte, in
fips_codes$county <- sub("DuPage", "Du Page", fips_codes$county) # handle du page, il
fips_codes$county <- sub("LaSalle", "La Salle", fips_codes$county) # handle la salle, il
fips_codes$county <- sub("DeWitt", "De Witt", fips_codes$county) # handle de witt, tx
fips_codes$county <- sub("DeSoto", "De Soto", fips_codes$county) # handle de soto, ms (and fl)
fips_codes$county <- sub("MiamiDade", "Miami-Dade", fips_codes$county) # handle miami dade, fl
fips_codes$ID <- paste(fips_codes$state_name, fips_codes$county, sep=",") # append state and county name
fips_codes$ID <- str_remove(fips_codes$ID, " County") # remove designation that don't appear in geographic ID data
fips_codes$ID <- str_remove(fips_codes$ID, " Parish")
fips_codes$ID <- str_remove(fips_codes$ID, " District")
fips_codes$ID <- str_remove(fips_codes$ID, " City")
fips_codes <- mutate_all(fips_codes, .funs=tolower) # change to lowercase
write.csv(fips_codes, "processing/fips_codes.csv") # write csv

# join crop_returns and geographic data

new_crop_returns <- left_join(x = crop_returns, y = fips_codes, by = "county_fips") # join crop returns data to state IDs
write.csv(new_crop_returns, "processing/net_returns/new_crop_returns.csv") # write csv
new_crop_returns1 <- new_crop_returns[, c("year","acres", "ID")] # trim columns

# data with crop acres for all years

df2 <- new_crop_returns1[, c("acres", "ID")]
df3 <- df2 %>% # count rows with acres data in same year and ID group
  group_by(ID) %>%
  summarise_each(funs(sum(!is.na(.))))
new_counties1 <- left_join(x = counties, y = df3, by = "ID") # join acres data to geographic state IDs
new_counties1$has_acres <- ifelse(new_counties1$acres < 1,0,1) # create a factor for counties with and without acres
pct <- round(length(which(new_counties1$has_acres == 1))/length(which(new_counties1$has_acres == 1 | new_counties1$has_acres == 0)), 4) # calculate percent of counties with data

# map counties with any acres data

ggplot(data = world) + # map US counties
  geom_sf() + geom_sf(data=subset(new_counties1, !is.na(has_acres)), aes(fill = factor(has_acres))) + # fill with number of acres
  scale_fill_discrete(name = "Has acres-planted data", labels = c("None", "Some")) + # change legend labels and colors
  ggtitle(sprintf("Counties with crop acres-planted data in any year (%s%%)", toString(pct*100))) + # set color scale and title
  coord_sf(xlim = c(-125, -66), ylim = c(24, 50), expand = FALSE) # set coordinates to continental U.S.

ggsave("results/initial_descriptive/net_returns/crops/maps/map_acres.png", width = 18, height = 10, dpi=96) # save map

# data with crop acres per year

df1 <- new_crop_returns1 %>% # count rows with acres data in same year and ID group
  group_by(ID, year) %>%
  summarise_each(funs(sum(!is.na(.))))
new_counties <- left_join(x = counties, y = df1, by = "ID") # join acres data to geographic state IDs
new_counties$has_acres <- ifelse(new_counties$acres < 1,0,1) # create a factor for counties with and without acres

# loop through years to map counties with acres data by year

rm(j)
for(j in year) {
  new_counties2 <- new_counties[new_counties$year == j,] # subset data by each year
  pct <- round(length(which(new_counties2$has_acres == 1))/length(which(new_counties2$has_acres == 1 | new_counties2$has_acres == 0)), 4) # calculate percent of counties with data
  
  ggplot(data = world) + # map US counties
    geom_sf() + geom_sf(data=subset(new_counties2, !is.na(has_acres)), aes(fill = factor(has_acres))) + # fill with number of acres
    scale_fill_discrete(name = "Has acres-planted data", labels = c("None", "Some")) + # change legend labels and colors
    ggtitle(sprintf("Counties with crop acres-planted data in %s (%s%%)", toString(j), toString(pct*100))) + # set color scale and title
    coord_sf(xlim = c(-125, -66), ylim = c(24, 50), expand = FALSE) # set coordinates to continental U.S.
  
  ggsave(sprintf("results/initial_descriptives/net_returns/crops/maps/map_%s_acres.png", toString(j)), width = 18, height = 10, dpi=96) # save map
}

##################################################
## map missing data from crop_returns - using census data
##################################################

new_crop_returns2 <- new_crop_returns[, c("year","acres_c", "ID")] # trim columns

# data with crop acres for all years

df2 <- new_crop_returns2[, c("acres_c", "ID")]
df3 <- df2 %>% # count rows with acres data in same year and ID group
  group_by(ID) %>%
  summarise_each(funs(sum(!is.na(.))))
new_counties1 <- left_join(x = counties, y = df3, by = "ID") # join acres data to geographic state IDs
new_counties1$has_acres <- ifelse(new_counties1$acres_c < 1,0,1) # create a factor for counties with and without acres
pct <- round(length(which(new_counties1$has_acres == 1))/length(which(new_counties1$has_acres == 1 | new_counties1$has_acres == 0)), 4) # calculate percent of counties with data

# map counties with any acres data

ggplot(data = world) + # map US counties
  geom_sf() + geom_sf(data=subset(new_counties1, !is.na(has_acres)), aes(fill = factor(has_acres))) + # fill with number of acres
  scale_fill_discrete(name = "Has acres-planted data", labels = c("None", "Some")) + # change legend labels and colors
  ggtitle(sprintf("Counties with CENSUS crop acres-planted data in any year (%s%%)", toString(pct*100))) + # set color scale and title
  coord_sf(xlim = c(-125, -66), ylim = c(24, 50), expand = FALSE) # set coordinates to continental U.S.

ggsave("results/initial_descriptives/net_returns/crops/maps/map_acres_census.png", width = 18, height = 10, dpi=96) # save map

# data with crop acres per year

df1 <- new_crop_returns2 %>% # count rows with acres data in same year and ID group
  group_by(ID, year) %>%
  summarise_each(funs(sum(!is.na(.))))
new_counties <- left_join(x = counties, y = df1, by = "ID") # join acres data to geographic state IDs
new_counties$has_acres <- ifelse(new_counties$acres_c < 1,0,1) # create a factor for counties with and without acres

# loop through years to map counties with acres data by year

year_c = c(2002, 2007, 2012, 2017)
rm(j)
for(j in year_c) {
  new_counties2 <- new_counties[new_counties$year == j,] # subset data by each year
  pct <- round(length(which(new_counties2$has_acres == 1))/length(which(new_counties2$has_acres == 1 | new_counties2$has_acres == 0)), 4) # calculate percent of counties with data
  
  ggplot(data = world) + # map US counties
    geom_sf() + geom_sf(data=subset(new_counties2, !is.na(has_acres)), aes(fill = factor(has_acres))) + # fill with number of acres
    scale_fill_discrete(name = "Has acres-planted data", labels = c("None", "Some")) + # change legend labels and colors
    ggtitle(sprintf("Counties with crop CENSUS acres-planted data in %s (%s%%)", toString(j), toString(pct*100))) + # set color scale and title
    coord_sf(xlim = c(-125, -66), ylim = c(24, 50), expand = FALSE) # set coordinates to continental U.S.
  
  ggsave(sprintf("results/initial_descriptives/net_returns/crops/maps/map_%s_acres_census.png", toString(j)), width = 18, height = 10, dpi=96) # save map
}


##################################################
## % differnce in survey and census data
##################################################

census_survey <- new_crop_returns[, c("year","acres", "acres_c", "ID")] # trim columns for comparison of census and survey data
census_survey_acres <- left_join(x = counties, y = new_crop_returns, by = "ID") # join
census_survey_acres$pct_diff <- (census_survey_acres$acres_c-census_survey_acres$acres)/census_survey_acres$acres*100 # calculate percent difference

# data comparing crop acres per year

year_c = c(2002, 2007, 2012, 2017)
rm(j)
for(j in year_c) {
  census_survey_acres1 <- census_survey_acres[census_survey_acres$year == j,] # subset data by each year

  ggplot(data = world) + # map US counties
    geom_sf(data=counties, aes(geometry=geom)) + geom_sf(data=subset(census_survey_acres1, !is.na(pct_diff)), aes(fill = pct_diff)) + # fill with number of acres
    scale_fill_viridis_c(name = "(census-survey acres)/survey acres") + # change legend labels and colors
    ggtitle(sprintf("Percent change from survey to census acres-planted data in %s", toString(j))) + # set color scale and title
    coord_sf(xlim = c(-125, -66), ylim = c(24, 50), expand = FALSE) # set coordinates to continental U.S.
  
  ggsave(sprintf("results/initial_descriptives/net_returns/crops/maps/map_%s_acres_census_survey.png", toString(j)), width = 18, height = 10, dpi=96) # save map
}


# loop through crops and years to map each
#
# rm(i)
# rm(j)
# for(i in crop) {
#   for(j in year) {
#     counties_subset = new_counties[new_counties$year == j & new_counties$crop == i,] # subset to year and crop
#     
#     ggplot(data = world) + # map US counties
#       geom_sf() +
#       geom_sf(data = counties_subset, aes(fill = acres)) +
#       scale_fill_viridis_c() + ggtitle(sprintf("Acres of %s in %s", i, toString(j))) +
#       coord_sf(xlim = c(-125, -66), ylim = c(24, 50), expand = FALSE)
#     
#     ggsave(sprintf("results/initial_descriptives/net_returns/crops/maps/map_%s_%s_acres.png", i, toString(j)), width = 20, height = 12, dpi=96) # save map
#   }
# }

