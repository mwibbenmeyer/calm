####################################################
# Matt Wibbenmeyer
# April 19, 2022
####################################################

## Load/install packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse,
               dplyr,
               readxl,
               sf,
               tidycensus,
               haven,
               stringr,
               data.table
)

# Set working directory to land-use 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../../../")

# Function to do a real GIS union operation such as in ArcGIS (this version excludes areas in b and not in a)
my_union <- function(a,b) {
  #
  # a - the first sf
  a <- counties
  b <- ecoregions %>% st_filter(.,counties, predicate = st_intersects())
  # b - the second sf
  
  if (dim(b)[1] > 0)  {
    
    op1 <- st_difference(a,st_union(b))
    missingcols <- setdiff(names(b),names(a))
    op1[missingcols] <- NA
    
    op3 <- st_intersection(b, a)
    union <- rbind(op1, op3)
    
  }    
  
  else {
    a[setdiff(names(b),names(a))] <- NA
    union <- a
  }
  
  return(union)
}

my_union2 <- function(a,b) {
  #
  # function doing a real GIS union operation such as in QGIS or ArcGIS
  #
  # a - the first sf
  # b - the second sf
  #
  st_agr(a) = "constant"
  st_agr(b) = "constant"
  op1 <- st_difference(a,st_union(b))
  op2 <- st_difference(b, st_union(a))
  op3 <- st_intersection(b, a)
  union <- plyr::rbind.fill(op1, op2, op3)
  return(st_as_sf(union))
}


states <- state.abb[state.abb %ni% c("AK","HI")]
counties <- get_acs(state = states, geography = "county", year = 2010, variables = "B19013_001", geometry = TRUE) %>%
  select(-c("variable","estimate","moe")) %>%
  st_transform(aea) %>%
  filter()

ecoregions <- read_sf("raw_data/misc/ecoregions/Data/eco_us.shp") %>%
                st_transform(aea)

ggplot(data = ecoregions %>% st_filter(.,counties, predicate = st_intersects())) + 
  geom_sf(aes(fill = DIVISION)) + 
  geom_sf(data = counties, fill = NA) + 
  theme_minimal()

#Map shows that ecoregions cross county boundaries; need to do a GIS union operation to identify ecoregion
#that constitutes majority of each county

c_eco_int <- my_union2(counties, 
                        ecoregions %>% st_filter(.,counties, predicate = st_intersects())) 

%>%
              dplyr::select(GEOID,NAME,geometry)

int <- c_eco_int %>% st_intersection(ecoregions %>% st_filter(.,counties, predicate = st_intersects()))


int$area <- st_area(int)
county_eco <- int %>% group_by(GEOID,DIVISION) %>%
              mutate(max_area = max(area, na.rm = TRUE)) %>% 
              filter(area == max_area)

ggplot(data = county_eco) + 
  geom_sf(aes(fill = DIVISION)) + 
  geom_sf(data = counties, fill = NA) + 
  theme_minimal()




