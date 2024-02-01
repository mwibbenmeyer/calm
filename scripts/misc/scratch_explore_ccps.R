## Load/install packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse,
               readxl,
               sf,
               tidycensus,
               haven,
               stringr,
               data.table,
               dplyr,
               haven,
               patchwork
)

setwd("F:/Projects/land-use/")

rm(df)
gc()

df <- read_dta("processing_output/countypanel_estimation_bal.dta")
df <- df %>% as.data.table()

counties <- get_decennial(geography = "county", year = 2000, variables = "P001001", geometry = TRUE, cache = TRUE) %>% 
  filter(!st_is_empty(.)) %>% #Remove features with empty geometry
  st_transform(st_crs(4269))

df.county <- df[ , initial_acres := sum(acresk, na.rm = TRUE), by = c("fips","year","initial_use","lcc")] %>%
              .[ , prob := acresk/initial_acres]

keep.uses = c("Forest","Urban","Crop")
df.geom <- merge(as.data.frame(df.county[lcc == "3_4" & initial_use %in% keep.uses & final_use %in% keep.uses]),
                 counties, by.x = "fips", by.y = "GEOID") %>% as.data.table()

map <- ggplot(data = df.geom[year == 2002 & initial_use == "Crop" & final_use == "Forest", ]) + 
  geom_sf(aes(fill = prob, geometry = geometry)) + 
  geom_sf(data = df.geom[year == 2002 & initial_use == "Crop" & final_use == "Forest" & prob > 0, ],
          aes(geometry = geometry), fill = "red")
map

histogram <- ggplot(data = df.geom[year == 2002 & initial_use == "Crop" & final_use == "Forest", ]) + 
  geom_histogram(aes(x = prob)) 

map + histogram
scatter <- ggplot(data = df.geom[year == 2002 & initial_use == "Crop" & final_use == "Forest", ]) + 
            geom_point(aes(x = prob, y = initial_acres)) 
scatter

df.county[ , same.use := as.numeric(initial_use == final_use)] %>%
        .[ , .(mean = mean(prob)), by = same.use]



rm(initial_use)
df.geom <- df.geom %>% as.data.table()
df.geom[final_use=="Forest",]

rm(final_use)
unique(df.geom$initial_use)[[3]]
unique(df.geom$initial_use)[[1]]
