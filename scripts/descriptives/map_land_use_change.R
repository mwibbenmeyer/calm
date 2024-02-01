####################################################
# Matt Wibbenmeyer
# January 31, 2022
# Plot of land use by county
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
setwd("../../")

savefig <- function(path,width,height) {
  lapply(c("png","pdf"), function(ftype) 
    ggplot2::ggsave(paste0(path,".",ftype),
                    device=ftype,width=width,height=height))
}

`%ni%` <- Negate(`%in%`)
options(tigris_use_cache = TRUE) #Tell tidycensus to cache shapefiles for future sessions

#census_api_key("", overwrite = TRUE, install = TRUE) # set API key

# Import data ------------------------------------------------------------------

aea <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

#Import county shapefile using tidycensus - b19013_001 is arbitrarily chosen
states <- state.abb[state.abb %ni% c("AK","HI")]
counties <- get_acs(state = states, geography = "county", year = 2010, variables = "B19013_001", geometry = TRUE) %>%
  select(-c("variable","estimate","moe")) %>%
  st_transform(aea)

points <- read_dta("processing_output/countypanel_estimation_bal.dta") %>% as.data.table()

#Collapse points data set to county-year-lcc-land use conversion level data set
df <- points[ , .(total_acres = acresk), by = c("fips","year","initial_use","final_use","lcc")] %>%
  .[ , initial_acres := sum(total_acres), by = c("fips","year","initial_use","lcc")] %>%
  merge(., points[ , .(stateAbbrev = first(stateAbbrev)), by = 'fips'], by = "fips") #Merge state abbreviation back in
df <- df[initial_use == "Pasture" | initial_use == "Range", initial_use := "Other"] #Recode pasture and range to "Other"


df2 <- df[initial_use != final_use, .(acres_out = sum(total_acres)), by = c("fips", "year", "initial_use")] %>%
        #Merge with total land beginning in each land use
        merge(df[initial_use == final_use] %>%
                .[initial_use %ni% c("Federal","Water"), .(acresk = sum(initial_acres)), by = c("fips","year","initial_use")]) %>%
        .[ , pctout := 100*(acres_out/acresk)] %>%
        #Merge with total land in county
        merge(df[initial_use == final_use] %>% 
                .[initial_use %ni% c("Federal","Water"), .(county_acres = sum(initial_acres)), by = c("fips","year")], 
            by = c("fips","year"))

df2 <- merge(df2 %>% as_tibble() ,counties, by.x = "fips", by.y = "GEOID") %>%
  st_set_geometry("geometry") %>% st_make_valid()


# Import data ------------------------------------------------------------------

ggplot(data = df2 %>% filter(year == 2012 & initial_use %ni% c("Federal","Rural","Water"))) + 
  geom_sf(aes(fill = acres_out), color = NA, lwd = 0) + 
  facet_wrap(vars(initial_use)) + 
  labs(fill = "Percent\n change") + 
  scale_fill_viridis_c() + 
  guides(fill = guide_colorbar(barwidth = 12)) + 
  theme_minimal() +
  theme(legend.position="bottom", legend.box = "horizontal") + 
  theme(panel.border = element_blank(),
        panel.grid = element_blank(),
        # panel.grid=element_line(color="red"),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank()) 

path = "results/initial_descriptives/NRI/"
dir.create(path, recursive = TRUE, showWarnings = FALSE)
savefig(paste0(path,"map_land_use"), width = 6.5*1.5, height = 4*1.5)



