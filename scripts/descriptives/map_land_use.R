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

points <- read_dta("processing/pointpanel/countypanel_estimation_bal.dta") %>% as.data.table()

#Collapse points data set to county-year-lcc-land use conversion level data set
df <- points[ , .(total_acres = acresk), by = c("fips","year","initial_use","final_use","lcc")] %>%
  .[ , initial_acres := sum(total_acres), by = c("fips","year","initial_use","lcc")] %>% 
  merge(., points[ , .(stateAbbrev = first(stateAbbrev)), by = 'fips'], by = "fips") #Merge state abbreviation back in


df <- df[initial_use == final_use] #Drop repeated observations for each LCC x county x year 
#df <- df[initial_use == "Pasture" | initial_use == "Range" | initial_use == "CRP", initial_use := "Other"] #Recode pasture and range to "Other"
df <- df[ , .(acresk = sum(initial_acres)), by = c("fips","year","initial_use")] %>%
        .[initial_use %ni% c("Federal","Water"), total_acres := sum(acresk), by = c("fips","year")] %>%
        .[ , pctarea := 100*(acresk/total_acres)]

df <- merge(df %>% as_tibble() ,counties, by.x = "fips", by.y = "GEOID") %>%
        st_set_geometry("geometry")


# Import data ------------------------------------------------------------------

# df <- arrange(mutate(initial_use=factor(initial_use,
#                                             levels=c("Crop","Forest","Urban","Pasture","Range","CRP"))),initial_use)

ggplot(data = df %>% filter(year == 2012 & initial_use %ni% c("Federal","Rural","Water")) %>% 
         mutate(initial_use=factor(initial_use,levels=c("Crop","Forest","Urban","Pasture","Range","CRP")))) + 
  geom_sf(aes(fill = pctarea), color = NA, lwd = 0) + 
  facet_wrap(vars(initial_use)) + 
  labs(fill = "Percent\nArea") + 
  scale_fill_viridis_c() + 
  guides(fill = guide_colorbar(barwidth = 1.5)) + 
  theme_minimal() +
  theme(legend.position="right", legend.box = "vertical") + 
  theme(panel.border = element_blank(),
        panel.grid = element_blank(),
        # panel.grid=element_line(color="red"),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank()) 

path = "results/initial_descriptives/NRI/"
dir.create(path, recursive = TRUE, showWarnings = FALSE)
savefig(paste0(path,"map_land_use"), width = 6.5*1.5, height = 4*1.5)


