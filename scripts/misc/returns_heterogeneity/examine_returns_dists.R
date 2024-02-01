# load/install packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse,
               sf,
               maps,
               rnaturalearth,
               rnaturalearthdata,
               tidycensus,
               ggplot2,
               usmap,
               patchwork,
               scales)
theme_set(theme_bw())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)); setwd('../../../') # relative paths to move directory to the root project directory

{
# colors
rffblue <- "#88C4F4"
rffred <- "#FF6663"
rffgreen <- "#50B161"
rffbrown <- "#74645E"
rffblack <- "#04273C"
rffpurple <- "#755EA6"
rffyellow <- "#EAD367"
rfforange <- "#F4A25F"
  
ecoregion_colors <- c(#East Coast
                      "211" = "#006d1a",
                      "212" = "#0d572b",
                      "221" = "#41ae76",
                      "222" = "#66c2a4",
                      "223" = "#449178",
                      "231" = "#00441b",
                      "232" = "#238b45",
                      "234" = "#e5f5f9",
                      "251" = "#ccece6",
                      "255" = "#99d8c9",
                      "411" = "#9ecae1",
                      
                      #"411" = "#f7fcfd",
                      
                      #Great Plains
                      "331" = "#feedde",
                      "332" = "#fdbe85",
                      "315" = "#fd8d3c",
                      # "311" = "#d94701",
                      
                      #Desert & Rockies
                      # "321" = "#ffffd4",
                      # "322" = "#fee391",
                      "313" = "#fec44f",
                      "341" = "#fe9929",
                      # "342" = "#ec7014",
                      "333" = "#cc4c02",
                      # "334" = "#8c2d04",
                      
                      #West Coast
                      "242" = "#3182bd",
                      "261" = "#9ecae1",
                      "262" = "#deebf7",
                      "263" = "#a5b8c9"
                    )

ecoregion_labels <- c("211" = "NE Mixed Forest",
                      "212" = "Laurentian",
                      "221" = "E. Broadleaf",
                      "222" = "Midwest Broadleaf",
                      "223" = "Central Int. Broadleaf",
                      "231" = "S.E. Mixed Forest",
                      "232" = "Outer Coastal Plain",
                      "234" = "Lower Miss. Riverine",
                      "251" = "Prarie Parkland-T",
                      "255" = "Prarie Parkland-ST",
                      "411" = "Everglades", #Missing from Dave
                      
                      #Great Plains
                      "331" = "Southern Rockies/GP",
                      "332" = "Middle Rockies",
                      "315" = "SW Plateau",
                      # "311" = "#d94701",
                      
                      #Desert & Rockies
                      # "321" = "#ffffd4",
                      # "322" = "#fee391",
                      "313" = "SW Semi Desert", 
                      "341" = "Int Mtn Semi Desert",
                      # "342" = "#ec7014",
                      "333" = "Northern Rockies",
                      # "334" = "#8c2d04", 
                      
                      #West Coast
                      "242" = "Pacific Cascade Mixed",
                      "261" = "Sierras Mixed Forest",
                      "262" = "CA Dry Steppe",
                      "263" = "CA Coastal Forest" 
                    )
}

aea <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

df <- read.csv("raw_data/net_returns/forest/forest_plot_rents/plot_rents_by_county_South.csv") %>% 
        mutate(county = str_pad(county, width = 5, pad = "0", side = "left"))
  
ecoregions <- read.csv("processing/misc/ecoregions/interpolated_ecocd_counties.csv") %>% 
                mutate(fips = str_pad(fips, width = 5, pad = "0", side = "left"))

ecoregion_label_df <- rownames_to_column(as.data.frame(ecoregion_labels))
colnames(ecoregion_label_df) <- c("ecoregion","ecoregion_label")
ecoregions <- merge(ecoregions, ecoregion_label_df, by = "ecoregion")
  
counties <- get_acs(geography = "county", year = 2010, variables = "B19013_001", geometry = TRUE) %>%
  mutate(fips = str_pad(GEOID, width = 5, pad = "0", side = "left")) %>% 
  select(-c("variable","estimate","moe")) %>%
  filter(substr(GEOID,1,2) != "72") %>% 
  st_transform(aea) %>% 
  st_make_valid()

states <- get_acs(geography = "state", year = 2010, variables = "B19013_001", geometry = TRUE) %>%
  mutate(fips = str_pad(GEOID, width = 2, pad = "0", side = "left")) %>% 
  select(-c("variable","estimate","moe")) %>%
  filter(GEOID != "72") %>% 
  st_transform(aea) %>% 
  st_make_valid()

df <- merge(df, ecoregions, by.x = "county", by.y = "fips")

counties <- merge(counties, ecoregions, by = 'fips')

m <- ggplot(data = counties %>% filter((ecoregion %in% unique(df$ecoregion)))) +
  geom_sf(aes(fill = as.character(ecoregion)),
          color = NA) + 
  scale_fill_manual(values = ecoregion_colors[as.character(unique(df$ecoregion))], 
                    labels = ecoregion_labels[as.character(unique(df$ecoregion))]) +
  geom_sf(data = states[counties %>% filter(ecoregion %in% unique(df$ecoregion)),  ], 
          fill = NA, color = "black") + 
  theme_void() + 
  labs(fill = "Ecoregion") + 
  theme(legend.position = c(0.35,0.85),
    legend.text = element_text(size= 6),
    legend.title = element_text(size= 6),
    legend.key.size = unit(0.25, 'in')) + 
    guides(fill = guide_legend(ncol=2))

m

h <- ggplot(data = df) + geom_histogram(aes(x = c_rent/1e3, fill = as.character(ecoregion)),
                                        color = "black") + 
  facet_wrap(~ecoregion, labeller = as_labeller(ecoregion_labels),
             ncol = 2) +
  scale_fill_manual(values = ecoregion_colors[as.character(unique(df$ecoregion))], 
                    labels = ecoregion_labels[as.character(unique(df$ecoregion))]) +
  theme_minimal() +
  labs(x = "Rent (thousands USD per acre)", y = "No. of plots", fill = "Ecoregion") +
  theme(legend.position = "none")


combined <- m + h + 
              plot_layout(widths = c(3,2)) 
  # theme(legend.text = element_text(size= 4),
  #       legend.title = element_text(size= 4))

combined
savefig(path = 'results/misc/returns_heterogeneity/map_distributions', width = 1.1*6.5, height = 1.1*3.5)



ggplot(data = df %>% filter(c_rent != 0)) + 
  geom_density(aes(x = c_rent, group = ecoregion, color = as.character(ecoregion), 
                   linetype = as.character(ecoregion)),
               lwd = 1)+
  scale_x_continuous(limits = c(0,50)) +
  theme(legend.position = "bottom")

c_rent

