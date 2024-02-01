####################################################
# Qinrui Xiahou
# March 28, 2022
# Net returns by LCC by year
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

## Set working directory to land-use 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../../../")

savefig <- function(path,width,height) {
  lapply(c("png","pdf"), function(ftype) 
    ggplot2::ggsave(paste0(path,".",ftype),
                    device=ftype,width=width,height=height))
}

`%ni%` <- Negate(`%in%`)

## Load the data
ddc_data_urbancal <- read_dta("processing/combined/ddc_data_urbancal.dta")
reg_data <- read_dta("processing/combined/reg_diagnostics.dta")
points <- read_dta("processing_output/countypanel_estimation_bal.dta") %>% as.data.table()
ccp <- read_csv("processing/ccp/ccps.csv")

## Summarize land acres by lcc
acres <- ddc_data_urbancal %>%
  group_by(final_use, year, lcc) %>%
  summarise(total_acres = sum(final_acres, na.rm=TRUE),
            count = n())

acres_reg <- reg_data %>%
  group_by(final_use, year, lcc) %>%
  summarise(total_acres = sum(final_acres, na.rm=TRUE),
            count = n())

## Compare the acres with points data
# Matt's version
df <- points[initial_use == final_use] %>%
  .[initial_use != "Rural" & final_use != "Rural"] %>% 
  .[initial_use == "Pasture" | initial_use == "Range", initial_use := "Other"] %>% #Recode pasture and range to "Other"
  .[ , .(total_acres = sum(acresk)), by = c("fips","lcc","initial_use","year")] %>%
  .[ , .(total_acres = sum(total_acres)), by = c("lcc","initial_use","year")] %>%
  .[ year >= 2002 & initial_use %ni% c("Federal","Water")] %>%
  .[ , lcc_acres := sum(total_acres), by = c("lcc","year")] %>%
  .[ , pctacres := total_acres/lcc_acres]

acres_points <- df %>%
  group_by(initial_use, year, lcc) %>%
  summarise(total_acres = sum(total_acres, na.rm=TRUE),
            count = n()) 

# Corrected Matt's version
df <- points %>% # shouldn't filter by initial_use == final_use because acresk is defined by land conversions, not by initial use in the point dataset
  .[initial_use %ni% c("Rural", "Federal", "Water", ".") & final_use %ni% c("Rural", "Federal", "Water", ".")] %>%  # not only rural
  .[initial_use == "Pasture" | initial_use == "Range", initial_use := "Other"] %>% 
  .[final_use == "Pasture" | final_use == "Range", final_use := "Other"] %>% # not only initial use, but also final use
  .[ , .(total_acres = sum(acresk)), by = c("fips","lcc","initial_use","year")] %>%
  .[ , .(total_acres = sum(total_acres)), by = c("lcc","initial_use","year")] %>%
  .[ year >= 2002 & initial_use %ni% c("Federal","Water")] %>%
  .[ , lcc_acres := sum(total_acres), by = c("lcc","year")] %>%
  .[ , pctacres := total_acres/lcc_acres]

acres_points_corrected <- df %>%
  group_by(initial_use, year, lcc) %>%
  summarise(total_acres = sum(total_acres, na.rm=TRUE),
            count = n()) # this matches with the "clean" version below

# Clean version
acres_points2 <- points %>%
  mutate(initial_use = case_when(initial_use == "Pasture" | initial_use == "Range" ~ "Other",
                                 TRUE ~ initial_use),
         final_use = case_when(final_use == "Pasture" | final_use == "Range" ~ "Other",
                               TRUE ~ final_use)) %>%
  filter(initial_use %in% c("Crop","Forest","Urban","CRP","Other") & final_use %in% c("Crop","Forest","Urban","CRP","Other")) %>%
  group_by(initial_use, year, lcc) %>%
  summarise(total_acres = sum(acresk, na.rm=TRUE),
            count = n()) # this is the correct version

# The following two versions are wrong but consistent due to the mistakes made in the CCP data
acres_ddc <- ddc_data_urbancal %>%
  filter(initial_use == final_use) %>%
  group_by(initial_use, year, lcc) %>%
  #distinct(initial_use, year, lcc, fips, .keep_all = TRUE) %>%
  summarise(total_acres = sum(initial_acres, na.rm=TRUE),
            count = n())

acres_ccp <- ccp  %>%
  filter(initial_use == final_use) %>%
  group_by(initial_use, year, lcc) %>%
  distinct(initial_use, year, lcc, fips, .keep_all = TRUE) %>%
  summarise(total_acres = sum(initial_acres, na.rm=TRUE),
            count = n()) # the same as acres_ddc

## Count the number of observations by year
obs <- ddc_data_urbancal %>%
  filter(initial_use != final_use) %>%
  group_by(initial_use, final_use, year, lcc) %>%
  summarise(count = n())

obs_reg <- reg_data %>%
  group_by(initial_use, final_use, year, lcc) %>%
  summarise(count = n())

temp <- ddc_data_urbancal %>%
  filter(fips == 8014)

## Plot the distribution of net returns by land use for 2002 and 2007
annotations <- reg_data %>%
  group_by(year) %>%
  summarize(avg_crop_nr = mean(crop_nr, na.rm=TRUE),
            label_crop_nr = paste("mean_crop_nr = ", round(avg_crop_nr,2), sep=""),
            avg_CRP_nr = mean(CRP_nr, na.rm=TRUE),
            label_CRP_nr = paste("mean_CRP_nr = ", round(avg_CRP_nr,2), sep=""),
            avg_forest_nr = mean(forest_nr, na.rm=TRUE),
            label_forest_nr = paste("mean_forest_nr = ", round(avg_forest_nr,2), sep=""),
            avg_urban_nr = mean(urban_nr, na.rm=TRUE),
            label_urban_nr = paste("mean_urban_nr = ", round(avg_urban_nr,2), sep=""),
            avg_other_nr = mean(other_nr, na.rm=TRUE),
            label_other_nr = paste("mean_other_nr = ", round(avg_other_nr,2), sep="")) 

# Crop
reg_data %>%
  ggplot(aes(x=crop_nr)) +
  geom_histogram(aes(y=..density..),
                 alpha=0.3, color="black", fill="white") +
  geom_density(alpha=0.2, fill="red") +
  facet_wrap(~year) +
  geom_vline(data = annotations, aes(xintercept = avg_crop_nr), color="blue", linetype="dashed") +
  geom_text(data = annotations, x=200, y=0.01, label=annotations$label_crop_nr) 

# CRP
reg_data %>%
  ggplot(aes(x=CRP_nr)) +
  geom_histogram(aes(y=..density..),
                 alpha=0.3, color="black", fill="white") +
  geom_density(alpha=0.2, fill="red") +
  facet_wrap(~year) +
  geom_vline(data = annotations, aes(xintercept = avg_CRP_nr), color="blue", linetype="dashed") +
  geom_text(data = annotations, x=500, y=0.01, label=annotations$label_CRP_nr) 

# Forest
reg_data %>%
  ggplot(aes(x=forest_nr)) +
  geom_histogram(aes(y=..density..),
                 alpha=0.3, color="black", fill="white") +
  geom_density(alpha=0.2, fill="red") +
  facet_wrap(~year) +
  geom_vline(data = annotations, aes(xintercept = avg_forest_nr), color="blue", linetype="dashed") +
  geom_text(data = annotations, x=100, y=0.02, label=annotations$label_forest_nr) 

# Urban
reg_data %>%
  ggplot(aes(x=urban_nr)) +
  geom_histogram(aes(y=..density..),
                 alpha=0.3, color="black", fill="white") +
  geom_density(alpha=0.2, fill="red") +
  facet_wrap(~year) +
  geom_vline(data = annotations, aes(xintercept = avg_urban_nr), color="blue", linetype="dashed") +
  geom_text(data = annotations, x=25000, y=0.0001, label=annotations$label_urban_nr) 

# Other
reg_data %>%
  ggplot(aes(x=other_nr)) +
  geom_histogram(aes(y=..density..),
                 alpha=0.3, color="black", fill="white") +
  geom_density(alpha=0.2, fill="red") +
  facet_wrap(~year) +
  geom_vline(data = annotations, aes(xintercept = avg_other_nr), color="blue", linetype="dashed") +
  geom_text(data = annotations, x=25, y=0.1, label=annotations$label_other_nr) 

## Plot the average CCPs by year by land use conversions
annotations2 <- reg_data %>%
  group_by(year, initial_use, final_use) %>%
  summarize(avg_dy_y = mean(y_dy, na.rm=TRUE),
            label_dy_y = paste("mean_dy_y = ", round(avg_dy_y,2), sep=""),
            avg_st_y = mean(y_st, na.rm=TRUE),
            label_st_y = paste("mean_st_y = ", round(avg_st_y,2), sep=""),
            obs = n(),
            label_obs = paste("#obs = ", round(obs, 0), sep="")) 

# 2002 dynamic
reg_data %>%
  filter(year == 2002) %>%
  ggplot(aes(x=y_dy)) +
  geom_histogram(aes(y=..density..),
                 alpha=0.3, color="black", fill="white") +
  geom_density(alpha=0.2, fill="red") +
  facet_grid(initial_use ~ final_use) +
  geom_vline(data = filter(annotations2,year==2002), aes(xintercept = avg_dy_y), color="blue", linetype="dashed") +
  geom_text(data = filter(annotations2,year==2002), x=0, y=0.3, label=paste(filter(annotations2,year==2002)$label_dy_y, "\n", filter(annotations2,year==2002)$label_obs)) 

# 2002 static
reg_data %>%
  filter(year == 2002) %>%
  ggplot(aes(x=y_st)) +
  geom_histogram(aes(y=..density..),
                 alpha=0.3, color="black", fill="white") +
  geom_density(alpha=0.2, fill="red") +
  facet_grid(initial_use ~ final_use) +
  geom_vline(data = filter(annotations2,year==2002), aes(xintercept = avg_st_y), color="blue", linetype="dashed") +
  geom_text(data = filter(annotations2,year==2002), x=0, y=0.25, label=paste(filter(annotations2,year==2002)$label_st_y, "\n", filter(annotations2,year==2002)$label_obs)) 

# 2007 dynamic
reg_data %>%
  filter(year == 2007) %>%
  ggplot(aes(x=y_dy)) +
  geom_histogram(aes(y=..density..),
                 alpha=0.3, color="black", fill="white") +
  geom_density(alpha=0.2, fill="red") +
  facet_grid(initial_use ~ final_use) +
  geom_vline(data = filter(annotations2,year==2007), aes(xintercept = avg_dy_y), color="blue", linetype="dashed") +
  geom_text(data = filter(annotations2,year==2007), x=0, y=0.3, label=paste(filter(annotations2,year==2007)$label_dy_y, "\n", filter(annotations2,year==2007)$label_obs)) 

# 2007 static
reg_data %>%
  filter(year == 2007) %>%
  ggplot(aes(x=y_st)) +
  geom_histogram(aes(y=..density..),
                 alpha=0.3, color="black", fill="white") +
  geom_density(alpha=0.2, fill="red") +
  facet_grid(initial_use ~ final_use) +
  geom_vline(data = filter(annotations2,year==2007), aes(xintercept = avg_st_y), color="blue", linetype="dashed") +
  geom_text(data = filter(annotations2,year==2007), x=0, y=0.2, label=paste(filter(annotations2,year==2007)$label_st_y, "\n", filter(annotations2,year==2007)$label_obs)) 

## Map the number of observations by year
#Import county shapefile using tidycensus - b19013_001 is arbitrarily chosen
states <- state.abb[!state.abb %in% c("AK","HI")]
aea <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
counties <- get_acs(state = states, geography = "county", year = 2010, variables = "B19013_001", geometry = TRUE) %>%
  select(-c("variable","estimate","moe")) %>%
  st_transform(aea)

# Map1: number of observations (conversions) by county by year
df <- counties  %>%
  mutate(year = 2002) %>%
  bind_rows(mutate(counties, year=2007)) %>% # make sure no county is dropped while merging by year
  mutate(GEOID = as.numeric(GEOID)) %>%
  left_join(reg_data, by=c("GEOID"="fips", "year")) %>%
  st_set_geometry("geometry")

df_summary <- df %>%
  ungroup() %>%
  group_by(year, GEOID, geometry) %>%
  summarize(obs = sum(is.na(crop_nr)==FALSE)) %>%
  as_tibble() %>%
  st_set_geometry("geometry")

df_summary %>%
  mutate(obs = ifelse(obs==0, NA, obs)) %>%
  ggplot() + 
  geom_sf(aes(fill = obs), color = "darkgray", lwd = 0) + 
  labs(fill = "Number of observations \nin the regressions") + 
  facet_wrap(vars(year), nrow = 2) + 
  scale_fill_distiller(na.value="white", trans = "reverse") + 
  guides(fill = guide_colorbar(barwidth = 12)) + 
  theme_minimal() +
  theme(legend.position="bottom", legend.box = "horizontal") + 
  theme(panel.border = element_blank(),
        panel.grid = element_blank(),
        # panel.grid=element_line(color="red"),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank()) 

path = "results/initial_descriptives/implied_probabilities/"
dir.create(path, recursive = TRUE, showWarnings = FALSE)
savefig(paste0(path,"obs_in_reg"), width = 8, height = 12.5)

# Map2: number of years available by county
df_summary2 <- df_summary %>%
  as.data.frame() %>%
  ungroup() %>%
  pivot_wider(names_from = "year", values_from = "obs") %>%
  mutate(years_available = factor(case_when(`2002`!=0 & `2007`!=0 ~ "2002 + 2007",
                                     `2002`==0 & `2007`!=0 ~ "Only 2007",
                                     `2002`!=0 & `2007`==0 ~ "Only 2002",
                                     `2002`==0 & `2007`==0 ~ "Neither"),
                                  order=TRUE,
                                  levels=c("2002 + 2007", "Only 2007", "Only 2002", "Neither"))) %>%
  as_tibble() %>%
  st_set_geometry("geometry")

df_summary2 %>%
  ggplot() + 
  geom_sf(aes(fill = years_available), color = "darkgray", lwd = 0) + 
  labs(fill = "Years available \nin the regressions") + 
  scale_colour_brewer(palette = "PuBu")	+
  #guides(fill = guide_colorbar(barwidth = 12)) + 
  theme_minimal() +
  theme(legend.position="bottom", legend.box = "horizontal") + 
  theme(panel.border = element_blank(),
        panel.grid = element_blank(),
        # panel.grid=element_line(color="red"),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank()) 

path = "results/initial_descriptives/implied_probabilities/"
dir.create(path, recursive = TRUE, showWarnings = FALSE)
savefig(paste0(path,"years_in_reg"), width = 8, height = 4)
