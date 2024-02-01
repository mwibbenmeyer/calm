####################################################
# Qinrui Xiahou
# May 3, 2022
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
setwd("../../../../")

savefig <- function(path,width,height) {
  lapply(c("png","pdf"), function(ftype) 
    ggplot2::ggsave(paste0(path,".",ftype),
                    device=ftype,width=width,height=height))
}

`%ni%` <- Negate(`%in%`)
options(tigris_use_cache = TRUE) #Tell tidycensus to cache shapefiles for future sessions

# Import data -------------------------------------------------------------

aea <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

#Import county shapefile using tidycensus - b19013_001 is arbitrarily chosen
states <- state.abb[state.abb %ni% c("AK","HI")]
counties <- get_acs(state = states, geography = "county", year = 2010, variables = "B19013_001", geometry = TRUE) %>%
  select(-c("variable","estimate","moe")) %>%
  st_transform(aea)

df <- read.csv("processing/simulation/predicted_probs/preds_2022-05-19_nolcc_st_southeast_newdata.csv") %>% 
        mutate(lcc = recode(lcc, "1_2" = "1-2",
                    "3_4" = "3-4",
                    "5_6" = "5-6",
                    "7_8" = "7-8"),
               fips = str_pad(fips, width = 5, side = "left", pad = "0"),
               stfips = substr(fips,1,2),
               acres = weighted_ccp*initial_acres,
               predicted_acres = phat*initial_acres)

full_data <- read_dta("processing/combined/ddc_data_urbancal_eco.dta")

# Map eco-regions -------------------------------------------------------------

plot_data <- counties  %>%
  mutate(GEOID = as.numeric(GEOID)) %>%
  left_join(full_data, by=c("GEOID"="fips")) %>%
  st_set_geometry("geometry")

plot_data_summary <- plot_data %>%
  ungroup() %>%
  # filter(initial_use != final_use,
  #        is.na(y_st)==FALSE,
  #        is.na(crop_nr)==FALSE,
  #        is.na(CRP_nr)==FALSE,
  #        is.na(forest_nr)==FALSE,
  #        is.na(urban_nr)==FALSE,
  #        is.na(other_nr)==FALSE,
  #        stateabbrev %in% c("AR", "AL", "FL", "GA", "KY", "LA", "MO", "MS", "NC", "SC", "TN")) %>% # is this necessary?
  group_by(year, P) %>%
  summarize(obs = sum(initial_use != final_use &
                      is.na(y_st)==FALSE &
                      is.na(crop_nr)==FALSE &
                      is.na(CRP_nr)==FALSE &
                      is.na(forest_nr)==FALSE &
                      is.na(urban_nr)==FALSE &
                      is.na(other_nr)==FALSE &
                      stateabbrev %in% c("AR", "AL", "FL", "GA", "KY", "LA", "MO", "MS", "NC", "SC", "TN"))) %>%
  as_tibble() %>%
  st_set_geometry("geometry")

plot_data_summary %>%
  filter(is.na(year)==FALSE) %>%
  mutate(obs = ifelse(obs==0, NA, obs)) %>%
  mutate(P = as.factor(P)) %>%
  ggplot() + 
  geom_sf(aes(fill = obs, group=P, color=P), color = "darkgray", lwd = 0) + 
  labs(fill = "Number of observations \nin the regressions") + 
  facet_wrap(vars(year), nrow = 3) + 
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
savefig(paste0(path,"obs_in_reg_se"), width = 8, height = 6)

# Make density plots ----------------------------------------------------------

df_long <- pivot_longer(df, cols = c("weighted_ccp","phat"), names_to = "variable") %>%
            mutate(variable = recode(variable,"phat" = "Predicted probability",
                                     "weighted_ccp" = "Conditional choice\nprobability"))

make_density <- function(initial, final, ub = 1, lb = 0) {
  
  p <- ggplot(data = df_long %>% filter(initial_use == initial,
                              final_use == final,
                              value <= ub,
                              value >= lb)) + 
             geom_density(aes(x = value, fill = variable),
                          alpha = 0.5) + 
           facet_wrap(~lcc, scales = "free") + 
          theme_minimal() + 
        labs(fill = "",
             x = "Probability",
             y = "Density")

  return(p)
}

make_density("Forest","Forest", ub = 1, lb = 0.995)
make_density("Crop","Crop", ub = 1, lb = 0.995)

make_density("Crop","Forest", ub = 0.0005, lb = 0)
make_density("Forest","Crop", ub = 0.00005, lb = 0)
make_density("Crop","Urban", ub = 0.005, lb = 0)
make_density("Forest","Urban", ub = 0.005, lb = 0)

hist(df %>% filter(initial_use == "Forest" & final_use == "Forest") %>% .$phat)

# Make density of difference in probability plots------------------------------

df$diff <- df$phat - df$weighted_ccp

make_diff_density <- function(initial, final, lb = -1, ub = 1) {
  
  p <- ggplot(data = df %>% filter(initial_use == initial,
                                        final_use == final,
                                   diff >= lb,
                                   diff <= ub)) + 
    geom_density(aes(x = diff), alpha = 0.5) + 
    facet_wrap(~lcc, scales = "free", 
               labeller = labeller(lcc = function(x) sprintf("LCC: %s",x))) + 
    theme_minimal() + 
    labs(fill = "",
         x = "Predicted probability - conditional choice probability",
         y = "Density",
         subtitle = sprintf("Transitions, %s to %s",initial,final))
  
  return(p)
}

make_diff_density("Forest","Forest", lb = -0.1, ub = 0.005)

median(df %>% filter(initial_use=="Forest", final_use == "Forest", lcc == "5-6") %>% .$diff, na.rm = TRUE)

# Make scatter plots ----------------------------------------------------------

df_state <- df %>% group_by(stfips,initial_use,final_use,lcc,year) %>% 
              summarize(weighted_ccp = mean(weighted_ccp, na.rm = TRUE),
                        phat = mean(phat, na.rm = TRUE))

make_scatter <- function(initial, final, zoom = FALSE, year = NA) {
  
  if (zoom == TRUE) {

    if (initial == final) {
      ub = 1
      lb = 0.95
    }
    else {
      ub = 0.005
      lb = 0
    }
      
  df_state <- df_state %>% filter(weighted_ccp >= lb,
                weighted_ccp <= ub,
                phat >= lb,
                phat <= ub)
  }
  
  if (!is.na(year)) df_state <- df_state %>% filter(year == year)
  
  p <- ggplot(data = df_state %>% filter(initial_use == initial,
                                        final_use == final)) + 
    geom_point(aes(x = weighted_ccp, y = phat),
                 alpha = 0.5) +
    geom_function(fun = function(x) x, color = "red") +
    facet_wrap(~lcc, scales = "free",
               labeller = labeller(lcc = function(x) sprintf("LCC: %s",x))) + 
    theme_minimal() + 
    labs(fill = "",
         x = "Conditional choice probability",
         y = "Predicted probability",
         subtitle = sprintf("Transitions, %s to %s",initial,final))
  
  return(p)
}

make_scatter("Forest","Forest",year =  2002, zoom = FALSE)
make_scatter("Crop","Crop",year =  2002, zoom = FALSE)

make_scatter("Crop","Forest",year =  2002, zoom = FALSE)
make_scatter("Forest","Crop",year =  2002, zoom = FALSE)
make_scatter("Crop","Urban",year =  2002, zoom = FALSE)
make_scatter("Forest","Urban",year =  2002, zoom = FALSE)

# Make acreage scatter plots ----------------------------------------------------------

make_scatter <- function(initial, final, zoom = FALSE, year = NA) {
  
  p <- ggplot(data = df %>% filter(initial_use == initial,
                                  final_use == final,
                                  year == year)) + 
    geom_point(aes(x = acres, y = predicted_acres),
               alpha = 0.5) +
    geom_function(fun = function(x) x, color = "red") +
    facet_wrap(~lcc, scales = "free",
               labeller = labeller(lcc = function(x) sprintf("LCC: %s",x))) + 
    theme_minimal() + 
    labs(fill = "",
         x = "Acres converted",
         y = "Predicted acres converted",
         subtitle = sprintf("Transitions, %s to %s",initial,final))
  
  return(p)
}

make_scatter("Forest","Crop",year =  2002, zoom = TRUE)

