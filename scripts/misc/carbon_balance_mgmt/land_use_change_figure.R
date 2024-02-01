####################################################
# Matt Wibbenmeyer
# July 27, 2021
# Map simulation results
####################################################

#devtools::install_github("kwstat/pals")   

## Load/install packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse,
               dplyr,
               readxl,
               sf,
               tidycensus,
               haven,
               stringr,
               data.table,
               RStata,
               cdlTools,
               nngeo,
               colorspace,
               pals,
               patchwork)


`%ni%` <- Negate(`%in%`)

savefig <- function(filename, dst ,width,height) {
  
  dir.create(dst, showWarnings = F, recursive = T)
  
  lapply(c("png","pdf"), function(ftype) 
    ggplot2::ggsave(paste0(dst, filename,".",ftype),
                    device=ftype,width=width,height=height))
}

# colors
rffblue <- "#88C4F4"
rffred <- "#FF6663"
rffgreen <- "#50B161"
rffbrown <- "#74645E"
rffblack <- "#04273C"
rffpurple <- "#755EA6"
rffyellow <- "#EAD367"
rfforange <- "#F4A25F"
rffgrey <- "#96A4AD"


setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # sets directory to the current directory
setwd('../../') # relative paths to move directory to the root project directory

aea <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

# Import data -------------------------------------------------------------

#Import county shapefile using tidycensus - b19013_001 is arbitrarily chosen
states <- state.abb[state.abb %ni% c("AK","HI")]
counties <- get_acs(state = states, geography = "county", year = 2010, variables = "B19013_001", geometry = TRUE) %>%
  select(-c("variable","estimate","moe")) %>%
  mutate(fips = str_pad(GEOID, width = 5, pad = "0", side = "left")) %>% 
  st_transform(aea) %>% 
  st_make_valid()
ecoregions <- read.csv("raw_data/misc/ecoregions/interpolated_ecocd_counties.csv") %>% 
  mutate(fips = str_pad(fips, width = 5, pad = "0", side = "left")) %>% 
  as.data.table()
ecoregions_sf <- merge(counties, ecoregions %>% select(fips, ecoregion), by = 'fips') %>% 
  group_by(ecoregion) %>%
  summarize(num.counties = sum(1))

points <- read_dta("processing/pointpanel/countypanel_estimation_bal.dta") %>% as.data.table()

hist <- points[initial_use != "Rural" & final_use != "Rural"] %>% 
  .[initial_use %ni% c("Federal","Water") & final_use %ni% c("Federal","Water")] %>% 
  .[ , final_use := ifelse(final_use %in% c("Pasture","Range","CRP"), "Other", final_use)] %>%
  .[ , .(acres = sum(acresk)), by = c("fips","final_use","year")] %>%
  .[year != 2015] %>% 
  .[year %in% seq(1982,2012,10)]

project.path <- "processing/misc/dynamics_issue_brief/"

sq <- do.call(rbind, lapply(seq(1,10), function(int)
  read.csv(sprintf("%ssimulations/status_quo/carbon_model_input/acreages_%s.csv", project.path, int)) %>% 
    mutate(year = 2012+int*5) %>% 
    mutate(fips = ifelse(fips == "46102","46113",fips)) %>% #Change Shannon County, SD to Oglala Lakota County, SD 
    mutate(fips = str_pad(fips, width = 5, side = "left", pad = "0")) %>% 
    group_by(year, fips, initial_use) %>% 
    dplyr::summarize(acres = sum(acres, na.rm = T)) %>% 
    dplyr::rename(final_use = initial_use) %>% 
    filter(year %in% seq(2022,2062,10)))) %>% 
  rbind(hist) %>% 
  as.data.table()

forest <- do.call(rbind, lapply(seq(1,10), function(int)
  read.csv(sprintf("%ssimulations/forest_increase_2/carbon_model_input/acreages_%s.csv", project.path, int)) %>% 
    mutate(year = 2012+int*5) %>% 
    mutate(fips = ifelse(fips == "46102","46113",fips)) %>% #Change Shannon County, SD to Oglala Lakota County, SD 
    mutate(fips = str_pad(fips, width = 5, side = "left", pad = "0")) %>% 
    group_by(year, fips, initial_use) %>% 
    dplyr::summarize(acres = sum(acres, na.rm = T)) %>% 
    dplyr::rename(final_use = initial_use) %>% 
    filter(year %in% seq(2022,2062,10)))) %>% 
  rbind(hist) %>% 
  as.data.table()

agg.forest <- do.call(rbind, lapply(seq(1,10), function(int)
  read.csv(sprintf("%ssimulations/aggressive_forest_increase/carbon_model_input/acreages_%s.csv", project.path, int)) %>% 
    mutate(year = 2012+int*5) %>% 
    mutate(fips = ifelse(fips == "46102","46113",fips)) %>% #Change Shannon County, SD to Oglala Lakota County, SD 
    mutate(fips = str_pad(fips, width = 5, side = "left", pad = "0")) %>% 
    group_by(year, fips, initial_use) %>% 
    dplyr::summarize(acres = sum(acres, na.rm = T)) %>% 
    dplyr::rename(final_use = initial_use) %>% 
    filter(year %in% seq(2022,2062,10)))) %>% 
  rbind(hist) %>% 
  as.data.table()

urban <- do.call(rbind, lapply(seq(1,9), function(int)
  read.csv(sprintf("%ssimulations/urbanization_decline/carbon_model_input/acreages_%s.csv", project.path, int)) %>% 
    mutate(year = 2012+int*5) %>% 
    mutate(fips = ifelse(fips == "46102","46113",fips)) %>% #Change Shannon County, SD to Oglala Lakota County, SD 
    mutate(fips = str_pad(fips, width = 5, side = "left", pad = "0")) %>% 
    group_by(year, fips, initial_use) %>% 
    dplyr::summarize(acres = sum(acres, na.rm = T)) %>% 
    dplyr::rename(final_use = initial_use) %>% 
    filter(year %in% seq(2022,2062,10)))) %>% 
  rbind(hist) %>% 
  as.data.table()



# Plotting functions ------------------------------------------------------

conversion_rates <- function(dataframe, county, yr) {
  
  last_yr = yr-10
  
  df.init <- dataframe[fips == county & year == last_yr] %>% 
    .[ , .(acres = sum(acres, na.rm =T)) , by = c("final_use")]
  
  df.new <- dataframe[fips == county & year == yr] %>% 
    .[ , .(acres = sum(acres, na.rm = T)) , by = c("final_use")] 
  
  df.total.init <- dataframe[fips == county & year == last_yr] %>% 
    .[ , .(total_acres = sum(acres, na.rm = T))]
  df.total.new <- dataframe[fips == county & year == yr] %>% 
    .[ , .(total_acres = sum(acres, na.rm = T))]
  
  df2 <- merge(df.init, df.new, by = "final_use", suffixes = c("_1","_2")) %>% 
    dplyr::mutate(total_acres_init = df.total.init[[1]],
                  total_acres_new = df.total.new[[1]],
                  change = acres_2/total_acres_new - acres_1/total_acres_init,
                  year = yr,
                  fips = county) %>%
    dplyr::select(final_use, fips, year, change)
  
  return(df2)
  
}

overall_conv_rate <- function(dataframe, county) {
  
  df.init <- dataframe[fips == county & year == 2012] %>% 
    .[ , .(acres = sum(acres, na.rm =T)) , by = c("final_use")]
  
  df.new <- dataframe[fips == county & year == 2052] %>% 
    .[ , .(acres = sum(acres, na.rm = T)) , by = c("final_use")] 
  
  df.total.init <- dataframe[fips == county & year == 2012] %>% 
    .[ , .(total_acres = sum(acres, na.rm = T))]
  df.total.new <- dataframe[fips == county & year == 2052] %>% 
    .[ , .(total_acres = sum(acres, na.rm = T))]
  
  df2 <- merge(df.init, df.new, by = "final_use", suffixes = c("_1","_2")) %>% 
    mutate(total_acres_init = df.total.init[[1]],
           total_acres_new = df.total.new[[1]],
           change = acres_2/total_acres_new - acres_1/total_acres_init,
           year = 2052,
           fips = county) %>%
    dplyr::select(final_use, fips, year, change)
  
  return(df2)
  
}


create_change_df <- function(scen.name) {
  
  df <- get(scen.name)
  
  cr <- do.call(rbind, lapply(unique(df$fips), function(e) 
    do.call(rbind, lapply(seq(1992,2062,10), function(y) conversion_rates(df, e, y)))))
  
  return(cr)
  
}

create_overall_change_df <- function(scen.name) {
  
  df <- get(scen.name)
  
  cr <- do.call(rbind, lapply(unique(df$fips), function(e) overall_conv_rate(df, e)))
  
  return(cr)
  
}



# Plotting functions ------------------------------------------------------

plot_pct_change <- function(scenario, save = F, dst, filename, max = NA) {
  
  dataframe <- create_change_df(scenario)
  
  reg.change <- dataframe
  
  reg.change <- merge(dataframe, counties, by.x = 'fips', by.y = 'GEOID') %>% 
    st_as_sf()
  
  if (!is.na(max)) reg.change <- reg.change %>% 
    mutate(change = 100*change) %>% 
    mutate(change = ifelse(abs(change) > max, max*sign(change), change))
  
  p <- ggplot(data = reg.change) + 
    geom_sf(aes(fill = change), color = NA) +
    scale_fill_continuous_diverging(h1 = 187, h2 = 339, c1 = 100, l1 = 25, l2 = 95, rev = TRUE) + 
    facet_grid(year~final_use) + 
    theme_void() + 
    labs(fill = "Percent change in\n ecoregion area")
  
  if (!is.na(max)) {
    p <- p + labs(caption = sprintf("Note: Changes are top-coded at an absolute value of %s for visualization.",max))
  }
  
  savefig(filename = filename, dst = dst, width = 6.5*1.75, height = 4.5*1.75)
  
  return(p)
  
}

plot_diff_pct_change <- function(scenario, save = F, dst = NA, filename = NA, max = NA) {
  
  dataframe <- create_change_df(scenario)
  sq.dataframe <- create_change_df('sq')
  
  dataframe <- merge(dataframe,sq.dataframe, by = c("final_use","year"), suffixes = c(".scen",".sq"))
  dataframe$diff_change <- dataframe$change.scen - dataframe$change.sq
  
  # reg.change <- merge(ecoregions_sf, dataframe, by = 'ecoregion')
  reg.change <- data.frame
  
  if (!is.na(max)) reg.change <- reg.change %>% 
    mutate(diff_change = 100*diff_change) %>% 
    mutate(diff_change = ifelse(abs(diff_change) > max, max*sign(diff_change), diff_change))
  
  p <- ggplot(data = reg.change %>% filter(year > 2012)) + 
    geom_sf(aes(fill = diff_change), color = NA) +
    scale_fill_continuous_diverging(h1 = 187, h2 = 339, c1 = 100, l1 = 25, l2 = 95, rev = TRUE) + 
    facet_grid(year~final_use) + 
    theme_void() + 
    labs(fill = "Difference in\npercent change")
  
  if (!is.na(max)) {
    p <- p + labs(caption = sprintf("Note: Changes are top-coded at an absolute value of %s for visualization.",max))
  }
  
  if (save == T) savefig(filename = filename, dst = dst, width = 6.5*1.75, height = 4*1.75)
  
  return(p)
  
}

plot_overall_diff_change <- function(save = F, dst, filename, max = NA, max.sq = NA) {
  
  forest.dataframe <- create_overall_change_df('forest')
  aggforest.dataframe <- create_overall_change_df('agg.forest')
  sq.dataframe <- create_overall_change_df('sq')
  
  dataframe <- merge(forest.dataframe,sq.dataframe, by = c("final_use","fips"), suffixes = c(".forest",".sq"))
  dataframe <- merge(dataframe, aggforest.dataframe, by = c("final_use","fips"), suffixes = c("", ".aggforest"))
  
  dataframe$forest_diff_change <- (dataframe$change.forest - dataframe$change.sq)*100
  dataframe$aggforest_diff_change <- (dataframe$change - dataframe$change.sq)*100
  dataframe$change.sq <- (dataframe$change.sq)*100
  
  reg.change <- merge(counties, dataframe, by = 'fips')
  
  if (!is.na(max)) reg.change <- reg.change %>% 
    mutate(forest_diff_change = ifelse(abs(forest_diff_change) > max, max*sign(forest_diff_change), forest_diff_change),
           urban_diff_change = ifelse(abs(aggforest_diff_change) > max, max*sign(aggforest_diff_change), aggforest_diff_change),
           change.sq = ifelse(abs(change.sq) > max.sq, max.sq*sign(change.sq), change.sq))
  
  
  p1 <- ggplot(data = reg.change) + 
    geom_sf(aes(fill = change.sq), color = NA) +
    # scale_fill_continuous_diverging(h1 = 187, h2 = 339, c1 = 100, l1 = 25, l2 = 95, rev = TRUE) + 
    scale_fill_gradient2(low = rffred,  high = rffblue, mid = lighten("#e0e4e7",0.75),
                         limits = c(-max,max)) + 
    facet_wrap(~final_use, nrow = 1) + 
    theme_void() + 
    labs(fill = "Change in percent\nland use")
  
  p2 <- ggplot(data = reg.change) + 
    geom_sf(aes(fill = forest_diff_change), color = NA) +
    # scale_fill_continuous_diverging(h1 = 187, h2 = 339, c1 = 100, l1 = 25, l2 = 95, rev = TRUE,
    #                                 limits = c(-max,max)) + 
    facet_wrap(~final_use, nrow = 1) + 
    theme_void() + 
    labs(fill = "Difference in percent\nland use relative\nto status quo")
  
  p3 <- ggplot(data = reg.change) + 
    geom_sf(aes(fill = aggforest_diff_change), color = NA) +
    # scale_fill_continuous_diverging(h1 = 187, h2 = 339, c1 = 100, l1 = 25, l2 = 95, rev = TRUE,
    #                                 limits = c(-max,max)) + 
    scale_fill_gradient2(low = rffred,  high = rffblue, mid = lighten("#e0e4e7",0.75),
                         limits = c(-max,max)) + 
    facet_wrap(~final_use, nrow = 1) + 
    theme_void() + 
    labs(fill = "Difference in percent\nland use relative\nto status quo")
  
  
  p <- (p1 + labs(subtitle = "Status Quo") + plot_layout(guides = "keep")) / 
    ((p2 + labs(subtitle = "Scenario: Forest Increase")) / 
       (p3 + labs(subtitle = "Scenario: Aggressive Forest Increase")) + 
       plot_layout(guides = "collect") & 
       # scale_colour_continuous(limits = range(c(-max,max))) & 
       theme(legend.position = 'right')) + 
    plot_layout(heights = c(1,2.5)) +
    plot_annotation(tag_levels = 'A')
  # guides(fill = guide_colourbar(barwidth = 10, barheight = 1))
  
  
  if (!is.na(max)) {
    p <- p + labs(caption = sprintf("Note: Changes in panel A are top-coded at an absolute value of %s for visualization.
                                    Changes in panels B and C are top-coded at an absolute value %s.",max.sq, max))
  }
  
  if (save == T) savefig(filename = filename, dst = dst, width = 6.5*1.75, height = 4*1.75)
  
  return(p)
  
}




plot_diff_pct_change("urban", save = F)

plot_pct_change("sq", save = T,  max = 5, dst = "results/misc/dynamics_issue_brief/",
                filename = "status_quo_change_pct_use")
plot_pct_change("forest", save = T,  max = 5, dst = "results/misc/dynamics_issue_brief/",
                filename = "forest_change_pct_use")
plot_pct_change("urban", save = T,  max = 5, dst = "results/misc/dynamics_issue_brief/",
                filename = "urban_change_pct_use")

plot_diff_pct_change("forest", save = T, dst = "results/misc/dynamics_issue_brief/",
                     filename = "forest_diff_pct_change", max = 1)
plot_diff_pct_change("urban", save = T, dst = "results/misc/dynamics_issue_brief/",
                     filename = "urban_diff_pct_change")

plot_overall_diff_change(dst = "results/misc/dynamics_issue_brief/",
                         filename = "overall_diff_pct_change", max = 5, max.sq = 50, save = T)

