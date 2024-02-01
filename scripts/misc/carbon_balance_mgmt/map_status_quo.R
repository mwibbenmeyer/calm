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

`%ni%` <- Negate(`%in%`)

savefig <- function(path,width,height) {
  lapply(c("png","pdf"), function(ftype) 
    ggplot2::ggsave(paste0(path,".",ftype),
                    device=ftype,width=width,height=height))
}

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # sets directory to the current directory
setwd('../../../') # relative paths to move directory to the root project directory

aea <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

#Import county shapefile using tidycensus - b19013_001 is arbitrarily chosen
states <- state.abb[state.abb %ni% c("AK","HI")]
counties <- get_acs(state = states, geography = "county", year = 2010, variables = "B19013_001", geometry = TRUE) %>%
  select(-c("variable","estimate","moe")) %>%
  st_transform(aea) %>% 
  st_make_valid()

init <- read.csv("processing/misc/dynamics_issue_brief/simulations/base/acreages.csv") %>% 
  mutate(fips = ifelse(fips == "46102","46113",fips)) %>% #Change Shannon County, SD to Oglala Lakota County, SD 
  mutate(fips = str_pad(fips, width = 5, side = "left", pad = "0")) %>% 
  merge(counties, by.x = "fips", by.y = "GEOID")

case_1 <- read.csv("processing/misc/dynamics_issue_brief/results/status_quo/carbon_model_input/acreages_10.csv") %>%   mutate(fips = ifelse(fips == "46102","46113",fips)) %>% #Change Shannon County, SD to Oglala Lakota County, SD 
  mutate(fips = str_pad(fips, width = 5, side = "left", pad = "0")) %>% 
  merge(counties, by.x = "fips", by.y = "GEOID")          

df <- merge(init, case_1 %>% dplyr::select(fips, initial_use, acres), 
            by = c('fips','initial_use'), 
            suffixes = c(".old",".case1")) %>%
        group_by(fips) %>% 
        mutate(total.acres = sum(acres.old, na.rm = T)) %>% 
        mutate(pct.change = 100*(acres.case1/total.acres - acres.old/total.acres)) %>% 
        mutate(initial_use = recode(initial_use, "Urban" = "Settlements"),
               initial_use = factor(initial_use, levels = c("Crop","Forest","Settlements","Other"))) %>% 
        st_as_sf() 

map <- ggplot(data = df) + geom_sf(aes(fill = ifelse(abs(pct.change) <= 50, pct.change, sign(pct.change)*50)),
                            color = NA) + 
  facet_wrap(~initial_use, ncol = 2) +
  scale_fill_gradient2(limits = c(-50,50), low = rffred, high = rffblue, mid = "grey97") +
  labs(fill = "Change in percent\nland use") +
  theme_void() +
  theme(legend.position="bottom")

map

x <- read.csv("processing/misc/carbon_balance/lu_summary_status_quo.csv") %>% as.data.table()
x_total<-x[,lapply(.SD,sum),by=list(initial_use,year),.SDcols=c('acres')] %>% 
          mutate(initial_use = recode(initial_use, "Urban" = "Settlements"),
            initial_use = factor(initial_use, levels = c("Crop","Forest","Settlements","Other")))

pl <-ggplot(x_total, aes(x = year, y =  acres/(1e3*2.47105),colour= initial_use)) +
  geom_line(size=.8)+ theme_bw(base_size = 12) +
  labs(title='',y='ha(millions)',x='year',color='Land use') + 
  theme(legend.position="bottom") +
  scale_colour_manual(values = c(rffblack,rffgreen,rffblue,rffred)) +
  guides(colour=guide_legend(nrow=2,byrow=TRUE))

map + pl + plot_layout(widths = c(2.5, 1)) +
  plot_annotation(tag_levels = 'A') &
  theme(plot.tag = element_text(size = 24))

savefig(path = "results/misc/carbon_balance_mgmt/land_use_change", width = 6.5*1.75, height = 4*1.75)
