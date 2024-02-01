####################################################
# Matt Wibbenmeyer
# January 31, 2022
# Transition table
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

df <- points[initial_use != "Rural" & final_use != "Rural"] %>% 
      .[initial_use == "Pasture" | initial_use == "Range", initial_use := "Other"] %>% #Recode pasture and range to "Other"
      .[final_use == "Pasture" | final_use == "Range", final_use := "Other"] %>% #Recode pasture and range to "Other"
      .[ , .(acresk = sum(acresk)), by = c("fips","lcc","initial_use","final_use","year")] %>%
      .[initial_use %ni% c("Federal","Water") & final_use %ni% c("Federal","Water")]
  
df2 <- df[ , .(acresk = sum(acresk)), by = c("initial_use","final_use","year")] %>%
        merge(df[ , .(total_acres = sum(acresk)), by = c("initial_use","year")]) %>% 
        .[ , pctchange := 100*acresk/total_acres]

make_panel <- function(y) {
  
  table <- df2[year == y & initial_use != "Rural" & final_use != "Rural", c("initial_use","final_use","pctchange")] %>%
            spread(key = "final_use", value = "pctchange") %>% 
            mutate_if(is.numeric, function(x) as.character(formatC(x, digits = 1, format = "f", big.mark = ",")))
  
  dst = 'results/summary_stats/transitions_table/'
  dir.create(dst, recursive = TRUE, showWarnings = FALSE)
  write.table(table, file = paste0(dst,sprintf("panel_%s.tex",y)),
              quote = FALSE, col.names = TRUE, row.names = FALSE,
              sep = '&', eol = '\\\\ \n')
  
  return(table)
  
  }

panel1997 <- make_panel(1997)
panel2002 <- make_panel(2002)
panel2007 <- make_panel(2007)
panel2012 <- make_panel(2012)