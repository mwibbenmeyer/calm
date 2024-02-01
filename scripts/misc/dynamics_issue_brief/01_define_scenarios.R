if (!require("pacman")) install.packages("pacman")
devtools::install_github("robin-a-young/RStata")

pacman::p_load(tidyverse,
               dplyr,
               readxl,
               sf,
               tidycensus,
               haven,
               stringr,
               data.table,
               RStata,
               SimDesign, #has quiet function
               cdlTools,
               properties)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # sets directory to the current directory
setwd('../../../') # relative paths to move directory to the root project directory


# Urban scenario ----------------------------------------------------------

pop.change <- read_xlsx("raw_data/misc/dynamics_issue_brief/us_census_2017_population_projections.xlsx",
                        range = "A9:B53",
                        col_names = F) %>% 
                rbind(., c(2015, 320700))
colnames(pop.change) <- c("year","population")
pop.change <- pop.change %>% 
                arrange(year) %>% 
                mutate(year5 = round((year-2.4)/5)*5) %>% 
                filter(year == year5) %>% 
                mutate(pop.change = population - lag(population),
                       inc.urban.acres = pop.change/4.5)

dst <- "processing/misc/dynamics_issue_brief/"
dir.create(dst, recursive = T, showWarnings = F)
write.csv(pop.change, paste0(dst,"urban_scenario.csv"))

# Forest scenario ----------------------------------------------------------

forest.change <- data.frame(year = seq(2015,2065,5)) %>% 
                  mutate(inc.forest.acres = ifelse(year == 2025 | year == 2030, 15000, NA))

write.csv(forest.change, paste0(dst,"forest_scenario.csv"))

aggressive.forest.change <- data.frame(year = seq(2015,2065,5)) %>% 
  mutate(inc.forest.acres = ifelse(year > 2020 & year <= 2050, 15000, NA))

write.csv(aggressive.forest.change, paste0(dst,"aggressive_forest_scenario.csv"))

