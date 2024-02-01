library(haven)
library(tidyverse)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # sets directory to the current directory
setwd('../../../..') # relative paths to move directory to the root project directory

# Actual conversions
ddc_data_urbancal <- read_dta("processing/combined/ddc_data_urbancal_dy.dta") #%>%
  #filter(is.na(CRP_nr)==FALSE, is.na(crop_nr)==FALSE) 

sum(distinct(ddc_data_urbancal,fips,year,lcc,initial_use,.keep_all=TRUE)$initial_acres, na.rm=TRUE)
sum(ddc_data_urbancal$final_acres, na.rm=TRUE)

initial <- ddc_data_urbancal %>%
  distinct(fips,year,lcc,initial_use,.keep_all=TRUE) %>%
  group_by(year, initial_use) %>%
  summarize(initial_acres = sum(initial_acres, na.rm=TRUE))

write_csv(initial, "processing/elasticity/initial_acres_all.csv")

initial_county <- ddc_data_urbancal %>%
  distinct(fips,year,lcc,initial_use,.keep_all=TRUE) %>%
  group_by(fips, year, initial_use) %>%
  summarize(initial_acres = sum(initial_acres, na.rm=TRUE))

write_csv(initial_county, "processing/elasticity/initial_acres_county_all.csv")

initial_county_lcc <- ddc_data_urbancal %>%
  distinct(fips,year,lcc,initial_use,.keep_all=TRUE) %>%
  group_by(fips, year, initial_use, lcc) %>%
  summarize(initial_acres = sum(initial_acres, na.rm=TRUE))

write_csv(initial_county_lcc, "processing/elasticity/initial_acres_county_lcc_all.csv")

# final <- ddc_data_urbancal %>%
#   group_by(year, final_use) %>%
#   summarize(final_acres = sum(final_acres))
# 
# conversions <- initial %>%
#   left_join(final, by=c('year')) %>%
#   mutate(conversions = final_acres - initial_acres,
#          conversions_pct = conversions/initial_acres) %>%
#   filter(initial_use == final_use)
# 
# conversions_alt <- ddc_data_urbancal %>%
#   filter(initial_use %in% c("Crop", "CRP"),
#          final_use %in% c("Crop", "CRP"),
#          initial_use != final_use,
#          final_acres != 0) %>%
#   group_by(year, initial_use, final_use) %>%
#   summarize(conversions = sum(final_acres)) %>%
#   left_join(initial, by=c("year", "initial_use")) %>%
#   mutate(conversions_pct = conversions/initial_acres)
# 
# # Predicted conversions (dynamic)
# implied_prob_cropcrp <- read_dta("processing/elasticity/implied_prob_cropcrp.dta")
# 
# dy_matrix <- matrix(c(0.99151324,0.31425713, 0.00848676, 0.68574287), ncol = 2)
# 
# initial_2015 <- matrix(as.vector(initial[initial$year==2015, "initial_acres"][[1]]), nrow=1)
# initial_2015
# 
# final_2015 <- initial_2015 %*% dy_matrix
# final_2015
# 
# pred_crop_crp <- dy_matrix[1,2]*initial[initial$year==2015, "initial_acres"][[1]][1]
# pred_crp_crop <- dy_matrix[2,1]*initial[initial$year==2015, "initial_acres"][[1]][2]
# 
# # Predicted conversions (static)
# implied_prob_cropcrp_st <- read_dta("processing/elasticity/implied_prob_cropcrp_static.dta")
# 
# st_matrix <- matrix(c(0.99943363, 0.00347376, 0.00056637, 0.99652624), ncol = 2)
# 
# final_2015_st <- initial_2015 %*% st_matrix
# final_2015_st
# 
# pred_crop_crp_st <- st_matrix[1,2]*initial[initial$year==2015, "initial_acres"][[1]][1]
# pred_crp_crop_st <- st_matrix[2,1]*initial[initial$year==2015, "initial_acres"][[1]][2]


