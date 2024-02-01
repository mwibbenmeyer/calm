library(haven)
library(tidyverse)
library(censusapi)
library(readr)
library(stringr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../../../../")


df_dave = read_dta('processing/combined/ddc_data_dave.dta') 

df_dave_small = df_dave %>% select(fips, year, pop_den, pipc, lcc, initial_use, final_use)

df = read_dta('processing/combined/ddc_data_urbancal_crprev_urbannrsub.dta')

df_years = filter(df, year %in% c(2007, 2012))   

df_removeother = filter(df, initial_use %in% c("Urban", "Crop", "Forest"))
df_removeother = filter(df_removeother, final_use %in% c("Urban", "Crop", "Forest"))


df_se = filter(df, stateabbrev %in% c("AR","AL","FL","GA","KY","LA","MO","MS","NC","SC","TN"))

df_east_south = filter(df, stateabbrev %in% c("AR","AL","FL","GA","KY","LA","MO","MS","NC","SC","TN",
                                      "VA", "WV", "PA", "MD", "DE", "NJ", "NY", "CT", "MA", "RI", "NH", "VT", "ME"))

df_east = filter(df, stateabbrev %in% c("VA", "WV", "PA", "MD", "DE", "NJ", "NY", "CT", "MA", "RI", "NH", "VT", "ME"))

df_west = filter(df, stateabbrev %in% c("CA", "OR", "WA", "ID", "MT", "NV", "NM", "AZ", "CO", "UT", "WY"))

df_midwest = filter(df, stateabbrev %in% c("MI", "IN", "OH", "IL", "WI", "MN", "IA", "MO", "ND", "SD", "KS", "NE"))

df_south_west = filter(df, stateabbrev %in% c("AR","AL","FL","GA","KY","LA","MO","MS","NC","SC","TN",
                                              "CA", "OR", "WA", "ID", "MT", "NV", "NM", "AZ", "CO", "UT", "WY"))


#,"VA", "WV", "PA", "MD", "DE", "NJ", "NY", "CT", "MA", "RI", "NH", "VT", "ME"

summary(df[, c(12:15, 49:51)])
summary(df_dave[, c(12:16, 58:59)])
summary(df_no2015[, c(12:15, 49:51)])
summary(df_se[, c(12:15, 49:51)])

summary(df_west[, c(12:15, 49:51)])


df_merge = merge(x = df_no2015, y = df_dave_small, by.all = c('fips', 'year', 'lcc', 'initial_use', 'final_use'))

test = df_merge %>% select(pop_den, pipc, income, pop_growth, pop_density)
library("Hmisc")
rcorr(as.matrix(test))



ggplot(test) +
  geom_point(aes(x = pipc, y = income))


write_dta(df_years, 'processing/combined/ddc_data_urbancal_crprev_urbannrsub_dave.dta')

