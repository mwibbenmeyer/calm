library(haven)
library(tidyverse)
library(censusapi)
library(readr)
library(readxl)
library(stringr)

substrRight = function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../../../../")

Sys.setenv(CENSUS_KEY="a811f383d3224c362794899a57b196696b70695c")


df = read_dta('processing/combined/ddc_data_urbancal_crprev.dta')

years = unique(df$year)

#the 1990 api sucks and i have to do this weird hack to get it to work
#just run this thing once and save it, it takes so long

# pop_1997 = data.frame()
# for (st in (1:56)[-c(3, 7, 14, 43, 52)]){
# 
#   df = getCensus(name = "1990/pep/int_charagegroups",
#                        vars = c("COUNTY", "AGEGRP", "HISP", "RACE_SEX", "POP", "YEAR"),
#                        region = "county:*",
#                        regionin = str_c("state:", str_pad(st, 2, pad = "0"))) %>%
#     select(state, county, POP, YEAR) %>% filter(YEAR == 97)
# 
# 
#   df1 = df %>% mutate(FIPS = str_c(state, county))
#   df1 = aggregate(df1$POP, by=list(FIPS=df1$FIPS), FUN = sum)
#   df2 = df[!duplicated(df$county), ] %>% select(state, county, year=YEAR)
# 
#   df3 = cbind(df2, POP = df1$x)
# 
#   pop_1997 = rbind(pop_1997, df3)
# 
# }
# 
# write.csv(pop_1997,'results/replace_urban_forest_nr/pop_1997.csv')

pop_1997 = read_csv("processing/net_returns/urban/pop_1997.csv") %>% select(2:5)
pop_1997$year = sub("^", "19", pop_1997$year)

pop_2000 = getCensus(name = "2000/pep/int_population",
                     vars = c("POP", "DATE_", "DATE_DESC"), 
                     region = "county:*",
                     DATE_ = 2,
                     DATE_ = 4,
                     DATE_ = 9)

pop_2015 = getCensus(name = "2015/pep/population",
                     vars = c("POP", "DATE_"),
                     region = "county:*",
                     DATE_ = 5,
                     DATE_ = 8)


pop_2000 = pop_2000 %>% 
  mutate(year = case_when(
    DATE_ == 2 ~ 2000,
    DATE_ == 4 ~ 2002,
    DATE_ == 9 ~ 2007,
)) %>% select(state, county, year, POP)


pop_2015 = pop_2015 %>% 
  mutate(year = case_when(
    DATE_ == 5 ~ 2012,
    DATE_ == 8 ~ 2015,
  )) %>% select(state, county, year, POP)

#pop density

landarea = read_excel("raw_data/net_returns/urban/LND01.xls") %>% 
  select(fips = STCOU, land_area = LND110200D)


pop_combine = rbind(pop_1997, pop_2000)
pop_combine = rbind(pop_combine, pop_2015)

pop_pivot = pivot_wider(pop_combine, names_from = year, values_from = POP, names_prefix = 'pop_')

pop_pivot = pop_pivot %>% mutate(aagrpct_2015 = ((pop_2015/pop_2012)^(1/5)-1)*100,
                                 aagrpct_2012 = ((pop_2012/pop_2007)^(1/5)-1)*100,
                                 aagrpct_2007 = ((pop_2007/pop_2002)^(1/5)-1)*100,
                                 aagrpct_2002 = ((pop_2002/pop_1997)^(1/5)-1)*100,
                                 GeoFIPS = str_c(state, county))

pop_density = merge(x = pop_pivot, y = landarea, by.x = 'GeoFIPS', by.y = 'fips', all.x = TRUE) %>%
  select(c(1, 4:9, 14))


pop_density = pop_density %>% mutate(pop_den_2015 = pop_2015 / land_area,
                                     pop_den_2012 = pop_2012 / land_area,
                                     pop_den_2007 = pop_2007 / land_area,
                                     pop_den_2002 = pop_2002 / land_area)

pop_density = pop_density %>% select(c(1, 9:12))

pop_density = pop_density %>%
  pivot_longer(!GeoFIPS, names_to = "year", values_to = "pop_density") %>% 
  mutate(year = substrRight(year, 4))


pop_prep = pop_pivot %>% select(c(9:13))

pop_prep = pop_prep %>%
  pivot_longer(!GeoFIPS, names_to = "year", values_to = "pop_growth") %>% 
  mutate(year = substrRight(year, 4))


pop_prep = merge(pop_prep, pop_density)


#for 2015 pep, grab date = 5 for 2012 est, date = 8 for 2015
#https://www.census.gov/data/developers/data-sets/popest-popproj/popest/popest-vars.Vintage_2015.html

# deflator = read_csv("raw_data/GDPDEF.csv")
# 
# deflator = deflator %>% mutate(year = substr(DATE, 1, 4)) %>% select(c(2:3))
# 
# deflator = aggregate(deflator$GDPDEF, by=list(year=deflator$year), FUN=mean)
# 
# #peg to 2010
# deflator[,2] = deflator[,2]*100/deflator[64,2]
# 
# 
# income = read_csv("raw_data/CAINC1/CAINC1__ALL_AREAS_1969_2020.csv")
# 
# income = subset(income, LineCode == 3)
# 
# income[, 9:60] = sapply(income[, 9:60], as.numeric)
# 
# income = income %>% mutate(county = substrRight(GeoFIPS, 3)) %>% 
#   subset(county != "000") %>% 
#   select(-county) 
#   
# #find trailing 5 years average and convert to 2010 dollars
# income = income %>% mutate(income_2002 = (deflator[56,2] / rowMeans(subset(income, select = c(37:41))) / 100)^-1,
#                            income_2007 = (deflator[61,2] / rowMeans(subset(income, select = c(42:46))) / 100)^-1,
#                            income_2012 = (deflator[66,2] / rowMeans(subset(income, select = c(47:51))) / 100)^-1,
#                            income_2015 = (deflator[69,2] / rowMeans(subset(income, select = c(50:54))) / 100)^-1)
#   
#   
# income = income %>% select(GeoFIPS, GeoName, Unit, income_per_cap_2002 = income_2002,
#                              income_per_cap_2007 = income_2007, income_per_cap_2012 = income_2012, 
#                              income_per_cap_2015 = income_2015)
# 
# 
# 
# 
# inc_prep = income %>% select(c(1, 4:7)) 
# 
# inc_prep = inc_prep %>%
#   pivot_longer(!GeoFIPS, names_to = "year", values_to = "income") %>% 
#   mutate(year = substrRight(year, 4))
# 
# 
# inc_pop_merge = merge(inc_prep, pop_prep, by = c('GeoFIPS', 'year'))
# 
# inc_pop_merge$GeoFIPS = as.numeric(as.character(inc_pop_merge$GeoFIPS))
# 
# df = df %>% select(-1) %>% mutate(fips = str_pad(as.character(df$fips), 5, 'left', '0'))

# df_final = merge(x = df, y = inc_pop_merge, by.x = c('fips', 'year'),
#                  by.y = c('GeoFIPS', 'year'), all.x = TRUE)

pop_prep$GeoFIPS = as.numeric(as.character(pop_prep$GeoFIPS))

df_final = merge(x = df, y = pop_prep, by.x = c('fips', 'year'),
                 by.y = c('GeoFIPS', 'year'), all.x = TRUE)

# df_final$income = as.numeric(as.character(df_final$income))

df_final$urban_nr = df_final$pop_growth
df_final = df_final %>% select(-pop_growth) 

df_final = df_final %>% mutate(urbanXlcc0 = case_when(lcc0 == 1 ~ urban_nr,
                                                             lcc0 == 0 ~ 0),
                               urbanXlcc1 = case_when(lcc1 == 1 ~ urban_nr,
                                                             lcc1 == 0 ~ 0),
                               urbanXlcc2 = case_when(lcc2 == 1 ~ urban_nr,
                                                             lcc2 == 0 ~ 0),
                               urbanXlcc3 = case_when(lcc3 == 1 ~ urban_nr,
                                                             lcc3 == 0 ~ 0),
                               urbanXlcc4 = case_when(lcc4 == 1 ~ urban_nr,
                                                             lcc4 == 0 ~ 0))

# fix fips code change from shannon county SD to oglala lakota county
df_final$fips[df_final$fips == '46113'] = '46102' 


# urbanxlccs = df_final[, c(14, 45:49)]
# urbanxlccs$urbanXlcc0 = ifelse(urbanxlccs$urbanXlcc0 != 0, urbanxlccs$urban_nr, 0)
# urbanxlccs$urbanXlcc1 = ifelse(urbanxlccs$urbanXlcc1 != 0, urbanxlccs$urban_nr, 0)
# urbanxlccs$urbanXlcc2 = ifelse(urbanxlccs$urbanXlcc2 != 0, urbanxlccs$urban_nr, 0)
# urbanxlccs$urbanXlcc3 = ifelse(urbanxlccs$urbanXlcc3 != 0, urbanxlccs$urban_nr, 0)
# urbanxlccs$urbanXlcc4 = ifelse(urbanxlccs$urbanXlcc4 != 0, urbanxlccs$urban_nr, 0)

# test_missing_lccs = urbanxlccs %>% filter(if_all(c(2:6), ~ is.na(.)))

# df_final[, c(45:49)] = urbanxlccs[, c(2:6)]
test_missing = df_final %>% filter(if_all(c(45:49), ~ is.na(.)))

# test_missing2 = df_final %>% filter(is.na(urban_nr))

# dff.fips = data.frame(df_final$fips) %>% rename(fips = df_final.fips)
# df.fips = data.frame(df$fips) %>% rename(fips = df.fips)
# 
# test = anti_join(dff.fips,df.fips)



write_dta(df_final, 'processing/combined/ddc_data_urbancal_crprev_urbannrsub.dta')


