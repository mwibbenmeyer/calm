if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse,
               ggplot2,
               readr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd('../../../..')

acreagefiles = list.files(path = "raw_data/net_returns/crops/extended_crops/acreage", pattern = "*.csv")


acreage_combine = data.frame()

for (file in acreagefiles){
  temp = read.csv(str_c('raw_data/net_returns/crops/extended_crops/acreage/', file)) %>% filter(str_detect(Value, fixed('('), negate = TRUE)) %>% mutate(Value = as.numeric(gsub(",","",Value)))

  acreage_combine = rbind(acreage_combine, temp)
}

write.csv(acreage_combine,"processing/net_returns/crops/extended_crops/combined_acreage.csv", row.names = FALSE)