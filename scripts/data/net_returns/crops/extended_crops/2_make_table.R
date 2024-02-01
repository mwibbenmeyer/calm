library(tidyverse)
library(readr)
library(readxl)


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd('../../../..')

ag = read_csv("raw_data/net_returns/crops/extended_crops/2017-agcensus-chapter1-table35-US.csv")
ag2 = read_csv("raw_data/net_returns/crops/extended_crops/2017-agcensus-chapter1-table36-US.csv")
ag3 = read_csv("raw_data/net_returns/crops/extended_crops/2017-agcensus-chapter1-table37-US.csv")
ag4 = read_csv("raw_data/net_returns/crops/extended_crops/2017-agcensus-chapter1-table38-US.csv")

field_crops = ag %>% filter(!grepl('IRRIGATED', `data item`) & grepl('ACRES HARVESTED', `data item`) & is.na(`domain category`))
vegetables = ag2 %>% filter(!grepl('FRESH MARKET', `data item`) & !grepl('TOTALS', `data item`) & !grepl('PROCESSING', `data item`) & !grepl('IRRIGATED', `data item`) & grepl('ACRES HARVESTED', `data item`) & is.na(`domain category`))
fruit = ag3 %>% filter(!grepl('NON-BEARING', `data item`) & grepl('ACRES BEARING', `data item`) & is.na(`domain category`))
berries = ag4 %>% filter(!grepl('IRRIGATED', `data item`) & grepl('ACRES GROWN', `data item`) & is.na(`domain category`))

field_crops$value = as.numeric(gsub(",","",field_crops$value))
vegetables$value = as.numeric(gsub(",","",vegetables$value))
fruit$value = as.numeric(gsub(",","",fruit$value))
berries$value = as.numeric(gsub(",","",berries$value))

merged = bind_rows(field_crops, vegetables, fruit, berries)


merged = merged[order(merged$value, decreasing = TRUE),]  

write.csv(merged,"processing/net_returns/crops/extended_crops/crop_table.csv", row.names = FALSE)