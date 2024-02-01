#Installer for usdarnass. See Github site for details
# devtools::install_github("rdinter/usdarnass")

# load/install packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse,
               # sf,
               # maps,
               # rnaturalearth,
               # rnaturalearthdata,
               # tidycensus,
               ggplot2,
               usdarnass,
               readr)

#Acquire NASS key online, then set using the following commands
nass_set_key("3ECAC9F4-9114-3E0E-BB69-68F365F0AFF6")
readRenviron("~/.Renviron")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load in table, sort for top 30 crops
crop_table = read_csv("crop_table.csv")
crops = crop_table %>% filter(!grepl('&', commodity) & !grepl('TOTALS', commodity))
crops = unique(crops$commodity)[1:30]

# PRICES

get_data <- function(yr, crop_name, var) {
  
  print(yr)
  
  query_nass_api <- function(y, c, v) {
    
    output <- nass_data(year = y,
                    sector = "CROPS",
                    commodity_desc = c,
                    agg_level_desc = "COUNTY",
                    statisticcat_desc = var,
                    freq_desc = "ANNUAL")
    return(output)
  
    }
  
  df <- NULL
  attempt <- 1
  
  #This is necessary because sometimes queries mysteriously fail/throw errors
  while( is.null(df) && attempt <= 10 ) {
    attempt <- attempt + 1
    try(df <- query_nass_api(y = yr, c = crop_name, v = var))
    Sys.sleep(10)
  } 
  
  # short_descs <- unique(df$short_desc)[] #Get list of variable descriptions
  # price_var <- short_descs[which(grepl( #Select relevant price variable from list
  #               sprintf("%s", "", "- PRICE RECEIVED", crop_name), short_descs) == TRUE)]
  
  # df2 <- df %>% filter(short_desc == price_var,
  #                     reference_period_desc == "MARKETING YEAR")
  
  if (var == "AREA HARVESTED") {
    # short_descs <- unique(df$short_desc)[] #Get list of variable descriptions
    # price_var <- short_descs[which(grepl( #Select relevant price variable from list
    #               sprintf("%s - ACRES HARVESTED", crop_name), short_descs) == TRUE)]
    df2 <- df %>% filter(domain_desc == "TOTAL",
                        reference_period_desc == "YEAR", unit_desc == "ACRES", 
                        prodn_practice_desc == "ALL PRODUCTION PRACTICES")
  }
  
  
  if (var == "PRICE RECEIVED") {
    df2 <- df %>% filter(reference_period_desc == "MARKETING YEAR", prodn_practice_desc == "ALL PRODUCTION PRACTICES")
  }
  if (var == "YIELD") {
    df2 <- df %>% filter(reference_period_desc == "YEAR", prodn_practice_desc == "ALL PRODUCTION PRACTICES")
  }

  return(df2)
  
  }


# Splitting it up is necessary, otherwise the API complains about row limits

crops_list_1 = crops[1:5]
crops_list_2 = crops[6:10]
crops_list_3 = crops[11:20]
crops_list_4 = crops[21:30]

# These are the only variables available, cost isn't in NASS as far as I can tell
# "YIELD", "AREA HARVESTED", "PRICE RECEIVED"

price_df1 <- get_data(yr = seq(1997,2015), crop_name = crops_list_1, "PRICE RECEIVED")
price_df2 <- get_data(yr = seq(1997,2015), crop_name = crops_list_2, "PRICE RECEIVED")
price_df3 <- get_data(yr = seq(1997,2015), crop_name = crops_list_3, "PRICE RECEIVED")
price_df4 <- get_data(yr = seq(1997,2015), crop_name = crops_list_4, "PRICE RECEIVED")

df_price = bind_rows(price_df1, price_df2, price_df3, price_df4) %>% 
  select(commodity_desc, short_desc, state_name, state_fips_code, Value, year) %>%
  mutate(Value = gsub("[^0-9.-]", "", Value)) %>% filter(Value != '')

yield_df1 <- get_data(yr = seq(1997,2015), crop_name = crops_list_1, "YIELD")
yield_df2 <- get_data(yr = seq(1997,2015), crop_name = crops_list_2, "YIELD")
yield_df3 <- get_data(yr = seq(1997,2015), crop_name = crops_list_3, "YIELD")
yield_df4 <- get_data(yr = seq(1997,2015), crop_name = crops_list_4, "YIELD")

df_yield = bind_rows(yield_df1, yield_df2, yield_df3, yield_df4) %>% 
  select(commodity_desc, short_desc, state_name, state_fips_code, Value, year) %>%
  mutate(Value = gsub("[^0-9.-]", "", Value)) %>% filter(Value != '')


acres_df = data.frame()

# for acerage the api complains about row limits anyway, so we can only do it
# with a really inefficient loop

for (crop in crops){
  print(crop)
  if (crop != 'HAY'){  
    temp = get_data(yr = seq(1997,2015), crop_name = crop, "AREA HARVESTED")
  }
  # Hay is too long, split it up into two API calls
  if (crop == 'HAY'){  
    temp1 = get_data(yr = seq(1997,2008), crop_name = crop, "AREA HARVESTED")
    temp2 = get_data(yr = seq(1997,2008), crop_name = crop, "AREA HARVESTED")
    temp = bind_rows(temp1, temp2)
  }
  acres_df = bind_rows(acres_df, temp)
}

acres_df = acres_df %>% select(commodity_desc, short_desc, state_name, state_fips_code, Value, year) %>%
  mutate(Value = gsub("[^0-9.-]", "", Value)) %>% filter(Value != '')

write.csv(df_yield,"yield_table.csv", row.names = FALSE)
write.csv(df_price,"price_table.csv", row.names = FALSE)

write.csv(acres_df,"acreage_table.csv", row.names = FALSE)



