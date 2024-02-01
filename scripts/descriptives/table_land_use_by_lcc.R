####################################################
# Matt Wibbenmeyer
# January 31, 2022
# Land use by LCC
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

###############################################################
# zipFastener for TWO dataframes of unequal length
zipFastener <- function(df1, df2, along=2)
{
  # parameter checking
  if(!is.element(along, c(1,2))){
    stop("along must be 1 or 2 for rows and columns
                                              respectively")
  }
  # if merged by using zip feeding along the columns, the
  # same no. of rows is required and vice versa
  if(along==1 & (ncol(df1)!= ncol(df2))) {
    stop ("the no. of columns has to be equal to merge
               them by zip feeding")
  }
  if(along==2 & (nrow(df1)!= nrow(df2))) {
    stop ("the no. of rows has to be equal to merge them by
               zip feeding")
  }
  # zip fastener preperations
  d1 <- dim(df1)[along]
  d2 <- dim(df2)[along]
  i1 <- 1:d1           # index vector 1
  i2 <- 1:d2 + d1      # index vector 2
  # set biggest dimension dMax
  if(d1==d2) {
    dMax <- d1
  } else if (d1 > d2) {
    length(i2) <- length(i1)    # make vectors same length, 
    dMax <- d1                  # fill blanks with NAs   
  } else  if(d1 < d2){
    length(i1) <- length(i2)    # make vectors same length,
    dMax <- d2                  # fill blanks with NAs   
  }
  
  # zip fastener operations
  index <- as.vector(matrix(c(i1, i2), ncol=dMax, byrow=T))
  index <- index[!is.na(index)]         # remove NAs
  
  if(along==1){
    colnames(df2) <- colnames(df1)   # keep 1st colnames                  
    res <- rbind(df1,df2)[ index, ]  # reorder data frame
  }
  if(along==2) res <- cbind(df1,df2)[ , index]           
  return(res)
}
###############################################################

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

df <- points[initial_use == final_use] %>%
        .[initial_use != "Rural" & final_use != "Rural"] %>% 
        .[initial_use == "Pasture" | initial_use == "Range", initial_use := "Other"] %>% #Recode pasture and range to "Other"
        .[ , .(total_acres = sum(acresk)), by = c("fips","lcc","initial_use","year")] %>%
        .[ , .(total_acres = sum(total_acres)), by = c("lcc","initial_use","year")] %>%
        .[ year >= 2002 & initial_use %ni% c("Federal","Water")] %>%
        .[ , lcc_acres := sum(total_acres), by = c("lcc","year")] %>%
        .[ , pctacres := total_acres/lcc_acres]

# Make table ------------------------------------------------------------------
        
table1 <- df[year == 2012 & initial_use != "Rural", c("lcc","initial_use","total_acres")] %>%
          spread(key = "initial_use", value = "total_acres")

table2 <- df[year == 2012 & initial_use != "Rural", c("lcc","initial_use","pctacres")] %>%
  spread(key = "initial_use", value = "pctacres") %>%
  mutate_if(is.numeric, function(x) paste0("[",as.character(formatC(x, digits = 2, format = "f")),"]"))
table2$Total <- ""
table2$lcc <- ""

table1$Total <- rowSums(table1[ ,2:5])
sum_lcc <- c("Total", formatC(colSums(table1[ ,2:5]), digits = 0, format = "f", big.mark = ","))
table1 <- table1 %>% mutate_if(is.numeric, function(x) as.character(formatC(x, digits = 0, format = "f", big.mark = ",")))

table <- zipFastener(table1,table2, along = 1) %>%
  rename(LCC = lcc) %>%
  mutate(LCC = str_replace_all(LCC,"_","--")) %>%
  mutate(LCC = ifelse(LCC!="", paste("\\multirow{2}{*}{", LCC, "}", sep=""), ""))

dst = 'results/summary_stats/table_land_use_by_lcc/'
dir.create(dst, recursive = TRUE, showWarnings = FALSE)
write.table(table, file = paste0(dst,"table_contents.tex"),
            quote = FALSE, col.names = TRUE, row.names = FALSE,
            sep = '&', eol = '\\\\ \n')
