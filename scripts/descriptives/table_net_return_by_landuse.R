####################################################
# Qinrui Xiahou
# March 28, 2022
# Net returns by LCC by year
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

## Set working directory to land-use 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../../")

savefig <- function(path,width,height) {
  lapply(c("png","pdf"), function(ftype) 
    ggplot2::ggsave(paste0(path,".",ftype),
                    device=ftype,width=width,height=height))
}

## Load the data
ddc_data_urbancal <- read_dta("processing/combined/ddc_data_urbancal.dta")

## Summary table
nr <- ddc_data_urbancal %>%
  distinct(fips, year, CRP_nr, forest_nr, urban_nr, other_nr, crop_nr, .keep_all = TRUE) %>%
  select(fips, year, CRP_nr, forest_nr, urban_nr, other_nr, crop_nr) %>%
  rename(CRP = CRP_nr, Forest = forest_nr, Urban = urban_nr, Other = other_nr, Crop = crop_nr) %>%
  pivot_longer(cols = c(3:7), names_to = "landuse", values_to = "nr")

nr_summary <- nr %>%
  group_by(year, landuse) %>%
  summarize(mean_nr = mean(nr, na.rm = TRUE),
            median_nr = median(nr, na.rm = TRUE),
            sd_nr = sd(nr, na.rm = TRUE))

nr_summary %>%
  arrange(landuse, year) %>%
  write.csv("results/summary_stats/net_return_by_landuse/nr_summary_temp.csv")

## Helper function
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

## Separate the tables
nr_median <- nr_summary %>%
  select(year, landuse, median_nr) %>%
  mutate(median_nr = formatC(median_nr, format="f", big.mark = ",", digits=2)) %>%
  pivot_wider(names_from = "landuse", values_from = "median_nr") %>%
  mutate(year = as.character(year))

nr_sd <- nr_summary %>%
  select(year, landuse, sd_nr) %>%
  mutate(sd_nr = paste("(", formatC(sd_nr, format="f", big.mark = ",", digits=2), ")", sep="")) %>%
  pivot_wider(names_from = "landuse", values_from = "sd_nr") %>%
  mutate(year = "")
  
nr_table <- zipFastener(nr_median,nr_sd, along = 1) %>%
  rename(Year = year) %>%
  mutate(Year = ifelse(Year!="", paste("\\multirow{2}{*}{", Year, "}", sep=""), ""))

dst = 'results/summary_stats/net_return_by_landuse/'
dir.create(dst, recursive = TRUE, showWarnings = FALSE)
write.table(nr_table, file = paste0(dst,"table_contents.tex"),
            quote = FALSE, col.names = TRUE, row.names = FALSE,
            sep = '&', eol = '\\\\ \n')

