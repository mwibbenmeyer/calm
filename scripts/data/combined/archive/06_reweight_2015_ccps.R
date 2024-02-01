
## Load/install packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse,
               data.table,
               ggplot2,
               usmap,
               sf,
               rnaturalearth,
               rnaturalearthdata,
               maps,
               tidycensus,
               tm,
               reshape2,
               rgeos,
               haven,
               Matrix,
               pracma,
               expm
)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # sets directory to the current directory
setwd('../../../') # relative paths to move directory to the root project directory

df <- haven::read_dta("processing/combined/full_combined_returns.dta") %>% as.data.table()

df <- df[order(fips,year,lcc,initial_use,final_use)] %>%
        .[year == 2015]


adjust_2015_probs <- function(data, f, l) {
  
  f 

  probs <- data[fips == f & lcc == l]$weighted_ccp
  m3 <- matrix(probs, nrow = 5, ncol = 5)
  
  #Identify the indices of any columns with only a zero/one
  one_cols_i <- as.numeric(lapply(apply(m3, 2, function(x) unique(x)),
                                  function(x) identical(x,c(0,1))) %>% unlist())
  one_pos <- which(one_cols_i == 1)
  
  #Identify the indices of any rows with only a zero
  zero_rows_i <- as.numeric(lapply(apply(m3, 1, function(x) unique(x)),
                                   function(x) identical(x,c(0))) %>% unlist())
  
  #Drop first row and column for counties in which CRP/Forest conversions result in singular matrix
  if (sum(one_cols_i) == 1 & sum(zero_rows_i) == 1) { 
    
    #Remove problem rows/columns and calculate m5
    m3 <- m3[which(zero_rows_i == 0), which(one_cols_i == 0)]
    m5 <- rootm(m3,3)$B %^% 5
    
    #Replace removed rows 
    zero_row <- rep(0,dim(m5)[2])
    missing_row <- setdiff(seq(1,5),which(zero_rows_i == 0))
    if (missing_row == 1) {
        m5 <- rbind(zero_row,
                m5[missing_row:4,])
    }
    else if (missing_row == 5) {
      m5 <- rbind(m5[1:4,],
                  zero_row)
    }
    else {
      m5 <- rbind(m5[1:missing_row-1,],
                    zero_row,
                    m5[missing_row:4,])
      }
    #Replace removed columns
    one_col <- rep(0,dim(m5)[1])
    one_col[one_pos] <-1
    missing_col <- setdiff(seq(1,5),which(one_cols_i == 0))
    if (missing_col == 1) {
      m5 <- cbind(one_col,
                  m5[ , missing_col:4])
    }
    else if (missing_col == 5) {
      m5 <- cbind(m5[ , 1:4],
                  one_col)
    }
    else {
      m5 <- cbind(m5[ , 1:missing_col-1],
                  one_col,
                  m5[ , missing_col:4])
    }
  }
  
  else  {
    m5 <- rootm(m3,3)$B %^% 5
    }
  
  output <- data.frame(fips = f,
                      lcc = l,
                       year = 2015,
                       initial_use = rep(uses, each = 5), 
                       final_use = rep(uses),
                       weighted_ccp = as.list(probs) %>% unlist(),
                   ccp_2015 = as.list(m5) %>% unlist())
  
  return(output)
  
}

uses <- c("CRP","Crop","Forest","Other","Urban")

for (f in unique(df$fips) %>% head(1000)) {
  for (l in lcc_values) {
    print(paste0(f,l))
    adjust_2015_probs(data = df, f = as.numeric(f), l = l)
    #print(result)
  }
}

df[fips == "1001" & lcc == "1_2"]

m <- do.call(rbind, do.call(rbind,
        lapply(unique(df$fips) %>% head(), function(f)
        lapply(lcc_values, function(l) adjust_2015_probs(df, f = f, l = l)))))
