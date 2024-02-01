####################################################
# Matt Wibbenmeyer
# June 16, 2021
# Script to make table showing conversions across uses
####################################################

## Load/install packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse,
               readxl,
               sf,
               tidycensus,
               haven,
               stringr,
               data.table
)

`%ni%` <- Negate(`%in%`)

points <- read_dta("processing/pointpanel/pointpanel_estimation_unb.dta") %>% as.data.table()

# points <- points[ , c("initial_use","final_use") := .(recode(initial_use, "CRP" = "Other",
#                                                                          "Pasture" = "Other",
#                                                                          "Range" = "Other"),
#                                                       recode(final_use, "CRP" = "Other",
#                                                              "Pasture" = "Other",
#                                                              "Range" = "Other"))] %>%
#           .[initial_use %ni% c("Rural","Water","Federal")]

points <- points[initial_use %ni% c("Rural","Water","Federal")]


###############################################################################
# Table summarizing initial and final acres 

# order <- data.table(initial_use=c("Crop","Forest","Urban","Other"), order=c(1,2,3,4))
order <- data.table(initial_use=c("Crop","Forest","Urban","Pasture","Range","CRP"), order=c(1,2,3,4,5,6))
points <- points[order, on = "initial_use" ]

col1 <- points[year == 2002, .(total_acresk = sum(acresk), order = mean(order)), by = initial_use] %>% 
  .[order(order)]%>%
  .[ , order := NULL]

col2 <- points[year == 2007, .(total_acresk = sum(acresk), order = mean(order)), by = initial_use] %>% 
  .[order(order) ]
col2 <- col2[ , order := NULL]
col2 <- col2 %>% setnames(., old = c('total_acresk'), new = c('total_acresk2007'))

col3 <- points[year == 2012, .(total_acresk = sum(acresk), order = mean(order)), by = initial_use] %>% 
  .[order(order) ]
col3 <- col3[ , order := NULL]
col3 <- col3 %>% setnames(., old = c('total_acresk'), new = c('total_acresk2012'))

acres.table <- cbind(col1,col2[, initial_use := NULL],col3[, initial_use := NULL])
acres.table <- acres.table %>% mutate_if(is.numeric, formatC, digits=0, width = 6, format = "f", big.mark = ',')

dst = 'results/summary_stats/acres_table/'
dir.create(dst, recursive = TRUE, showWarnings = FALSE)
write.table(acres.table, file = paste0(dst,"table_contents.tex"),
            quote = FALSE, row.names = FALSE, col.names = FALSE,
            sep = '&', eol = '\\\\ \n')


###############################################################################
# Table summarizing transitions from 2002 to 2012

order1 <- order %>% as.data.table() %>% setnames(., old = c('initial_use'), new = c('use2002'))
order2 <- order %>% as.data.table() %>% setnames(., old = c('initial_use'), new = c('use2012'))


trans.table <- # Cast 2002 and 2012 land uses from long to wide
            dcast(points[year %in% c(2002,2012)], riad_id + acresk + fips ~ year, value.var = c("initial_use") ) %>%
              setnames(., old = c('2002','2012'), new = c('use2002','use2012')) %>%
            # Drop NA values which exist for points that are not present in 2002
              .[!is.na(use2002) & !is.na(use2012)] %>%
            # Calculate initial acres and final acres in each land use type
              .[ , transition := paste0(use2002,"_",use2012)] %>%
              .[ , initial_acres := sum(acresk), by = use2002] %>%
              .[ , .(final_acres = sum(acresk), 
                     initial_acres = mean(initial_acres), 
                     use2002 = first(use2002),
                     use2012 = first(use2012)), by = transition] %>%
            # Calculate percent of land in each long transition (2002-2012)
              .[ , percent := final_acres/initial_acres] %>%
              .[ , c("use2002","use2012","percent")] %>%
            # Order variables
              .[order2, on = 'use2012'] %>%
              .[order(order)] %>%
              .[order1, on = 'use2002'] %>%
              .[order(order)] %>%
            # Cast from long to wide
              dcast(., forcats::as_factor(use2002) ~ forcats::as_factor(use2012), value.var = "percent")

trans.table <- trans.table %>% mutate_if(is.numeric, formatC, digits=2, width = 4, format = "fg", big.mark = ',')
trans.table

dst = 'results/summary_stats/transition_table/'
dir.create(dst, recursive = TRUE, showWarnings = FALSE)
write.table(trans.table, file = paste0(dst,"table_contents.tex"),
            quote = FALSE, row.names = FALSE, col.names = FALSE,
            sep = '&', eol = '\\\\ \n')


