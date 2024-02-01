###############################################################################
# Qinrui Xiahou
# Mar 23, 2022
# Script to generate LaTex tables of regression results and elasticities
# Data input: 1) regression output from results\initial_estimation\regs_2022-03
#             2) final acres from processing\elasticity
###############################################################################

## Load/install packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse,
               usmap,
               ggplot2,
               foreign,
               haven,
               RColorBrewer,
               maps,
               patchwork,
               imager,
               readxl)
theme_set(theme_bw())

# Set working directory to land-use 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../../../../")

### Elasticity Tables

## Read in data
final_acres_lcc <- read_excel("processing/elasticity/lcc_static_all_no2015/final_acres_lcc_static.xlsx")

## Data wrangling
elasticity <- final_acres_lcc %>%
  rename(landuse=`...1`) %>%
  mutate(order = c(4,2,1,3,5)) %>%
  arrange(order) %>%
  select(landuse, baseline, crop10pct, crp10pct, forest10pct, urban10pct, other10pct) %>%
  mutate(Crop = (crop10pct-baseline)/baseline/0.1,
         CRP = (crp10pct-baseline)/baseline/0.1,
         Forest = (forest10pct-baseline)/baseline/0.1,
         Urban = (urban10pct-baseline)/baseline/0.1,
         Other = (other10pct-baseline)/baseline/0.1) %>%
  select(Crop, CRP, Forest, Urban, Other)

## Format the table
elasticity_formatted <- elasticity %>%
  round(3) %>%
  mutate(across(everything(), as.character)) %>%
  mutate(landuse = c("Crop", "CRP", "Forest", "Urban", "Others")) %>%
  mutate(Crop = ifelse(landuse=="Crop", paste("\\textbf{", Crop, "}", sep=""), Crop),
         CRP = ifelse(landuse=="CRP", paste("\\textbf{", CRP, "}", sep=""), CRP),
         Forest = ifelse(landuse=="Forest", paste("\\textbf{", Forest, "}", sep=""), Forest),
         Urban = ifelse(landuse=="Urban", paste("\\textbf{", Urban, "}", sep=""), Urban),
         Other = ifelse(landuse=="Others", paste("\\textbf{", Other, "}", sep=""), Other)) %>%
  mutate_all(~case_when(. == "0" ~ "0.000",
                        . == "\\textbf{0}" ~ "\\textbf{0.000}",
                        . == "-0.03" ~ "-0.030",
                        TRUE ~ .)) %>%
  select(landuse, Crop, CRP, Forest, Urban, Other)

## Export the table
dst = 'results/model_results/elasticities/lcc_static_all_no2015/'
dir.create(dst, recursive = TRUE, showWarnings = FALSE)
write.table(elasticity_formatted, file = paste0(dst,"table_contents.tex"),
            quote = FALSE, row.names = FALSE, col.names = FALSE,
            sep = '&', eol = '\\\\ \n')


### Regression Tables

## Read in data
reg_lcc <- read_delim("results/initial_estimation/regs_2022-03/regs_2022-03-11_unweighted_lcc_3digits_st_no2015.txt", 
                      "\t", escape_double = FALSE, trim_ws = TRUE) %>% # may need to update
  rename(var = X1, est = `(1)`) %>%
  filter(is.na(est)==FALSE, est != "y",
         !var %in% c("Observations", "R-squared", "weights", "r2_a"))

## Data wrangling and table formatting
nr_coef <- reg_lcc %>%
  slice(1:40) %>%
  mutate(id = row_number(),
         var2 = var) %>% 
  separate(var2, sep="#c.d", c(NA, "landuse")) %>%
  tidyr::fill(landuse) %>% # copy the previous value
  mutate(landuse = ifelse(landuse=="Other", "Others", landuse)) %>%
  mutate(flag1 = id %% 2,
         flag2 = ifelse(flag1 == 1, ((id+1)/2)%%4, (id/2)%%4),
         flag2 = ifelse(flag2 == 0, 4, flag2),
         flag = paste("LCC", flag2, "_par", flag1, sep="")) %>%
  select(-id, -flag1, -flag2, -var) %>%
  pivot_wider(names_from = "landuse", values_from = "est") %>%
  select(flag, Crop, CRP, Forest, Urban, Others)

nr_coef_formatted <- nr_coef %>%
  mutate(id = row_number()) %>%
  mutate(LCC = case_when(id%%2==0 ~ "", 
                         flag=="LCC1_par1" ~ "\\multirow{2}{*}{1--2}",
                         flag=="LCC2_par1" ~ "\\multirow{2}{*}{3--4}",
                         flag=="LCC3_par1" ~ "\\multirow{2}{*}{5--6}",
                         flag=="LCC4_par1" ~ "\\multirow{2}{*}{7--8}")) %>%
  mutate(` ` = "") %>%
  select(` `, LCC, Crop, CRP, Forest, Urban, Others)

cost_coef <- reg_lcc %>%
  slice(41:152) %>%
  mutate(id = row_number(),
         var2 = var) %>%
  separate(var2, sep="to", c("temp", "finaluse")) %>%
  separate(temp, sep="#c.", c(NA, "initialuse")) %>%
  tidyr::fill(initialuse, finaluse) %>% # copy the previous value
  mutate(initialuse = ifelse(initialuse=="Other", "Others", initialuse),
         finaluse = ifelse(finaluse=="Other", "Others", finaluse)) %>%
  mutate(flag1 = id %% 2,
         flag2 = ifelse(flag1 == 1, ((id+1)/2)%%4, (id/2)%%4),
         flag2 = ifelse(flag2 == 0, 4, flag2),
         flag = paste("LCC", flag2, "_par", flag1, sep="")) %>%
  select(-id, -flag1, -flag2, -var) %>%
  pivot_wider(names_from = "finaluse", values_from = "est") %>%
  select(initialuse, flag, Crop, CRP, Forest, Urban, Others) %>%
  arrange(initialuse)

cost_coef_formatted <- cost_coef %>%
  mutate(id = row_number()) %>%
  mutate(LCC = case_when(id%%2==0 ~ "", 
                         flag=="LCC1_par1" ~ "\\multirow{2}{*}{1--2}",
                         flag=="LCC2_par1" ~ "\\multirow{2}{*}{3--4}",
                         flag=="LCC3_par1" ~ "\\multirow{2}{*}{5--6}",
                         flag=="LCC4_par1" ~ "\\multirow{2}{*}{7--8}")) %>%
  mutate(initialuse = case_when(id%%8==1 ~ paste("\\multirow{8}{*}{", initialuse, "}", sep=""), 
                                TRUE ~ "")) %>%
  mutate(Crop = case_when(initialuse=="\\multirow{8}{*}{Crop}" ~ "\\multirow{8}{*}{$-$}", 
                          TRUE ~ Crop),
         CRP = case_when(initialuse=="\\multirow{8}{*}{CRP}" ~ "\\multirow{8}{*}{$-$}", 
                         TRUE ~ CRP),
         Forest = case_when(initialuse=="\\multirow{8}{*}{Forest}" ~ "\\multirow{8}{*}{$-$}", 
                            TRUE ~ Forest),
         Urban = case_when(initialuse=="\\multirow{8}{*}{Urban}" ~ "\\multirow{8}{*}{$-$}", 
                           TRUE ~ Urban),
         Others = case_when(initialuse=="\\multirow{8}{*}{Others}" ~ "\\multirow{8}{*}{$-$}", 
                            TRUE ~ Others),
         Urban = case_when(initialuse=="\\multirow{8}{*}{CRP}" ~ "\\multirow{8}{*}{(no data)}", 
                           TRUE ~ Urban),
         CRP = case_when(initialuse=="\\multirow{8}{*}{Forest}" ~ "\\multirow{8}{*}{(no data)}", 
                         TRUE ~ CRP)) %>%
  select(initialuse, LCC, Crop, CRP, Forest, Urban, Others)

## Export the tables
dst = 'results/model_results/regressions/lcc_static_all_no2015/'
dir.create(dst, recursive = TRUE, showWarnings = FALSE)
write.table(nr_coef_formatted, file = paste0(dst,"table_contents_nr.tex"),
            quote = FALSE, row.names = FALSE, col.names = FALSE,
            sep = '&', eol = '\\\\ \n')
write.table(cost_coef_formatted, file = paste0(dst,"table_contents_cost.tex"),
            quote = FALSE, row.names = FALSE, col.names = FALSE,
            sep = '&', eol = '\\\\ \n', na = "")
