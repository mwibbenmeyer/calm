## Load/install packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse,
               readxl,
               sf,
               tidycensus,
               haven,
               stringr,
               data.table,
               dplyr,
               haven,
               patchwork,
               ggpmisc
)

#Function to save figures
savefig <- function(path,width,height) {
  
  lapply(c("png","pdf"), function(ftype) 
    ggplot2::ggsave(paste0(path,".",ftype),
                    device=ftype,width=width,height=height))
  
}


setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # sets directory to the current directory
setwd('../../') # relative paths to move directory to the root project directory

df <- read_dta("processing/combined/ddc_data.dta")

outpath <- "results/initial_descriptives/combined/scatter_returns_probs/"
dir.create(outpath, recursive = TRUE, showWarnings = FALSE)


# Create plots ------------------------------------------------------------

make_plot <- function(i_use, f_use, year, save = FALSE) {
  
  low.return <- str_to_lower(i_use)
  data <- df %>% filter(year == year & initial_use == i_use & final_use == f_use) %>%
            mutate(initial_returns = get(sprintf("%s_nr",low.return), .))
  N <- dim(data)[1]
  
  # Run regressions separately to get labels for plot
  runreg <- function(l) {
    regdata <- data %>% filter(lcc == l)
    result <- summary(lm(paste("weighted_ccp", "~", "initial_returns"), regdata, weights = initial_acres))$coefficients
    result <- result %>% as.data.frame() %>% dplyr::select(c("Estimate","Std. Error"))
    colnames(result) <- c("estimate","se")
    result <- result[2,] %>%
                mutate(tstat = estimate/se)  %>% 
                pivot_longer(cols = c("estimate","se","tstat"), names_to = "Stat") %>%
                mutate(lcc = l)
    return(result)
    }

  labels <- do.call(rbind,lapply(unique(data$lcc), runreg)) %>%
          as.data.frame()
  labels$value <- prettyNum(as.numeric(labels$value), digits = 2, format = "fg")
  labels_wide <- labels %>% spread(key = "Stat", value = "value") %>%
                  mutate(label = sprintf("b = %s, t = %s",estimate,tstat),
                         x = max(data$initial_returns, na.rm = TRUE)*0.8, 
                         row = as.numeric(rownames(.)),
                         y  = 0.9 - (row-1)*0.05)

  # Make plot  
  p <- ggplot(data = data) +
    geom_point(aes(x = initial_returns, y = weighted_ccp, color = lcc, size = initial_acres), alpha = 0.75) +
    geom_smooth(data = data, aes(x = initial_returns, y = weighted_ccp, color = lcc, weight = initial_acres),
                method="lm",
                se = FALSE) +
    geom_text(data = labels_wide, aes(x = x, y = y, label = label, color = lcc)) +
    theme_minimal()    + 
    labs(x = sprintf("%s returns",i_use), y = "Weighted CCP",
         color = "LCC", size = "Initial acres",
         title = "Initial use returns vs. transition probability",
         subtitle = sprintf("Transitions from %s to %s, %s, N = %s", i_use, f_use, year, N))+
    ylim(0,1)

  p
  
  if (save == TRUE) savefig(sprintf("%s%s_to_%s_%s",outpath,i_use,f_use,year), width = 6*1.4, height = 4*1.4)
  
  return(p)
  }

i_uses = c("Crop","Forest","Other")
f_uses = c("Crop","Forest","Urban","Other")

lapply(i_uses, function(i) lapply(f_uses, function(f) make_plot(i_use = i, f_use = f, year = 2012, save = TRUE)))
