# load/install packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse,
               sf,
               maps,
               rnaturalearth,
               rnaturalearthdata,
               tidycensus,
               ggplot2,
               usmap,
               patchwork,
               scales,
               colorspace)
theme_set(theme_bw())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)); setwd('../../../') # relative paths to move directory to the root project directory

{
  # colors
  rffblue <- "#88C4F4"
  rffred <- "#FF6663"
  rffgreen <- "#50B161"
  rffbrown <- "#74645E"
  rffblack <- "#04273C"
  rffpurple <- "#755EA6"
  rffyellow <- "#EAD367"
  rfforange <- "#F4A25F"
  
  ecoregion_colors <- c(#East Coast
    "211" = "#006d1a",
    "212" = "#0d572b",
    "221" = "#41ae76",
    "222" = "#66c2a4",
    "223" = "#449178",
    "231" = "#00441b",
    "232" = "#238b45",
    "234" = "#e5f5f9",
    "251" = "#ccece6",
    "255" = "#99d8c9",
    "411" = "#f7fcfd",
    
    #Great Plains
    "331" = "#feedde",
    "332" = "#fdbe85",
    "315" = "#fd8d3c",
    # "311" = "#d94701",
    
    #Desert & Rockies
    # "321" = "#ffffd4",
    # "322" = "#fee391",
    "313" = "#fec44f",
    "341" = "#fe9929",
    # "342" = "#ec7014",
    "333" = "#cc4c02",
    # "334" = "#8c2d04",
    
    #West Coast
    "242" = "#3182bd",
    "261" = "#9ecae1",
    "262" = "#deebf7",
    "263" = "#a5b8c9"
  )
  
  ecoregion_labels <- c("211" = "NE Mixed Forest",
                        "212" = "Laurentian",
                        "221" = "E. Broadleaf",
                        "222" = "Midwest Broadleaf",
                        "223" = "Central Int. Broadleaf",
                        "231" = "S.E. Mixed Forest",
                        "232" = "Outer Coastal Plain",
                        "234" = "Lower Miss. Riverine",
                        "251" = "Prarie Parkland-T",
                        "255" = "Prarie Parkland-ST",
                        "411" = "Everglades", #Missing from Dave
                        
                        #Great Plains
                        "331" = "Southern Rockies/GP",
                        "332" = "Middle Rockies",
                        "315" = "SW Plateau",
                        # "311" = "#d94701",
                        
                        #Desert & Rockies
                        # "321" = "#ffffd4",
                        # "322" = "#fee391",
                        "313" = "SW Semi Desert", 
                        "341" = "Int Mtn Semi Desert",
                        # "342" = "#ec7014",
                        "333" = "Northern Rockies",
                        # "334" = "#8c2d04", 
                        
                        #West Coast
                        "242" = "Pacific Cascade Mixed",
                        "261" = "Sierras Mixed Forest",
                        "262" = "CA Dry Steppe",
                        "263" = "CA Coastal Forest" 
  )
}


df <- read.csv("raw_data/net_returns/forest/forest_plot_rents/plot_rents_by_county_South.csv") %>% 
  mutate(county = str_pad(county, width = 5, pad = "0", side = "left"))

ecoregions <- read.csv("processing/misc/ecoregions/interpolated_ecocd_counties.csv") %>% 
  mutate(fips = str_pad(fips, width = 5, pad = "0", side = "left"))


simulation <- function(Cv, sim_iters, data = df) {

  estimate_comparison <- function(Cval, i) {
  
    # Set simulation parameters 
    target_rate <- 0.01
    
    #Generate data set with random error terms
    df2 <- df %>% mutate(e_f = log(-log(runif(dim(df)[1]))),
                        e_c = log(-log(runif(dim(df)[1]))),
                        e_diff = e_c - e_f)
    
    calc_beta <- function(C) {
    
      #Note: C must not be too small or function will converge incorrectly.
      
      # Set initial values simulation
      
      diff <- 1e6
      b_guess <- 1
      change <- 0.5
      step <- 1
      slope = NA
      broken <- 0
      
      # Iterate 
      
      while (abs(diff) > 1e-3) {
        
        df2 <- df2 %>% mutate(V = b_guess*c_rent - C,
                            convert = as.numeric(e_diff < V),
                            prob = exp(V)/(1+exp(V)),
                            prob = ifelse(prob == 1, 1-1e-6, prob),
                            logl = convert*log(prob) + (1-convert)*log(1-prob))
        
        logL = sum(df2$logl, na.rm = T)
        
        diff <- target_rate - mean(df2$convert)
        
        if (step == 1) { increment <- 0.5*sign(diff)
        } else if (step != 1) {
          slope <- -(diff - diff_prev)/increment
          increment <- diff/slope
        }
        
        if (step > 1 & slope == 0) { 
          break
          broken <- 1
        }
        
        b_guess <- max(b_guess + increment,0)
        diff_prev <- diff
        step <- step + 1 
        
        
      }
    
      if (broken == 0) { return(list(C, b_guess, logL)) 
      } else {
          print("Did not converge")
        }
      
    }
    
    # ans_df <- do.call(rbind, lapply(seq(200,5000,100), calc_beta)) %>% 
    #         as.data.frame()
    # ggplot(data = ans_df) + geom_point(aes(x = as.numeric(V1), y = as.numeric(V3)))
    
    
    ans <- calc_beta(Cval) %>% unlist()
    
    df2 <- df2 %>% mutate(V = ans[[2]][1]*c_rent - ans[[1]][1],
                        convert = as.numeric(e_diff < V))
    
    indiv <- glm(convert ~ c_rent, 
                      family = binomial(link = "logit"), 
                      data = df2)
    
    county <- glm(convert ~ c_rent, 
                  family = binomial(link = "logit"), 
                  data = df2 %>% group_by(county) %>% mutate(c_rent = mean(c_rent)))
    
    p <- predict(county, type = "response")
    
    results <- data.frame(c("intercept","c_rent"),cbind(ans[1:2],indiv$coefficients,county$coefficients))
    colnames(results) <- c("var","true","indiv","county")
    results <- results %>% mutate(iter = i)
    
    return(results)
    
  }

  
  result <- do.call(rbind, lapply(seq(1,sim_iters), estimate_comparison, Cval = Cv)) %>% 
              pivot_longer(cols = c("indiv","county"), 
                          names_to = "aggregation",
                          values_to = "estimate") %>% 
              mutate(true = ifelse(var == "intercept",-true,true),
                     aggregation = factor(aggregation, levels = c("county","indiv")),
                     aggregation = recode(aggregation, "indiv" = "Plot",
                                          "county" = "County"),
                     var = factor(var, labels = c("beta[F]","beta[CF]")))
  
  
  means <- result %>% group_by(var) %>% summarize(true_val = mean(true)) %>% 
            mutate(var = factor(var, labels = c("beta[F]","beta[CF]")))
  p <- ggplot(data = result ) + 
              stat_density(aes(x = estimate, color = aggregation, y = ..ndensity..),
                           geom = "line",lwd = 1) + 
              geom_vline(data = means, aes(xintercept = true_val),
                         lwd = 1, linetype = "dashed") + 
              facet_wrap(~var, scales = "free", labeller = label_parsed) +
              scale_color_manual(values = c(darken(rffblue,0.25),rffred)) +
              labs(x = "Estimate",
                   y = "Density (scaled)",
                   color = "Aggregation") +
              theme_minimal() +
              theme(strip.text = element_text(size = 12))
  p

  return(list(result, p))
  
}

make_table <- function(results, filename, dst) {

  r <- lapply(seq(1,length(results)), function(x) results[[x]][1])
  
  table_contents <- do.call(rbind, lapply(seq(1,length(r)), function(x) 
                                          r[[x]][[1]] %>% as_tibble() %>% 
                                            group_by(var,aggregation) %>% 
                                            rename(variable = var ) %>% 
                                            summarize(mean = mean(estimate),
                                                      var = var(estimate),
                                                      true = mean(true)) %>%
                                            mutate(row = x))) %>% 
                      pivot_wider(names_from = c("aggregation"), values_from = c("mean","var")) %>% 
                      # filter(variable == "beta[F]") %>% 
                      mutate(County_bias = mean_County - true,
                             Plot_bias = mean_Plot - true,
                             se_Plot = var_Plot^(1/2),
                             se_County = var_County^(1/2),
                            biaspct_Plot = Plot_bias/se_Plot,
                            biaspct_County = County_bias/se_County) %>%  
                      dplyr::select(true, mean_Plot, se_Plot, biaspct_Plot, mean_County, se_County, biaspct_County) %>% 
                      mutate(variable = recode(variable, "beta[F]" = "\T $\\beta_F$",
                                                        "beta[CF]" = "$\\beta_{CF}$")) %>%
                      mutate(across(where(is.numeric), formatC, digits = 2, width = 3, format = "fg", big.mark = ","))
  
  table_contents        
  
  dir.create(dst, recursive = TRUE, showWarnings = FALSE)
  write.table(table_contents, file = paste0(dst,filename),
              quote = FALSE, col.names = TRUE, row.names = FALSE,
              sep = '&', eol = '\\\\ \n')
  
}

result <- simulation(100, iters = 100)

results <- lapply(c(10,20,50,100,200), simulation, iters = 100)
make_table(results, dst = 'results/misc/returns_heterogeneity/table_simulations/', filename = "table_contents.tex")

# Compare to results when obs with returns > median return are dropped

med <- median(df$c_rent)
results <- lapply(c(10,20,50,100,200), simulation, sim_iters = 100,
                  data %>% filter(c_rent < med))
make_table(results, dst = 'results/misc/returns_heterogeneity/table_low_returns/', filename = "table_contents.tex")


