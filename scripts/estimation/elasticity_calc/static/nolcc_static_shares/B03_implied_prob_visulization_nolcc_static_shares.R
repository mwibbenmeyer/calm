###########################################################################
# Qinrui Xiahou
# Feb 28, 2022
# Script to create scatterplots and maps of predicted and actual conversions
# Data input: 1) implied_prob_static_shares.dta from value fn estimation
#             2) ddc_data_urbancal_st from combined data
###########################################################################

## Load/install packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse,
               usmap,
               ggplot2,
               foreign,
               haven,
               RColorBrewer,
               maps,
               imager,
               patchwork)
theme_set(theme_bw())


## Read in data
ddc_data_urbancal_st <- read_dta("L:/Project-Land_Use/processing/combined/ddc_data_urbancal_st.dta")
implied_prob_lcc_st <- read_dta("L:/Project-Land_Use/processing/elasticity/nolcc_static_shares/implied_prob_static_shares_2012.dta")


## Process the data
act_prob_st <- ddc_data_urbancal_st %>%
  filter(year == 2012,
         lcc != 0) %>%
  select(fips, lcc, initial_use, final_use, weighted_ccp)

pred_prob_st <- implied_prob_lcc_st %>%
  select(fips, initial_use, p1, p2, p3, p4, p5) %>%
  rename(Urban = p1,
         CRP = p2, 
         Crop = p3,
         Forest = p4,
         Other = p5) %>%
  pivot_longer(cols = c("Urban", "CRP", "Crop", "Forest", "Other"),
               names_to = "final_use",
               values_to = "implied_prob") # note that LCC is not available in the nolcc model

comb_prob_st <- act_prob_st %>%
  full_join(pred_prob_st, by=c("fips", "initial_use", "final_use"))

summary <- comb_prob_st %>%
  filter(is.na(implied_prob)==FALSE) %>%
  group_by(initial_use, final_use, lcc) %>%
  summarize(cor = round(cor(weighted_ccp, implied_prob),2))
  

## Visualize the data (scatterplots)

land_uses <- c("Urban", "CRP", "Crop", "Forest", "Other")

for (k in land_uses) {
  for (j in land_uses) {
    temp <- comb_prob_st[comb_prob_st$initial_use %in% k & comb_prob_st$final_use %in% j, ]
    temp %>%
      ggplot(aes(x=weighted_ccp, y=implied_prob)) +
      geom_point(alpha=0.5) +
      xlim(0, 1) +
      ylim(0, 1) +
      facet_wrap(~lcc, nrow=2) +
      labs(title = sprintf("Actual and predicted conversion probabilities by LCC: %s to %s", k, j))
    ggsave(sprintf("L:/Project-Land_Use/results/initial_descriptives/implied_probabilities/static/nolcc_shares/scatterplot_2012_%s_to_%s.png", k, j), height=8, width=12)
  }
}

temp <- comb_prob_st[comb_prob_st$initial_use %in% "Forest" & comb_prob_st$final_use %in% "Urban", ]
temp %>%
  ggplot(aes(x=weighted_ccp, y=implied_prob)) +
  geom_point(alpha=0.5) +
  facet_wrap(~lcc, nrow=2) +
  labs(title = sprintf("Actual and predicted conversion probabilities by LCC: %s to %s", "Forest", "Urban"))



## Visualize the data (density plots)

land_uses <- c("Urban", "CRP", "Crop", "Forest", "Other")

for (k in land_uses) {
  for (j in land_uses) {
    temp <- comb_prob_st[comb_prob_st$initial_use %in% k & comb_prob_st$final_use %in% j, ]
    temp %>%
      pivot_longer(cols=c("weighted_ccp", "implied_prob"), names_to="prob", values_to="value") %>%
      ggplot(aes(x=value, fill=prob)) +
      geom_density(alpha=0.5) +
      facet_wrap(~lcc, nrow=2) +
      labs(title = sprintf("Actual and predicted conversion probabilities by LCC: %s to %s", k, j))
    ggsave(sprintf("L:/Project-Land_Use/results/initial_descriptives/implied_probabilities/static/nolcc_shares/density_2012_%s_to_%s.png", k, j), height=8, width=12)
  }
}


## Visualize the data (maps)

layout <- "
AAA
AAA
BBB
BBB
"

LCCs <- c("1_2", "3_4", "5_6", "7_8")

for (k in land_uses) {
  for (j in land_uses) {
    for (i in LCCs) {
      
      temp2 <- comb_prob_st[comb_prob_st$initial_use %in% k & comb_prob_st$final_use %in% j & comb_prob_st$lcc %in% i, ]
      
      act_map <- plot_usmap(data = temp2, values = "weighted_ccp", color = "#E1F0FC", size=0.2, regions = "counties", exclude = c("AK","HI")) +
        scale_fill_viridis_c(limits=c(0, 1)) +
        labs(title = sprintf("Actual Conversions from %s to %s in LCC %s", k, j, i), fill = "Rate of conversion (smoothed)")
      pred_map <- plot_usmap(data = temp2, values = "implied_prob", color = "#E1F0FC", size=0.2, regions = "counties", exclude = c("AK","HI")) +
        scale_fill_viridis_c(limits=c(0, 1)) +
        labs(title = sprintf("Predicted Conversions from %s to %s in LCC %s", k, j, i), fill = "Implied probability")
      
      act_map + pred_map + plot_layout(design = layout)
      ggsave(sprintf("L:/Project-Land_Use/results/initial_descriptives/implied_probabilities/static/nolcc_shares/maps_2012_%s_to_%s_LCC%s.png", k, j, i), height=10, width=10)
      
    }
  }
}
