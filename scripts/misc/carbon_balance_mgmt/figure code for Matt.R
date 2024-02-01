
library(tidyverse)
library(ggplot2)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) ; setwd("../../../")

#Forest area plot:  (use csv: forest_summary_status_quo.csv)

x_forest <- read.csv("processing/misc/carbon_balance/forest_summary_status_quo.csv")  

pl1<-ggplot(x_forest, aes(x = year, y = value/1e6,colour= variable)) + facet_wrap(~broad,nrow=1)+
  geom_line(size=.8)+ theme_bw(base_size = 12)+
  theme(legend.position = "right")+ 
  labs(title='',y='acres (millions)',x='year',color='Forest area') +
  scale_colour_manual(values = c(rffblue,rffred,rffblack))

#Land use change plots: (use csv:lu_summary_status_quo.csv)

x <- read.csv("processing/misc/carbon_balance/lu_summary_status_quo.csv") %>% as.data.table()  %>% 
  mutate(initial_use = recode(initial_use, "Urban" = "settlements",
                                            "Crop" = "crop",
                              "Other" = "other",
                              "Forest" = "forest"),
         initial_use = factor(initial_use, levels = c("crop","forest","settlements","other")))

x_broad<-x[,lapply(.SD,sum),by=list(broad,initial_use,year),.SDcols=c('acres')] 

pl2<-ggplot(x_broad, aes(x = year, y = acres/1e6, colour= initial_use)) + facet_wrap(~broad,nrow = 1,scales='free')+
  geom_line(size=.8)+ theme_bw(base_size = 12)+
  theme(legend.position = "right")+ 
  labs(title='',y='acres (millions)',x='year',color='Land use') +
  scale_colour_manual(values = c(rffblack,rffgreen,rffblue,rffred))


# put plots together:

pl1 / pl2 + plot_annotation(tag_levels = 'A') &
  theme(plot.tag = element_text(size = 24))

savefig(path = "results/misc/carbon_balance_mgmt/trends_by_region", width = 6.5*1.75, height = 4*1.75)
