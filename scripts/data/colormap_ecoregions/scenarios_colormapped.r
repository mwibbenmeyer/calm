library(data.table)
library(dplyr)
library(tidyverse) 
library(plyr)
library(dtplyr)
library(ggplot2)
library(diagram)
library(reshape)
library(stringr)
library(knitr) #for kable()
library(readr)
library(urbnmapr)


path_in <- dirname(rstudioapi::getActiveDocumentContext()$path)
path_out <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# ecoregion = read_excel("county to ecoregion.xls", 
#                        sheet = "county to ecoregion", col_types = "text") %>% 
#   mutate(fips = str_pad(Fips, 5, side = "left", pad = '0')) %>% 
#   mutate(ecoregion = P) %>% select(fips, ecoregion)

# ecoregion = read_csv("All_county_ecocd.csv") %>% 
#   mutate(fips = str_pad(fips, 5, side = "left", pad = '0')) %>% select(fips, ecoregion = ecocd)

ecoregion = read_csv("interpolated_ecocd_counties.csv")


#manually input some missing definitions
# ecoregion[nrow(ecoregion) + 1,] = list('12086', '411') #Miami-Dade
# ecoregion[nrow(ecoregion) + 1,] = list('08014', '331') #Broomfield Colorado
# ecoregion[nrow(ecoregion) + 1,] = list('01101', '231') #Montgomery Alabama
ecoregion$fips[ecoregion$fips == '46113'] = '46102' #Oglala Lakota, SD
# ecoregion$ecoregion[ecoregion$fips == '06041'] = '261'

# 
# 
# ecoregion$ecoregion<-ifelse(ecoregion$ecoregion=='262','261',ecoregion$ecoregion)
# ecoregion$ecoregion<-ifelse(ecoregion$ecoregion=='263','261',ecoregion$ecoregion)
ecoregion$ecoregion<-ifelse(ecoregion$ecoregion=='334','331',ecoregion$ecoregion)
ecoregion$ecoregion<-ifelse(ecoregion$ecoregion=='322','313',ecoregion$ecoregion)
ecoregion$ecoregion<-ifelse(ecoregion$ecoregion=='342','341',ecoregion$ecoregion)
ecoregion$ecoregion<-ifelse(ecoregion$ecoregion=='321','313',ecoregion$ecoregion)
# ecoregion$ecoregion<-ifelse(ecoregion$ecoregion=='311','315',ecoregion$ecoregion)

states_sf = get_urbn_map(map = "states", sf = TRUE)
counties_sf = get_urbn_map(map = "counties", sf = TRUE)
mapping_data = merge(x = counties_sf, y = ecoregion, by.x = 'county_fips', by.y = 'fips')



colormap = mapping_data %>%
  ggplot() +
  geom_sf(mapping = aes(fill = ecoregion), color = NA) +
  coord_sf(datum = NA) +
  scale_fill_manual(name = "Ecoregions",
                    values = c(#East Coast
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
                               ),
                    labels=c(
                              "211" = "NE Mixed Forest",
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
                              ), na.value="black") 
colormap
ggsave('colormap_ecoregions.png',scale=2)





outfn<-"lu_change"
alt<-read.table(paste(path_in,'/',outfn,"_c_reg.csv",sep=""),header=TRUE, sep=",")

outfn<-"no_lu_change"
base<-read.table(paste(path_in,'/',outfn,"_c_reg.csv",sep=""),header=TRUE, sep=",")

comp<-merge(base,alt,by='Plabel')
comp$delt1<-comp$dC1.y-comp$dC1.x
comp$delt2<-comp$dC2.y-comp$dC2.x
comp$delt3<-comp$dC3.y-comp$dC3.x
comp$delt4<-comp$dC4.y-comp$dC4.x
comp$delt5<-comp$dC5.y-comp$dC5.x
comp$delt6<-comp$dC6.y-comp$dC6.x


dC_m<-melt(comp,measure.vars=c('delt1','delt2','delt3','delt4','delt5','delt6'))
dC_m<-as.data.table(dC_m)
dC_m<-dC_m[,c('Plabel','ecocd.x','variable','value')]
in_x<-c('delt1','delt2','delt3','delt4','delt5','delt6')
out_x<-c(2020,2025,2030,2035,2040,2045)
SCC_RFF<-c(0,185,205.5,226,244.5,263)
SCC_IWG<-c(0,53.00,58.00,64.00,69.00,74.00)
dC_m$year<-mapvalues(dC_m$variable, from=in_x, to=out_x)
dC_m$SCC_RFF<-mapvalues(dC_m$variable, from=in_x, to=SCC_RFF)
dC_m$SCC_IWG<-mapvalues(dC_m$variable, from=in_x, to=SCC_IWG)
dC_m$SCC_RFF<-as.numeric(as.character(dC_m$SCC_RFF))
dC_m$SCC_IWG<-as.numeric(as.character(dC_m$SCC_IWG))
dC_m$year<-as.numeric(as.character(dC_m$year))


disc<-0.03
dC_m<-transform(dC_m,val_c_rff=(value*44/12*SCC_RFF*5)/((1+disc)^(year-2020))/1e9)
dC_m<-transform(dC_m,val_c_iwg=(value*44/12*SCC_IWG*5)/((1+disc)^(year-2020))/1e9)

x<-dC_m[,lapply(.SD,sum),by=c('Plabel'),.SDcols=c('val_c_rff','val_c_iwg','value')]

sum(x$val_c_rff)
sum(x$val_c_iwg)

sum(5*x$value*44/12/1e6)  #net emissions in CO2 eq

    
#Collapse the difference in ecoregions for plotting

x$Plabel[which(x$Plabel == "Northeastern")] = 'Luarentian'
x$Plabel[which(x$Plabel == "Central Int Broadleaf")] = 'Midwest Broadleaf'

test = aggregate(x$val_c_rff, by=list(Plabel=x$Plabel), FUN=sum)

barchart = test %>%
  ggplot(aes(x,reorder(Plabel, -x), fill=Plabel)) + labs(title='Carbon value of land use change',x='billion $(2020)',y='Ecological province')+
  geom_col() +
  labs(fill="") +
  scale_fill_manual(values = c(
    "Luarentian" = "#006d2c",
    "E. Broadleaf " = "#41ae76",
    "Midwest Broadleaf" = "#66c2a4",
    "S.E. Mixed Forest" = "#00441b",
    "Outer Coastal Plain" = "#238b45",
    "Lower Miss. Riverine" = "#e5f5f9",
    "Prarie Parkland-T" = "#ccece6",
    "Prarie Parkland-ST" = "#99d8c9",
    
    #Great Plains
    "Southern Rockies/GP" = "#feedde",
    "Middle Rockies" = "#fdbe85",
    "SW Plateau" = "#fd8d3c",
    
    #Desert & Rockies
    "SW semi desert " = "#fec44f",
    "Int Mtn semi desert" = "#fe9929",
    "Northern Rockies" = "#cc4c02",
    
    #West Coast
    "Cascades" = "#3182bd",
    "Sierras" = "#9ecae1"), guide="none") + theme_minimal()
barchart
ggsave('colormap_Csink_merged.png',scale=2)


colormap_nolabels = mapping_data %>%
  ggplot() +
  geom_sf(mapping = aes(fill = ecoregion), color = NA) +
  coord_sf(datum = NA) +
  scale_fill_manual(name = "Ecoregions",
                    values = c(#East Coast
                      "212" = "#006d2c",
                      "221" = "#41ae76",
                      "222" = "#66c2a4",
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
                      "311" = "#d94701", #This is still not merged
                      
                      #Desert & Rockies
                      #"321" = "#ffffd4",
                      #"322" = "#fee391",
                      "313" = "#fec44f",
                      "341" = "#fe9929",
                      #"342" = "#ec7014",
                      "333" = "#cc4c02",
                      #"334" = "#8c2d04",
                      
                      #West Coast
                      "242" = "#3182bd",
                      "261" = "#9ecae1"
                      #"262" = "#deebf7"
                    ), guide="none") + theme(
                      panel.background = element_rect(fill='transparent'), 
                      plot.background = element_rect(fill='transparent', color=NA),
                      panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(),
                      legend.background = element_rect(fill='transparent'),
                      legend.box.background = element_rect(fill='transparent'))
colormap_nolabels

barchart + annotation_custom(ggplotGrob(colormap_nolabels), xmin=-26, xmax=-11, ymin=-10, ymax=19)
ggsave('barchart_colormap.png',scale=2)

