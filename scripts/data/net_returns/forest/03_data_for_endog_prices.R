setwd("C:/Users/19193/Documents/Projects/RPA_Harvest/newtake/")

library(data.table)
library(plyr)
library(ggplot2)
library(gridExtra)
library(dplyr)
library(broom)

# READ subplot RECORDS by species FROM FIA--note that these include multiple remeasurements

RD<-read.table("harvest_choice_east_full_withOwner.csv",header=TRUE, sep=",")   # raw data includes species, condition level data
RD$t1_plt_cn<-as.factor(RD$t1_plt_cn)
RD$t2_plt_cn<-as.factor(RD$t2_plt_cn)
RD$t2_condid<-as.factor(RD$t2_condid)
RD$t1_condid<-as.factor(RD$t1_condid)
RD$PID<-RD$plot+RD$countycd*1e5+RD$statecd*1e8					# use this plot id code to isolate most recent inventory

length(unique(RD[["t1_plt_cn"]]))
length(unique(RD[["PID"]]))

setnames(RD,"statecd","sfips")
RD<- transform(RD, county=countycd+sfips*1e3)
RD$fips<-RD$county

regions<-read.table("state_to_region.csv",header=TRUE, sep=",") 		#bring in subregion cross walk to states
RD<-merge(RD, regions, by="sfips",all.X=TRUE)

ecoregions<-read.table("interpolated_ecocd_counties.csv",header=TRUE, sep=",") 	#bring in CALM ecoregion for counties
RD<-merge(RD, ecoregions, by="fips",all.X=TRUE)

RD$comm<-ifelse(RD$t1_actual_owncd==41,1,0)					#commercial owner
RD$federal<-ifelse((RD$t1_actual_owncd>10 & RD$t1_actual_owncd<26),1,0)		#federal owner

RD$soft<-ifelse(RD$spcd<300,1,0)
RD$harv_code<-ifelse((RD$t2_trtcd1==10|RD$t2_trtcd2==10|RD$t2_trtcd3==10),1,0)	# harvest code
RD$D_slope<-ifelse(RD$t1_slope>30,1,0)					        # slope dummy
RD<-transform(RD, opriv=abs(1-comm-t1_public))  			        # non-comm private owner

prices<-read.table("all_prices.csv",header=TRUE, sep=",") 			# bring in prices by region and species
RD_p<-merge(RD,prices, by=c('subregion','spcd'), all.X=TRUE)
RD_p$saw_p<-ifelse(is.na(RD_p$saw_p),0,RD_p$saw_p)
RD_p$pulp_p<-ifelse(is.na(RD_p$pulp_p),0,RD_p$pulp_p)

RD_p$sptype<-ifelse(RD_p$spcd<300,"softwood","hardwood")  			# softwood vs hardwood
RD_p$softwood<-ifelse(RD_p$spcd<300,1,0)
RD_p$hardwood<-ifelse(RD_p$spcd>300,1,0)

# core output variables===>

RD_p$rem_saw_cf<-RD_p$t2_sawtimber_removal
RD_p$rem_pulp_cf<-RD_p$t2_grw_stk_removal-RD_p$t2_sawtimber_removal
#  --convert cf to mbf assumes 12 bf/cf
   sc= 12/1000
#  --convert cf to tons (tons/cf) (17.75 tons/mcf averaging hardwoods (20) and softwoods (15.5)) .035 from MT study
   pc= .035
RD_p$rem_saw_mbf<-RD_p$rem_saw_cf*sc
RD_p$rem_pulp_gt<-RD_p$rem_pulp_cf*pc
RD_p$rev_saw<-RD_p$rem_saw_mbf*RD_p$saw_p
RD_p$rev_pulp<-RD_p$rem_pulp_gt*RD_p$pulp_p
RD_p$farea<-RD_p$expns
RD_p$inv_gs_cf<-RD_p$t1_grw_stk_vol
RD_p$reserve<-ifelse(RD_p$t1_reservcd==0,1,0)

#First-- screen plots to include only most recent measurement (plt_cn)

result <- RD_p%>% 									#filter to define latest plot measurements
             group_by(PID) %>%
             filter(t2_measyear == max(t2_measyear))

x<-as.data.frame(result)
x<-as.data.table(x)


of_county_species<-x[,list( 	inv_gs_cf=sum(inv_gs_cf*(expns),na.rm=TRUE),		#summarize removals and revenues by species by county
				rem_saw_mbf=sum(rem_saw_mbf*(expns)/remper,na.rm=TRUE),
				rem_pulp_gt=sum(rem_pulp_gt*(expns)/remper,na.rm=TRUE),
				rev_saw=sum(rev_saw*(expns)/remper,na.rm=TRUE),
				rev_pulp=sum(rev_pulp*(expns)/remper,na.rm=TRUE),
				pulp_p=mean(pulp_p,na.rm=TRUE),
				saw_p=mean(saw_p,na.rm=TRUE),
                                total_rem_cf=sum(t2_grw_stk_removal*(expns)/remper,na.rm=TRUE)),
                                by=list(fips,spcd)]
colSums(Filter(is.numeric, of_county_species))
write.csv(of_county_species,file=paste("county_species_product_harvestandprice.csv",sep=""))

of_county_soft_hard<-x[,list( 	inv_gs_cf=sum(inv_gs_cf*(expns),na.rm=TRUE),		#summarize removals and revenues by forest type by county
				rem_saw_mbf=sum(rem_saw_mbf*(expns)/remper,na.rm=TRUE),
				rem_pulp_gt=sum(rem_pulp_gt*(expns)/remper,na.rm=TRUE),
				rev_saw=sum(rev_saw*(expns)/remper,na.rm=TRUE),
				rev_pulp=sum(rev_pulp*(expns)/remper,na.rm=TRUE),
				pulp_p=mean(pulp_p,na.rm=TRUE),
				saw_p=mean(saw_p,na.rm=TRUE),
                                total_rem_cf=sum(t2_grw_stk_removal*(expns)/remper,na.rm=TRUE)),
                                by=list(fips,sptype)]
colSums(Filter(is.numeric, of_county_species))
write.csv(of_county_soft_hard,file=paste("county_type_product_harvestandprice.csv",sep=""))

of_plot_totals<-x[,list( 	inv_gs_cf=sum(inv_gs_cf*(expns),na.rm=TRUE),		#summarize removals and revenues by plots
                                farea=mean((expns*reserve),na.rm=TRUE),
				rem_saw_mbf=sum(rem_saw_mbf*(expns)/remper,na.rm=TRUE),
				rem_pulp_gt=sum(rem_pulp_gt*(expns)/remper,na.rm=TRUE),
				rev_saw=sum(rev_saw*(expns)/remper,na.rm=TRUE),
				rev_pulp=sum(rev_pulp*(expns)/remper,na.rm=TRUE),
                                total_rem_cf=sum(t2_grw_stk_removal*(expns)/remper,na.rm=TRUE),
				remper=mean((remper),na.rm=TRUE)),
                                by=list(PID,USPS,fips,ecoregion)]
of_county_totals<-of_plot_totals[,list( total_inventory_cf=sum(inv_gs_cf,na.rm=TRUE),		#summarize removals and revenues by county
                                for_area=sum(farea,na.rm=TRUE),
				rem_saw_mbf=sum(rem_saw_mbf,na.rm=TRUE),
				rem_pulp_gt=sum(rem_pulp_gt,na.rm=TRUE),
				rev_saw=sum(rev_saw,na.rm=TRUE),
				rev_pulp=sum(rev_pulp,na.rm=TRUE),
                                total_rem_cf=sum(total_rem_cf,na.rm=TRUE)),
                                by=list(fips,USPS,ecoregion)]
of_county_totals$rev_pa<-(of_county_totals$rev_saw+of_county_totals$rev_pulp)/of_county_totals$farea
of_county_totals$lval<-(of_county_totals$rev_pa/0.04)					#county level lval estimates....DR=4%
colSums(Filter(is.numeric, of_county_totals))
write.csv(of_county_totals,file=paste("county_totals.csv",sep=""))

of_ecoregion_totals<-of_plot_totals[,list( total_inventory_cf=sum(inv_gs_cf,na.rm=TRUE),		#summarize removals and revenues by ecoregion
                                for_area=sum(farea,na.rm=TRUE),
				rem_saw_mbf=sum(rem_saw_mbf,na.rm=TRUE),
				rem_pulp_gt=sum(rem_pulp_gt,na.rm=TRUE),
				rev_saw=sum(rev_saw,na.rm=TRUE),
				rev_pulp=sum(rev_pulp,na.rm=TRUE),
                                total_rem_cf=sum(total_rem_cf,na.rm=TRUE)),
                                by=list(ecoregion,USPS)]
of_ecoregion_totals$rev_pa<-(of_ecoregion_totals$rev_saw+of_ecoregion_totals$rev_pulp)/of_ecoregion_totals$farea
of_ecoregion_totals$lval_e<-(of_ecoregion_totals$rev_pa/0.04)				#ecoregion level lval estimates....DR=4%
colSums(Filter(is.numeric, of_ecoregion_totals))
write.csv(of_ecoregion_totals,file=paste("ecoregion_totals.csv",sep=""))

x2<-of_ecoregion_totals[,c('lval_e','USPS','ecoregion')]
x3<-merge(of_county_totals,x2, by=c('ecoregion','USPS'))
x3$lval_s<-x3$lval
x3$lval_s<-ifelse(x3$lval_s<0.1,x3$lval_e,x3$lval_s)

write.csv(x3,file=paste("county_totals_with_imputed_missing.csv",sep=""))

county_lval_estimates<-x3

of_state_totals<-of_plot_totals[,list( 	  inv_gs_cf=sum(inv_gs_cf,na.rm=TRUE),		#summarize by state to compare with published results
                                  farea=sum(farea,na.rm=TRUE),
                                  total_rem=sum(total_rem_cf,na.rm=TRUE)),
                                  by=list(USPS)]
# For checking against national reporting:
of_state_totals
colSums(Filter(is.numeric, of_state_totals))

(pl1<-ggplot(x3, aes(x=lval_s)) + xlim(0,3000)+
 geom_histogram(binwidth = 200) + facet_wrap(~ecoregion)
) 


