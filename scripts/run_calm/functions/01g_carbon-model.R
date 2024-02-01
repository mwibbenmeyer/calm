#  FILENAME: 	CALM_carbon_projection.txt
#  Progrmamer:  David Wear
#  System:  	Part of the CALM modeling framework
#  Function: 	Generates net carbon emissions for the US LULUCF sector
#  Inputs: 	Land use changes for a scenario from the LU modeling component
#		Carbon yield estimates and age transition matrices for each ecoregion/forest type/management class
#  Outputs:  	Estimates of nonforest and forest GHG sinks in the United States, including: 
#		Summary of afforestation and deforestation by ecoregion (csv's) for forest carbon projections
#		Detailed forest carbon dynamics for each ecoregion/forest type/management class 
#  Structure:   Three main functions executed in sequence:
#		landuse: processes land use projections from LU model
#		f_carbon: defines carbon flux in forests by time step using carbon yields, age transition matrices, land use
#		land_carb: combines forest and nonforest projections to define change in carbon sinks by GHGI categories.
#  Notes:	5-year time step, 50 year projection period
#		Methodology described in Wear and Wibbenmeyer (2023)
#		https://www.rff.org/publications/working-papers/land-use-change-no-net-loss-policies-and-effects-on-carbon-dioxide-removals/ 


#################################################################
# Processes land use projections from LU model to feed forest carbon estimates, defines non-forest carbon flux

landuse <- function(inputs, # A list of dataframes describing land use conversions in each period, by ecoregion
                  scenario_name = NA, # Name of scenario. If NA, output is not written to disk.
                  intensify = FALSE, 
                  net = FALSE,       # switches for special run--ordinarily turned off:
                  no_luc = FALSE)    # net--for a net land use change approach, no_luc--holds land uses constant
  {
  
  #----------------------------------------------------------------
  #  Define national average nonforest carbon flux coefficients by lu and luc
  #  net emission coefficients are derived from US GHG inventory (EPA)
  
  luc<-c('Other_Other','Crop_Crop','Urban_Urban','Other_Crop','Other_Urban','Crop_Other',
  	      'Crop_Urban','Urban_Crop','Urban_Other')
  coeff<-c(0.0161,-0.1565,-3.23737,.19034,.92482,-.3253,
          .6525,0,0)
  den<-data.frame(luc, coeff)		# densities are measured as MMT CO2 eq 
  den$coeff<-den$coeff/(2.47*1000)        # convert from mha to acre densities
  
  #----------------------------------------------------------------
  # Input land use change files (by timestep)
  
  for (i in 1:length(inputs)){                                                 #loop over time steps (i)
    
    lu_delt<- inputs[[i]]
    lu_delt$ecocd<-lu_delt$ecoregion
    lu_delt<-as.data.table(lu_delt)
    lu_delt<-lu_delt[!(lu_delt$ecocd==0),]
    lu_delt$ecocd<-ifelse(lu_delt$ecocd==262,261,lu_delt$ecocd)	#recode some ecological regions
    #lu_delt$ecocd<-ifelse(lu_delt$ecocd==263,261,lu_delt$ecocd)	
    lu_delt$ecocd<-ifelse(lu_delt$ecocd==334,331,lu_delt$ecocd)
    lu_delt$ecocd<-ifelse(lu_delt$ecocd==322,313,lu_delt$ecocd)
    lu_delt$ecocd<-ifelse(lu_delt$ecocd==342,341,lu_delt$ecocd)
    lu_delt$ecocd<-ifelse(lu_delt$ecocd==321,313,lu_delt$ecocd)
    #lu_delt$ecocd<-ifelse(lu_delt$ecocd==315,311,lu_delt$ecocd)

  if(no_luc & i==1){						# special case: set up a no land use change scenario	
  	lu_t1<-lu_delt[,lapply(.SD,sum),by=list(ecoregion,initial_use),.SDcols=c("acres")]
  	lu_no<-merge(lu_delt,lu_t1,by=c('ecoregion','initial_use'),all.x=TRUE)
  	lu_no$acres.x<-ifelse(lu_no$initial_use==lu_no$final_use,lu_no$acres.y,0)
  	lu_no<-lu_no[,1:5]
  	colnames(lu_no)<-colnames(lu_delt)	
     }
  if(no_luc) { lu_delt<-lu_no}
  
  #====== Define nonforest land use c flux for time step
  
    lu_delt$luc<-paste(lu_delt$initial_use,"_",lu_delt$final_use,sep="")
    lu_delt_nf<-lu_delt[!(lu_delt$initial_use=='Forest' | lu_delt$final_use=='Forest'),]	# 3 x 3 nonforest matrix
  
    x<-merge(lu_delt_nf,den, by='luc', all.x=TRUE)
    x$flux<-x$acres*x$coeff
    sum(x$flux)
  
    x_sums<-data.table(x)[,list(area=sum(acres),flux=sum(flux)),by=c('luc')]
    a_sums<-data.table(lu_delt)[,list(area=sum(acres)),by=c('luc')]
    x_sums$t<-i
    a_sums$t<-i
    lu_delt$t<-i
  
  #====== accumulate outputs over loop
  if(i==1){nf_flux<-x_sums}else{
  	nf_flux<-rbind(nf_flux,x_sums)}
  if(i==1){area_flux<-a_sums}else{
  	area_flux<-rbind(area_flux,a_sums)}
  if(i==1){lu_delt_agg<-lu_delt}else{
  	lu_delt_agg<-rbind(lu_delt_agg,lu_delt)}
  
  #====== summarize forest dynamics (parse to afforestation and deforestation)
  
  lu_delt_f<-lu_delt[(lu_delt$initial_use=='Forest' | lu_delt$final_use=='Forest'),]	   # forest matrix elements
  lu_delt_f<-lu_delt_f[!(lu_delt_f$initial_use=='Forest' & lu_delt_f$final_use=='Forest'),]  # remove FRF
  
  def<-lu_delt_f[(lu_delt_f$initial_use=='Forest'),]
  aff<-lu_delt_f[(lu_delt_f$final_use=='Forest'),]
  
  def_tot<-data.table(def)[,list(area=sum(acres)),by=c('ecocd')]
  aff_tot<-data.table(aff)[,list(area=sum(acres)),by=c('ecocd')]
  def_tot$t<-i
  aff_tot$t<-i
  
  #====== accumulate outputs over loop
  if(i==1){f_gain<-aff_tot}else{
    f_gain<-rbind(f_gain,aff_tot)}
  if(i==1){f_loss<-def_tot}else{
    f_loss<-rbind(f_loss,def_tot)}
  
  } #end loop over time steps (i)
  
  #----------------------------------------------------------------
  #  Summarize forest losses and gain by ecoregion
  
  f_loss$area<-f_loss$area*1000
  f_gain$area<-f_gain$area*1000
  
  f_loss_c<-dcast(f_loss,value.var="area",ecocd~t)
  colnames(f_loss_c)<-c('ecocd',paste0("l",seq(1,length(inputs))))
  f_loss_c$ecocd<-as.numeric(f_loss_c$ecocd)
  loss<-f_loss_c
  
  f_gain_c<-dcast(f_gain,value.var="area",ecocd~t)
  colnames(f_gain_c)<-c('ecocd',paste0("g",seq(1,length(inputs))))
  f_gain_c$ecocd<-as.numeric(f_loss_c$ecocd)
  gain<-f_gain_c
  
  if(net){						# special case (net=T)
    loss[,2:11]<-loss[,2:11]-gain[,2:11]
    gain[,2:11]<-0
  }
  if(no_luc){						# special case (no_luc=T)
    gain[,2:11]<-0
    loss[,2:11]<-0
  }

  #----------------------------------------------------------------
  # write output files
  if (!is.na(scenario_name)) {
  	write.csv(gain,file=paste(output_path, scenario_name,"/forest_gain.csv",sep=""))
  	write.csv(loss,file=paste(output_path, scenario_name, "/forest_loss.csv",sep=""))
  	write.csv(nf_flux,file=paste(output_path, scenario_name, "/nf_flux",".csv",sep=""))
  }
  
  #----------------------------------------------------------------
    lu_output<-list("gain" = gain,"loss" = loss,"nf_flux" = nf_flux)
    return(lu_output)

  } #end function landuse


#################################################################
# Defines carbon flux in forests by time step using carbon yields, age transition matrices, land use

f_carbon<-function(f_gain,f_loss,
                   scenario_name = NA,
                   intensify = FALSE, 
                   net = FALSE,       # switches for special run--ordinarily turned off:
                   no_luc = FALSE)
  {
  
  #----------------------------------------------------------------
  #  dimension the carbon density output file
   age<-seq(from=5,to=220, by = 5)
   c_den_out<-as.data.frame(age)
   
  #----------------------------------------------------------------
  # input carbon models for all ecoregions/forest types/ownership/origin code 
  # All_files.csv is the list of relevant combinations (~79)
  
  carbon_models <- read.table(paste(input_path,"carbon_models/All_files.csv",sep=""),header=TRUE, sep=",")
  for (i in 1:nrow(carbon_models)){				#loop i: over number of carbon models (region, origin, type, owner)
   cm<-carbon_models[i,]
    eco_cd<-cm$ecocd
    f_typ<-cm$ftype
    fed_code<-cm$fed_cd
    origin_code<-cm$origin_cd
    plabel<-cm$plabel
    region<-cm$region
  
  #====== unpack model components: tm is transition matrix, cden is carb density by age, A_0 is initital age dist,
  #       eco_area is total area in eco region,soft_area is area of softwoods
    c_mod<-readRDS(file=paste(path_carb,'C_',eco_cd,'_',f_typ,'_fed_',fed_code,'_origin_',origin_code,'.rds',sep=""))
    cden<-c_mod$cden
    A_0<-c_mod$A_0
    tm<-c_mod$tm
    eco_area<-c_mod$eco_area
    a_share<-sum(A_0)/eco_area
    soft_area<-c_mod$soft_area
    s_share<-soft_area/eco_area  
  
  #====== assign gains and losses to forest types--based on processed LU change scenario-- 
  #       following code assigns all forest gains to planted forests where planted forests are modeled (NA indicates no planted forests)
  
  if(is.na(origin_code)){origin_code<-3}	
  if(intensify){
  	loss_x<-f_loss[(f_loss$ecocd==eco_cd),]*a_share	
          if(eco_cd==231 | eco_cd==232 |eco_cd==263 |eco_cd==262 |eco_cd==241 |eco_cd==261){
  	      if(origin_code==1 & fed_code==0){
  		 gain_x<-f_gain[(f_gain$ecocd==eco_cd),]*1.0	
  	      }else{						
  		 gain_x<-f_gain[(f_gain$ecocd==eco_cd),]*0.0	}
          }else{						
  	      gain_x<-f_gain[(f_gain$ecocd==eco_cd),]*a_share	}			
  }else{
  	loss_x<-f_loss[(f_loss$ecocd==eco_cd),]*a_share			# prorate the area change to ecoregion components based on area share
  	gain_x<-f_gain[(f_gain$ecocd==eco_cd),]*a_share			# this can be adjusted to focus change on a subset of areas based on ftype/origin
  }

    #====== build out age structure of forests for 10, 5-yr time steps (50 years)
    A<-matrix(0,nrow=dim(f_loss)[2],ncol=ncol(tm))
    A[1,]<-A_0	
    for (j in 2:dim(f_loss)[2]){
  
    loss<-matrix(0,nrow=1,ncol=ncol(tm))					# loss and gains by age class
    gain<-matrix(0,nrow=1,ncol=ncol(tm))
  
  	gain[,1]<-gain_x[,j+1]						# all gain in first age class
      	A_tot<-sum(A[j-1,])
  	loss[1,]<-loss_x[,j+1]*(A[j-1,]/A_tot)				# losses across all age classes proportional to ac distribution 
      A[j,]<-A[j-1,] %*% tm						# apply transition matrix to age/disturb forests
      A[j,]<-A[j,]+gain-loss	
    }
  #====== Estimate carbon stocks and sinks
    gain_yr<-as.matrix(gain_x[1,2:dim(f_gain)[2]])
    loss_yr<-as.matrix(loss_x[1,2:dim(f_loss)[2]])
    Atot<-rowSums(A)
  
    C<-A%*%(cden)								# defines carbon stocks (C)
    lamb<-.95
    if(eco_cd>240){lamb<-.99}						#lamb defines share of soil carbon at age 1
    C_soil_t<-t(loss_yr[1,]*cden[1])-t(gain_yr[1,]*cden[1])*lamb  	# net soil C transfer with land use changes
  
    C<-as.data.table(C)					
    colnames(C)<-c('C')
    C<-C[, lag.value:=c(NA, C[-.N])]
    C<-cbind(C,Atot)
    colnames(C)<-c('C','C_lag','area')
  
    C$C_soil_t[2:dim(f_gain)[2]]<-as.matrix(C_soil_t[1,])
    C$dC<-(C$C-C$C_lag + C$C_soil_t)/5e6					# defines sink as stock change
    C$year<-seq(from=2017,to = 2017+5*(dim(f_loss)[2] - 1), by = 5)
    C$ecocd<-eco_cd
    C$f_type<-f_typ
    C$fed_code<-fed_code
    C$origin_code<-origin_code
    C$plabel<-plabel
    C$region<-region
  
    c_den<-as.data.frame(cden)
    if(length(cden)==30){c_den$age<-(seq(from=0, to=145, by=5))      	# assigns ages based on region (east vs west)
    }else{c_den$age<-(seq(from=0, to=210,by=10))}
  
    c_den_out<-merge(c_den_out,c_den,by='age',all.x=TRUE)
  
    origin<-"natural"
    if(origin_code==1){origin<-"planted"}
    colnames(c_den_out)[ncol(c_den_out)]<-c(paste(eco_cd,plabel,f_typ,origin,sep=","))
  
    if(i==1){C_agg<-C} else{ C_agg<-rbind(C_agg,C)}			# accumulate estimates over the loop
  
    # Adf<-as.data.frame(t(A))
    # 
    # if(length(cden)==30){Adf$age<-(seq(from=0, to=145, by=5))     	# assigns ages based on region (east vs west)
    # }else{Adf$age<-(seq(from=0, to=210,by=10))}
    # 
    # Adf<-Adf[,c(1,5,9,12)]
    # colnames(Adf)<-c('2017','2037','2057','age')
    # Adf_m<-melt(Adf,id='age',na.rm=TRUE)
    # Adf_m$eco<-eco_cd
    # Adf_m$origin<-origin_code
    # Adf_m$ftyp<-f_typ
    # 
    # if(i==1){A_agg<-Adf_m} else{ A_agg<-rbind(A_agg,Adf_m)}		# accumulate estimates over the loop
  
    }									#end loop i
  
  #----------------------------------------------------------------
  #  write key outputs:  forest C and dC by ecoregion/ftype/owner/origin
    if (!is.na(scenario_name)) { write.csv(C_agg, file=paste(output_path, scenario_name, "/forest_c_by_region_etc.csv",sep=""))}
  
   return(C_agg)
  
   }   #end function f_carbon


#################################################################
# Combines forest and nonforest projections to define change in carbon sinks by GHGI categories.

land_carb<-function(US_C,US_nfC, scenario_name = NA){
  
  US_C<-as.data.table(US_C)
  US_C$origin_code[is.na(US_C$origin_code)] <- 0
  US_C$Olab<-plyr::mapvalues(US_C$origin_code, from=c(1,0,'NA'), to=c('planted','natural','natural'))
  US_C$Flab<-plyr::mapvalues(US_C$f_type, from=c('softwood','hardwood'), to=c('soft','hard'))
  
  #----------------------------------------------------------------
  # Input nonforest carbon estimates for scenario
  
  US_nfC<-as.data.table(US_nfC)
  US_nfC$year<-US_nfC$t*5+2017
  US_nfC<-US_nfC[!(US_nfC$year==2017),]
	inc<-unique(US_nfC$luc)
	outc<-c('Cropland','Other','Settlements','Cropland','Other','Settlements','Cropland','Other','Settlements')
  									# summarizes land use change carbon by ending land use
  US_nfC$luc<-plyr::mapvalues(US_nfC$luc, from=inc, to=outc)
  US_nfC_s<-US_nfC[,lapply(.SD,sum),by=list(year,luc),.SDcols=c("flux")]
  
  US_t<-US_C[,lapply(.SD,sum),by=list(year),.SDcols=c("C","dC")]
  US_t<-US_t[!(year==2017 ),]
  US_t$dC<-US_t$dC+20							# add Alaska based on Domke et al 2022
  US_t$luc<-'Forest'
  US_t$flux<-US_t$dC*44/12*(-1)						# convert from solid C to CO2 eq (note change in sign)
  
  US_fC<-US_t[,c('year','luc','flux')]
  US_nfC_t<-US_nfC_s[,c('year','luc','flux')]
  
  US_HWPC<-US_fC
  US_HWPC$luc<-'Wood Products'
  US_HWPC$flux<--94							# add HWPC based on four year average EPA GHGI
  
  US_all_flux<-rbind(US_fC,US_nfC_t,US_HWPC)				# combine forest and nonforest fluxes
  US_all_flux<-US_all_flux[!(US_all_flux$year==2022),]
  #US_all_flux$year<-US_all_flux$year-3
  
  #----------------------------------------------------------------
  # Input historical flux values from EPA Greenhouse Gas Inventory
  
  indata<-read.table(paste(input_path,"carbon_models/Table 6-8_sum_figure_v2.csv",sep=""),header=TRUE, sep=",")
  indata$Forest<-indata$Forest_eco+indata$To_forests +indata$To_cropland +indata$To_.Grassland +indata$To_Settlements
  indata$Other<-indata$Wetlands+indata$Grassland				# summarizes GHG inventories to match projection categories
  indata<-indata[1:31,]
  
  # total historical flux values
  rowSums(indata)-indata[,1]-indata[,2]-indata[,14]
  
  x<-melt(indata,measure.var=c('Forest','HWPC','Cropland','Settlements','Other'),id.var=c('year'))
  colnames(x)<-c('year','luc','flux')
  
  levels(x$luc)[levels(x$luc)=='HWPC'] <- 'Wood Products'
  x2<-rbind(x,US_all_flux)
 
  if (!is.na(scenario_name)) { write.csv(x2,file=paste(output_path, scenario_name, "/land_c_flux.csv",sep=""))}
  
  return(x2)
  
  }    #end function land_carb



######################################################################
# Main program

run_carb <- function(inputs, # List of ecoregion-specific land use transition dataframes, by interval
                     scenario_name = NA, # Name of scenario. If NA, output is not written to disk.
                     intensify = FALSE, 
                     net = FALSE,       # switches for special run--ordinarily turned off:
                     no_luc = FALSE
                     ) {
  
  lu <- landuse(inputs = inputs, 
                scenario_name = scenario_name,
                intensify = intensify,
                net = net,
                no_luc = no_luc)
  
  fc <- f_carbon(lu$gain, lu$loss,
                 scenario_name = scenario_name,
                 intensify = intensify,
                 net = net,
                 no_luc = no_luc)
  
  land_c<-land_carb(fc,lu$nf_flux,
                    scenario_name = scenario_name)
  
  return(land_c)
    
}

