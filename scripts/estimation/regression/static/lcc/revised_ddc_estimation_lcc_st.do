cd L:/Project-Land_Use/

run "scripts/estimation/program_make_conv_dummies.do"

use "processing\combined\ddc_data_urbancal", clear

/******************************************************************************/
/* ADD DUMMY VARIABLES 					   						 			*/

make_conv_dummies
drop CRPtoUrban ForesttoCRP UrbantoCRP UrbantoCrop UrbantoForest UrbantoOther // no variation

/******************************************************************************/
/* RUN REGRESSION						   						 			*/

capture log close
local dir "results\initial_estimation\regs_2022-02\"
capture mkdir `dir'
log using "`dir'regs_2022-02-04_unweighted_lcc_st.log", replace
capture rm "`dir'regs_2022-02-04_unweighted_lcc_st.xls" 
capture rm "`dir'regs_2022-02-04_unweighted_lcc_st.txt"
capture rm "`dir'regs_2022-02-04_unweighted_lcc_3digits_st.xls"
capture rm "`dir'regs_2022-02-04_unweighted_lcc_3digits_st.txt"

*keep if inlist(stateabbrev,"AR","AL","FL","GA","KY","LA") | ///
*			inlist(stateabbrev,"MO","MS","NC","SC","TN")

/* This run uses updated CCPs in which we code y as missing if there is no land in initial use in year t-5, it adjusts 2015 CCPs from 3-year to 5-year probabilities, and it replaces zero CCPs with minimum CCPs. */

* drop observations that do not convert to other land uses
keep if initial_use != final_use

* this version does not include year fixed effects or weights, but it includes interaction terms with LCCs (note that LCC=0 covers only urban->urban so is not included in the interactions)

ds, has(varl Conversion*)
rename y_st y // to uniformize all the regression tables 

reg y ///
	c.CRPXlcc1#c.dCRP c.CRPXlcc2#c.dCRP c.CRPXlcc3#c.dCRP c.CRPXlcc4#c.dCRP ///
	c.cropXlcc1#c.dCrop c.cropXlcc2#c.dCrop c.cropXlcc3#c.dCrop c.cropXlcc4#c.dCrop ///
	c.forestXlcc1#c.dForest c.forestXlcc2#c.dForest c.forestXlcc3#c.dForest c.forestXlcc4#c.dForest ///
	c.urbanXlcc1#c.dUrban c.urbanXlcc2#c.dUrban c.urbanXlcc3#c.dUrban c.urbanXlcc4#c.dUrban ///
	c.otherXlcc1#c.dOther c.otherXlcc2#c.dOther c.otherXlcc3#c.dOther c.otherXlcc4#c.dOther ///
	c.lcc1#c.CRPtoCrop c.lcc2#c.CRPtoCrop c.lcc3#c.CRPtoCrop c.lcc4#c.CRPtoCrop ///
	c.lcc1#c.CRPtoForest c.lcc2#c.CRPtoForest c.lcc3#c.CRPtoForest c.lcc4#c.CRPtoForest ///
	c.lcc1#c.CRPtoOther c.lcc2#c.CRPtoOther c.lcc3#c.CRPtoOther c.lcc4#c.CRPtoOther ///
	c.lcc1#c.CroptoCRP c.lcc2#c.CroptoCRP c.lcc3#c.CroptoCRP c.lcc4#c.CroptoCRP ///
	c.lcc1#c.CroptoForest c.lcc2#c.CroptoForest c.lcc3#c.CroptoForest c.lcc4#c.CroptoForest ///
	c.lcc1#c.CroptoOther c.lcc2#c.CroptoOther c.lcc3#c.CroptoOther c.lcc4#c.CroptoOther ///
	c.lcc1#c.CroptoUrban c.lcc2#c.CroptoUrban c.lcc3#c.CroptoUrban c.lcc4#c.CroptoUrban ///
	c.lcc1#c.ForesttoCrop c.lcc2#c.ForesttoCrop c.lcc3#c.ForesttoCrop c.lcc4#c.ForesttoCrop ///
	c.lcc1#c.ForesttoOther c.lcc2#c.ForesttoOther c.lcc3#c.ForesttoOther c.lcc4#c.ForesttoOther ///
	c.lcc1#c.ForesttoUrban c.lcc2#c.ForesttoUrban c.lcc3#c.ForesttoUrban c.lcc4#c.ForesttoUrban ///
	c.lcc1#c.OthertoCRP c.lcc2#c.OthertoCRP c.lcc3#c.OthertoCRP c.lcc4#c.OthertoCRP ///
	c.lcc1#c.OthertoCrop c.lcc2#c.OthertoCrop c.lcc3#c.OthertoCrop c.lcc4#c.OthertoCrop ///
	c.lcc1#c.OthertoForest c.lcc2#c.OthertoForest c.lcc3#c.OthertoForest c.lcc4#c.OthertoForest ///
	c.lcc1#c.OthertoUrban c.lcc2#c.OthertoUrban c.lcc3#c.OthertoUrban c.lcc4#c.OthertoUrban, robust cluster(fips) noconstant
est save "`dir'regs_2022-02-04_unweighted_lcc_st.est", replace
outreg2 using "`dir'regs_2022-02-04_unweighted_lcc_st.xls", e(r2_a) stats(coef se) bdec(10) ///
addtext(weights, NO) append
outreg2 using "`dir'regs_2022-02-04_unweighted_lcc_3digits_st.xls", e(r2_a) stats(coef se) bdec(3) ///
addtext(weights, NO) append

log close
