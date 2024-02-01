cd L:/Project-Land_Use/

use "processing\combined\ddc_data_urbancal", clear


/******************************************************************************/
/* ADD DUMMY VARIABLES 					   						 			*/

* dummy1: conversion from/to a land use type
levelsof initial_use, local(K)
foreach k in `K' {
	gen initial_`k' = (initial_use=="`k'")
}

levelsof final_use, local(J)
foreach j in `J' {
	gen final_`j' = (final_use=="`j'")
}

foreach i_use in `K' {
	gen d`i_use' = final_`i_use' - initial_`i_use'
}

* dummy2: specific conversions
foreach k in `K' {
	foreach j in `J' {
		if "`k'" != "`j'" {
			gen `k'to`j' = (initial_use=="`k'") & (final_use=="`j'")
			label variable `k'to`j' "Conversion from `k' to `j'"
		}
	}
}

drop CRPtoUrban ForesttoCRP UrbantoCRP UrbantoCrop UrbantoForest UrbantoOther // no variation

/******************************************************************************/
/* RUN REGRESSION						   						 			*/

capture log close
local dir "results\initial_estimation\regs_2022-02\"
capture mkdir `dir'
log using "`dir'regs_2022-02-04_unweighted_lcc_dy.log", replace
capture rm "`dir'regs_2022-02-04_unweighted_lcc_dy.xls" 
capture rm "`dir'regs_2022-02-04_unweighted_lcc_dy.txt"
capture rm "`dir'regs_2022-02-04_unweighted_lcc_3digits_dy.xls"
capture rm "`dir'regs_2022-02-04_unweighted_lcc_3digits_dy.txt"

*keep if inlist(stateabbrev,"AR","AL","FL","GA","KY","LA") | ///
*			inlist(stateabbrev,"MO","MS","NC","SC","TN")

/* This run uses updated CCPs in which we code y as missing if there is no land in initial use in year t-5, it adjusts 2015 CCPs from 3-year to 5-year probabilities, and it replaces zero CCPs with minimum CCPs. */

* drop observations that do not convert to other land uses
keep if initial_use != final_use

* this version does not include year fixed effects or weights, but it includes interaction terms with LCCs (note that LCC=0 covers only urban->urban so is not included in the interactions)

ds, has(varl Conversion*)
rename y_dy y // to uniformize all the regression tables 

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
outreg2 using "`dir'regs_2022-02-04_unweighted_lcc_dy.xls", e(r2_a) stats(coef se) bdec(10) ///
addtext(weights, NO) append
outreg2 using "`dir'regs_2022-02-04_unweighted_lcc_3digits_dy.xls", e(r2_a) stats(coef se) bdec(3) ///
addtext(weights, NO) append

log close

local dir "results\initial_estimation\regs_2022-01\"
log using "`dir'post-regression_summary.log", replace

bysort lcc: tab initial_use final_use if e(sample)==1
bysort lcc: tab initial_use final_use if e(sample)==1, sum(initial_acres)
// bysort lcc: tab initial_use final_use if e(sample)==1, sum(final_acres) // actual converted acres

log close

