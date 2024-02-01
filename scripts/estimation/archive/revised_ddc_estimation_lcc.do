cd L:/Project-Land_Use/

use "processing\combined\ddc_data", clear


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


/******************************************************************************/
/* RUN REGRESSION						   						 			*/

capture log close
local dir "results\initial_estimation\regs_2021-12\"
capture mkdir `dir'
log using "`dir'regs_2021-12-01_unweighted_lcc.log", replace

*keep if inlist(stateabbrev,"AR","AL","FL","GA","KY","LA") | ///
*			inlist(stateabbrev,"MO","MS","NC","SC","TN")

/* This run uses updated CCPs in which we code y as missing if there is no land in initial use in year t-5, it adjusts 2015 CCPs from 3-year to 5-year probabilities, and it replaces zero CCPs with minimum CCPs. */

* drop observations that do not convert to other land uses
keep if initial_use != final_use

* this version does not include year fixed effects or weights, but it includes interaction terms with LCCs

ds, has(varl Conversion*)

reg y c.CRP_nr#c.dCRP c.crop_nr#c.dCrop c.forest_nr#c.dForest c.urban_nr#c.dUrban c.other_nr#c.dOther ///
`r(varlist)' ///
c.lcc2#c.CRPtoCrop c.lcc3#c.CRPtoCrop c.lcc4#c.CRPtoCrop ///
c.lcc2#c.CRPtoForest c.lcc3#c.CRPtoForest c.lcc4#c.CRPtoForest ///
c.lcc2#c.CRPtoOther c.lcc3#c.CRPtoOther c.lcc4#c.CRPtoOther ///
c.lcc2#c.CRPtoUrban c.lcc3#c.CRPtoUrban c.lcc4#c.CRPtoUrban ///
c.lcc2#c.CroptoCRP c.lcc3#c.CroptoCRP c.lcc4#c.CroptoCRP ///
c.lcc2#c.CroptoForest c.lcc3#c.CroptoForest c.lcc4#c.CroptoForest ///
c.lcc2#c.CroptoOther c.lcc3#c.CroptoOther c.lcc4#c.CroptoOther ///
c.lcc2#c.CroptoUrban c.lcc3#c.CroptoUrban c.lcc4#c.CroptoUrban ///
c.lcc2#c.ForesttoCRP c.lcc3#c.ForesttoCRP c.lcc4#c.ForesttoCRP ///
c.lcc2#c.ForesttoCrop c.lcc3#c.ForesttoCrop c.lcc4#c.ForesttoCrop ///
c.lcc2#c.ForesttoOther c.lcc3#c.ForesttoOther c.lcc4#c.ForesttoOther ///
c.lcc2#c.ForesttoUrban c.lcc3#c.ForesttoUrban c.lcc4#c.ForesttoUrban ///
c.lcc2#c.OthertoCRP c.lcc3#c.OthertoCRP c.lcc4#c.OthertoCRP ///
c.lcc2#c.OthertoCrop c.lcc3#c.OthertoCrop c.lcc4#c.OthertoCrop ///
c.lcc2#c.OthertoForest c.lcc3#c.OthertoForest c.lcc4#c.OthertoForest ///
c.lcc2#c.OthertoUrban c.lcc3#c.OthertoUrban c.lcc4#c.OthertoUrban ///
c.lcc2#c.UrbantoCRP c.lcc3#c.UrbantoCRP c.lcc4#c.UrbantoCRP c.lcc0#c.UrbantoCRP ///
c.lcc2#c.UrbantoCrop c.lcc3#c.UrbantoCrop c.lcc4#c.UrbantoCrop c.lcc0#c.UrbantoCrop ///
c.lcc2#c.UrbantoForest c.lcc3#c.UrbantoForest c.lcc4#c.UrbantoForest c.lcc0#c.UrbantoForest ///
c.lcc2#c.UrbantoOther c.lcc3#c.UrbantoOther c.lcc4#c.UrbantoOther c.lcc0#c.UrbantoOther ///
c.CRPXlcc2#c.dCRP c.CRPXlcc3#c.dCRP c.CRPXlcc4#c.dCRP c.CRPXlcc0#c.dCRP ///
c.cropXlcc2#c.dCrop c.cropXlcc3#c.dCrop c.cropXlcc4#c.dCrop c.cropXlcc0#c.dCrop ///
c.forestXlcc2#c.dForest c.forestXlcc3#c.dForest c.forestXlcc4#c.dForest c.forestXlcc0#c.dForest ///
c.urbanXlcc2#c.dUrban c.urbanXlcc3#c.dUrban c.urbanXlcc4#c.dUrban c.urbanXlcc0#c.dUrban ///
c.otherXlcc2#c.dOther c.otherXlcc3#c.dOther c.otherXlcc4#c.dOther c.otherXlcc0#c.dOther, robust cluster(fips)
outreg2 using "`dir'regs_2021-12-01_unweighted_lcc.xls", e(r2_a) stats(coef se) bdec(8) ///
addtext(weights, NO) append


log close
