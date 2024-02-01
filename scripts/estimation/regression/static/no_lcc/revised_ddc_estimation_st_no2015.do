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

* Drop y from 2012 (to be consistent with the dynamic model)
drop if year == 2012
tab year if y_st != .

/******************************************************************************/
/* RUN REGRESSION						   						 			*/

capture log close
local dir "results\initial_estimation\regs_2022-03\"
capture mkdir `dir'
log using "`dir'regs_2022-03-29_unweighted_st_no2015.log", replace
capture rm "`dir'regs_2022-03-29_unweighted_st_no2015.xls" 
capture rm "`dir'regs_2022-03-29_unweighted_st_no2015.txt"
capture rm "`dir'regs_2022-03-29_unweighted_3digits_st_no2015.xls"
capture rm "`dir'regs_2022-03-29_unweighted_3digits_st_no2015.txt"

*keep if inlist(stateabbrev,"AR","AL","FL","GA","KY","LA") | ///
*			inlist(stateabbrev,"MO","MS","NC","SC","TN")

/* This run uses updated CCPs in which we code y as missing if there is no land in initial
use in year t-5, it adjusts 2015 CCPs from 3-year to 5-year probabilities, and it replaces zero CCPs with minimum CCPs. */

* drop observations that do not convert to other land uses
keep if initial_use != final_use

* this version does not include year fixed effects, weights or interaction terms
ds, has(varl Conversion*)
rename y_st y // to uniformize all the regression tables 
reg y c.CRP_nr#c.dCRP c.crop_nr#c.dCrop c.forest_nr#c.dForest c.other_nr#c.dOther c.urban_nr#c.dUrban `r(varlist)', robust cluster(fips) noconstant
outreg2 using "`dir'regs_2022-03-29_unweighted_st_no2015.xls", e(r2_a) stats(coef se) bdec(10) ///
addtext(weights, NO) append
outreg2 using "`dir'regs_2022-03-29_unweighted_3digits_st_no2015.xls", e(r2_a) stats(coef se) bdec(3) ///
addtext(weights, NO) append

log close
