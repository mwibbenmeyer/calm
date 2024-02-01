cd L:/Project-Land_Use/
set matsize 10000

run "scripts/estimation/regression/program_make_conv_dummies.do"

use "processing\combined\ddc_data_dave", clear

/******************************************************************************/
/* ADD DUMMY VARIABLES 					   						 			*/

make_conv_dummies

/******************************************************************************/
/* RUN REGRESSION						   						 			*/

capture log close
local dir "results\initial_estimation\regs_2022-05\"
capture mkdir `dir'
// log using "`dir'regs_2022-05-19_unweighted_nolcc_st.log", replace
capture rm "`dir'regs_2022-05-19_nolcc_3digits_st_southeast_olddata.xls"
capture rm "`dir'regs_2022-05-19_nolcc_3digits_st_southeast_olddata.txt"
capture rm "`dir'regs_2022-05-19_nolcc_3digits_st_southeast_newdata.xls"
capture rm "`dir'regs_2022-05-19_nolcc_3digits_st_southeast_newdata.txt"

// keep if inlist(stateabbrev,"AR","AL","FL","GA","KY","LA") | ///
// 			inlist(stateabbrev,"MO","MS","NC","SC","TN")

/* This run uses updated CCPs in which we code y as missing if there is no land in initial use in year t-5, it adjusts 2015 CCPs from 3-year to 5-year probabilities, and it replaces zero CCPs with minimum CCPs. */

* drop observations that do not convert to other land uses
keep if initial_use != final_use

* rename lcc variable so it doesn't conflict with lists of lcc indicators. Drop lcc = 0 indicators since lcc == 0 not present
rename lcc LCC
drop *Xlcc0

* this version does not include year fixed effects or weights, but it includes interaction terms with LCCs (note that LCC=0 covers only urban->urban so is not included in the interactions)

ds, has(varl Conversion*)


/******************************************************************************/
* Our data
/******************************************************************************/

* define the variables
local thetavars_base
foreach use in Crop Forest Urban {
  	if "`use'" != "CRP" local loweruse = lower("`use'")
	else local loweruse = "`use'"
	local thetavars_base "`thetavars_base' c.`loweruse'_nr*#c.d`use'"
	}
di "`thetavars_base'"

// local thetavars_ecoP
// foreach use in Crop Forest Urban {
//   	if "`use'" != "CRP" local loweruse = lower("`use'")
// 	else local loweruse = "`use'"
// 	local thetavars_ecoP "`thetavars_ecoP' c.`loweruse'_nr*#c.d`use'#i.P"
// 	}
// di "`thetavars_ecoP'"

local etavars_base
foreach iuse in Crop Forest {
    foreach fuse in Crop Forest Urban {
	    capture confirm var `iuse'to`fuse'
		if !_rc != 0 local etavars_base "`etavars_base' `iuse'to`fuse'"
		}
}
di "`etavars_base'"

// local etavars_ecoP
// foreach iuse in Crop Forest {
//     foreach fuse in Crop Forest Urban {
// 	    capture confirm var `iuse'to`fuse'
// 		if !_rc != 0 local etavars_ecoP "`etavars_ecoP' c.`iuse'to`fuse'#i.P"
// 		}
// }
// di "`etavars_ecoP'"

* specification 1: transition-LCC specific intercepts
// reghdfe y_st `thetavars_base' `etavars_base', a(P year) vce(robust) noconstant
reg y_st `thetavars_base' `etavars_base' i.P i.year, vce(robust) noconstant

est save "`dir'regs_2022-05-19_unweighted_nolcc_st_southeast_olddata.est", replace
outreg2 using "`dir'regs_2022-05-19_nolcc_3digits_st_southeast_olddata.xls", e(r2_a) stats(coef se) bdec(3) ///
addtext(weights, NO, eta, transition, theta, landuse) append

// * specification 3: transition-LCC-ecoregionP specific intercepts
// reghdfe y_st `thetavars_base' `etavars_ecoP', a(year) vce(robust) noconstant
//
// est save "`dir'regs_2022-05-19_unweighted_nolcc_st3_southeast_olddata.est", replace
// outreg2 using "`dir'regs_2022-05-19_nolcc_3digits_st_southeast_olddata.xls", e(r2_a) stats(coef se) bdec(3) ///
// addtext(weights, NO, eta, transitionxLCCxP, theta, landusexLCC) append
//
// * specification 5: transition-LCC-ecoregionP specific intercepts and coefficients
// reghdfe y_st `thetavars_ecoP' `etavars_ecoP', a(year) vce(robust) noconstant
//
// est save "`dir'regs_2022-05-19_unweighted_nolcc_st5_southeast_olddata.est", replace
// outreg2 using "`dir'regs_2022-05-19_nolcc_3digits_st_southeast_olddata.xls", e(r2_a) stats(coef se) bdec(3) ///
// addtext(weights, NO, eta, transitionxLCCxP, theta, landusexLCCxP) append


/******************************************************************************/
* Dave's data
/******************************************************************************/

* define the variables
local thetavars_base_new = "c.ma_nrev_ag#c.dCrop c.mv_rev_ag#c.dCrop c.ma_rent_f#c.dForest c.mv_rent_f#c.dForest c.corr_a_f#c.dForest dwet dplant_cost dpop_den dpipc"

// local thetavars_ecoP_new
// foreach var in `thetavars_base_new' {
// 	local thetavars_ecoP_new "`thetavars_ecoP_new' c.`var'#i.P"
// 	}
// di "`thetavars_ecoP_new'"

local etavars_base_new
foreach iuse in Crop Forest {
    foreach fuse in Crop Forest Urban {
	    capture confirm var `iuse'to`fuse'
		if !_rc != 0 local etavars_base_new "`etavars_base_new' `iuse'to`fuse'"
		}
}
di "`etavars_base_new'"

// local etavars_ecoP_new
// foreach iuse in Crop Forest {
//     foreach fuse in Crop Forest Urban {
// 	    capture confirm var `iuse'to`fuse'
// 		if !_rc != 0 local etavars_ecoP_new "`etavars_ecoP_new' c.`iuse'to`fuse'#i.P"
// 		}
// }
// di "`etavars_ecoP_new'"

* specification 1: transition-LCC specific intercepts
// reghdfe y_st `thetavars_base_new' `etavars_base_new', a(P year) vce(robust) noconstant
reg y_st `thetavars_base_new' `etavars_base_new' i.P i.year,vce(robust) noconstant
est save "`dir'regs_2022-05-19_unweighted_nolcc_st_southeast_newdata.est", replace
outreg2 using "`dir'regs_2022-05-19_nolcc_3digits_st_southeast_newdata.xls", e(r2_a) stats(coef se) bdec(3) ///
addtext(weights, YES, eta, transition, theta, landuse) append

// * specification 3: transition-LCC-ecoregionP specific intercepts
// reghdfe y_st `thetavars_base_new' `etavars_ecoP_new', a(year) vce(robust) noconstant
//
// est save "`dir'regs_2022-05-19_unweighted_nolcc_st3_southeast_olddata.est", replace
// outreg2 using "`dir'regs_2022-05-19_nolcc_3digits_st_southeast_olddata.xls", e(r2_a) stats(coef se) bdec(3) ///
// addtext(weights, NO, eta, transitionxLCCxP, theta, landusexLCC) append
//
// * specification 5: transition-LCC-ecoregionP specific intercepts and coefficients
// reghdfe y_st `thetavars_ecoP_new' `etavars_ecoP_new', a(year) vce(robust) noconstant
//
// est save "`dir'regs_2022-05-19_unweighted_nolcc_st5_southeast_olddata.est", replace
// outreg2 using "`dir'regs_2022-05-19_nolcc_3digits_st_southeast_olddata.xls", e(r2_a) stats(coef se) bdec(3) ///
// addtext(weights, NO, eta, transitionxLCCxP, theta, landusexLCCxP) append
