cd L:/Project-Land_Use/
set matsize 10000

run "scripts/estimation/regression/program_make_conv_dummies.do"

use "processing\combined\ddc_data_urbancal_eco", clear

/******************************************************************************/
/* ADD DUMMY VARIABLES 					   						 			*/

make_conv_dummies
drop UrbantoCRP UrbantoCrop UrbantoForest UrbantoOther // no variation

/******************************************************************************/
/* RUN REGRESSION						   						 			*/

capture log close
local dir "results\initial_estimation\regs_2022-04\"
capture mkdir `dir'
*log using "`dir'regs_2022-04-27_unweighted_lcc_st.log", replace
capture rm "`dir'regs_2022-04-27_lcc_st_newspec.xls" 
capture rm "`dir'regs_2022-04-27_lcc_st_newspec.txt"
capture rm "`dir'regs_2022-04-27_lcc_3digits_st_newspec.xls"
capture rm "`dir'regs_2022-04-27_lcc_3digits_st_newspec.txt"

keep if inlist(stateabbrev,"AR","AL","FL","GA","KY","LA") | ///
			inlist(stateabbrev,"MO","MS","NC","SC","TN")

/* This run uses updated CCPs in which we code y as missing if there is no land in initial use in year t-5, it adjusts 2015 CCPs from 3-year to 5-year probabilities, and it replaces zero CCPs with minimum CCPs. */

* drop observations that do not convert to other land uses
keep if initial_use != final_use

* rename lcc variable so it doesn't conflict with lists of lcc indicators. Drop lcc = 0 indicators since lcc == 0 not present
rename lcc LCC
drop *Xlcc0

* this version does not include year fixed effects or weights, but it includes interaction terms with LCCs (note that LCC=0 covers only urban->urban so is not included in the interactions)

ds, has(varl Conversion*)

local thetavars_base
foreach use in CRP Crop Forest Urban Other {
  	if "`use'" != "CRP" local loweruse = lower("`use'")
	else local loweruse = "`use'"
	local thetavars_base "`thetavars_base' c.`loweruse'Xlcc*#c.d`use'"
	}
di "`thetavars_base'"

local thetavars_ecoP
foreach use in CRP Crop Forest Urban Other {
  	if "`use'" != "CRP" local loweruse = lower("`use'")
	else local loweruse = "`use'"
	local thetavars_ecoP "`thetavars_ecoP' c.`loweruse'Xlcc*#c.d`use'#i.P"
	}
di "`thetavars_ecoP'"

local etavars_base
foreach iuse in CRP Crop Forest Urban Other {
    foreach fuse in CRP Crop Forest Urban Other {
	    capture confirm var `iuse'to`fuse'
	    /* Example line using year and state specific intercepts */
		*if !_rc != 0 local etavars_base "`etavars_base' c.lcc*#c.`iuse'to`fuse'#i.year#i.statefips"
		if !_rc != 0 local etavars_base "`etavars_base' c.lcc*#c.`iuse'to`fuse'"
		}
}
di "`etavars_base'"

local etavars_ecoP
foreach iuse in CRP Crop Forest Urban Other {
    foreach fuse in CRP Crop Forest Urban Other {
	    capture confirm var `iuse'to`fuse'
	    /* Example line using year and state specific intercepts */
		if !_rc != 0 local etavars_ecoP "`etavars_ecoP' c.lcc*#c.`iuse'to`fuse'#i.P"
		*if !_rc != 0 local etavars_ecoP "`etavars_ecoP' c.lcc*#c.`iuse'to`fuse'"
		}
}
di "`etavars_ecoP'"


* specification 1: transition-LCC specific intercepts
reg y_st `thetavars_base' `etavars_base', noconstant robust cluster(fips)

est save "`dir'regs_2022-04-27_unweighted_lcc_st1.est", replace
outreg2 using "`dir'regs_2022-04-27_lcc_st_newspec.xls", e(r2_a) stats(coef se) bdec(10) ///
addtext(weights, NO, eta, transitionxLCC, theta, landusexLCC) append
outreg2 using "`dir'regs_2022-04-27_lcc_3digits_st_newspec.xls", e(r2_a) stats(coef se) bdec(3) ///
addtext(weights, NO, eta, transitionxLCC, theta, landusexLCC) append

* specification 2: transition-LCC specific intercepts + weights
reg y_st `thetavars_base' `etavars_base' [aweight=initial_acres], noconstant robust cluster(fips)

est save "`dir'regs_2022-04-27_unweighted_lcc_st2.est", replace
outreg2 using "`dir'regs_2022-04-27_lcc_st_newspec.xls", e(r2_a) stats(coef se) bdec(10) ///
addtext(weights, YES, eta, transitionxLCC, theta, landusexLCC) append
outreg2 using "`dir'regs_2022-04-27_lcc_3digits_st_newspec.xls", e(r2_a) stats(coef se) bdec(3) ///
addtext(weights, YES, eta, transitionxLCC, theta, landusexLCC) append

* specification 3: transition-LCC-ecoregionP specific intercepts
reg y_st `thetavars_base' `etavars_ecoP', noconstant robust cluster(fips)

est save "`dir'regs_2022-04-27_unweighted_lcc_st3.est", replace
outreg2 using "`dir'regs_2022-04-27_lcc_st_newspec.xls", e(r2_a) stats(coef se) bdec(10) ///
addtext(weights, NO, eta, transitionxLCCxP, theta, landusexLCC) append
outreg2 using "`dir'regs_2022-04-27_lcc_3digits_st_newspec.xls", e(r2_a) stats(coef se) bdec(3) ///
addtext(weights, NO, eta, transitionxLCCxP, theta, landusexLCC) append

* specification 4: transition-LCC-ecoregionP specific intercepts + weights
reg y_st `thetavars_base' `etavars_ecoP' [aweight=initial_acres], noconstant robust cluster(fips)

est save "`dir'regs_2022-04-27_unweighted_lcc_st4.est", replace
outreg2 using "`dir'regs_2022-04-27_lcc_st_newspec.xls", e(r2_a) stats(coef se) bdec(10) ///
addtext(weights, YES, eta, transitionxLCCxP, theta, landusexLCC) append
outreg2 using "`dir'regs_2022-04-27_lcc_3digits_st_newspec.xls", e(r2_a) stats(coef se) bdec(3) ///
addtext(weights, YES, eta, transitionxLCCxP, theta, landusexLCC) append

* specification 5: transition-LCC-ecoregionP specific intercepts and coefficients
reg y_st `thetavars_ecoP' `etavars_ecoP', noconstant robust cluster(fips)

est save "`dir'regs_2022-04-27_unweighted_lcc_st5.est", replace
outreg2 using "`dir'regs_2022-04-27_lcc_st_newspec.xls", e(r2_a) stats(coef se) bdec(10) ///
addtext(weights, NO, eta, transitionxLCCxP, theta, landusexLCCxP) append
outreg2 using "`dir'regs_2022-04-27_lcc_3digits_st_newspec.xls", e(r2_a) stats(coef se) bdec(3) ///
addtext(weights, NO, eta, transitionxLCCxP, theta, landusexLCCxP) append

* specification 6: transition-LCC-ecoregionP specific intercepts and coefficients + weights
reg y_st `thetavars_ecoP' `etavars_ecoP' [aweight=initial_acres], noconstant robust cluster(fips)

est save "`dir'regs_2022-04-27_unweighted_lcc_st6.est", replace
outreg2 using "`dir'regs_2022-04-27_lcc_st_newspec.xls", e(r2_a) stats(coef se) bdec(10) ///
addtext(weights, YES, eta, transitionxLCCxP, theta, landusexLCCxP) append
outreg2 using "`dir'regs_2022-04-27_lcc_3digits_st_newspec.xls", e(r2_a) stats(coef se) bdec(3) ///
addtext(weights, YES, eta, transitionxLCCxP, theta, landusexLCCxP) append


* log close
