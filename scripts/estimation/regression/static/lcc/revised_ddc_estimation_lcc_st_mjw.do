*cd L:/Project-Land_Use/
cd F:/Projects/land-use/

run "scripts/estimation/program_make_conv_dummies.do"

use "processing\combined\ddc_data_urbancal", clear

/******************************************************************************/
/* ADD DUMMY VARIABLES 					   						 			*/

make_conv_dummies
drop CRPtoUrban ForesttoCRP UrbantoCRP UrbantoCrop UrbantoForest UrbantoOther // no variation

/******************************************************************************/
/* RUN REGRESSION						   						 			*/

capture log close
local dir "results\initial_estimation\regs_2022-04\"
capture mkdir `dir'
*log using "`dir'regs_2022-02-04_unweighted_lcc_st.log", replace
capture rm "`dir'regs_2022-02-04_unweighted_lcc_st.xls" 
capture rm "`dir'regs_2022-02-04_unweighted_lcc_st.txt"
capture rm "`dir'regs_2022-02-04_unweighted_lcc_3digits_st.xls"
capture rm "`dir'regs_2022-02-04_unweighted_lcc_3digits_st.txt"

*keep if inlist(stateabbrev,"AR","AL","FL","GA","KY","LA") | ///
*			inlist(stateabbrev,"MO","MS","NC","SC","TN")

/* This run uses updated CCPs in which we code y as missing if there is no land in initial use in year t-5, it adjusts 2015 CCPs from 3-year to 5-year probabilities, and it replaces zero CCPs with minimum CCPs. */

* drop observations that do not convert to other land uses
keep if initial_use != final_use

* rename lcc variable so it doesn't conflict with lists of lcc indicators. Drop lcc = 0 indicators since lcc == 0 not present
rename lcc LCC
drop *Xlcc0

* this version does not include year fixed effects or weights, but it includes interaction terms with LCCs (note that LCC=0 covers only urban->urban so is not included in the interactions)

ds, has(varl Conversion*)

local thetavars
foreach use in CRP Crop Forest Urban Other {
  	if "`use'" != "CRP" local loweruse = lower("`use'")
	else local loweruse = "`use'"
	local thetavars "`thetavars' c.`loweruse'Xlcc*#c.d`use'"
	}
di "`thetavars'"

local etavars 
foreach iuse in CRP Crop Forest Urban Other {
    foreach fuse in CRP Crop Forest Urban Other {
	    capture confirm var `iuse'to`fuse'
	    /* Example line using year and state specific intercepts */
		*if !_rc != 0 local etavars "`etavars' c.lcc*#c.`iuse'to`fuse'#i.year#i.statefips"
		if !_rc != 0 local etavars "`etavars' c.lcc*#c.`iuse'to`fuse'"
		}
}
di "`etavars'"

reg y_st `thetavars' `etavars', noconstant robust cluster(fips)

est save "`dir'regs_2022-02-04_unweighted_lcc_st.est", replace
outreg2 using "`dir'regs_2022-02-04_unweighted_lcc_st.xls", e(r2_a) stats(coef se) bdec(10) ///
addtext(weights, NO) append
outreg2 using "`dir'regs_2022-02-04_unweighted_lcc_3digits_st.xls", e(r2_a) stats(coef se) bdec(3) ///
addtext(weights, NO) append

log close
