cd L:/Project-Land_Use/
set matsize 10000

global date "2022-09-18"

run "scripts/estimation/regression/program_make_conv_dummies_newmodel2.do"

use "processing\combined\ddc_data", clear

/******************************************************************************/
/* ADD DUMMY VARIABLES 					   						 			*/

make_conv_dummies
drop UrbantoCrop UrbantoForest UrbantoOther // no variation

//drop if y_st == .

foreach x of varlist other_nr otherXlcc1 otherXlcc2 otherXlcc3 otherXlcc4 {
  replace `x' = 0 if (`x' == .)
}

/******************************************************************************/
/* FILTER THE DATASET   				   						 			*/

// keep if inlist(stateabbrev,"AR","AL","FL","GA","KY","LA") | ///
// 			inlist(stateabbrev,"MO","MS","NC","SC","TN")

* rename lcc variable so it doesn't conflict with lists of lcc indicators. Drop lcc = 0 indicators since lcc == 0 not present
rename lcc LCC
drop *Xlcc0

* drop observations that do not convert to other land uses
keep if initial_use != final_use

/******************************************************************************/
/* RUN REGRESSION WITHOUT LCCS 			   						 			*/

local dir "results\initial_estimation\regs_2023-09\"
capture mkdir `dir'

* this version does not include year fixed effects or interaction terms
ds, has(varl Conversion*)
reg y_st c.crop_nr#c.dCrop c.forest_nr#c.dForest c.other_nr#c.dOther c.urban_nr#c.dUrban `r(varlist)' [aweight = initial_acres], robust cluster(fips) noconstant
est save "`dir'regs_${date}_nolcc.est", replace
outreg2 using "`dir'regs_${date}_nolcc.xls", e(r2_a) stats(coef se) bdec(3) ///
addtext(weights, NO) append

preserve

predict resid, r
keep fips year LCC initial_use final_use resid
export delimited "`dir'resid_${date}_nolcc.csv", replace

restore

/******************************************************************************/
/* RUN REGRESSION WITH LCCS 			   						 			*/

local dir "results\initial_estimation\regs_2023-09\"
capture mkdir `dir'

* this version does not include year fixed effects or weights, but it includes interaction terms with LCCs (note that LCC=0 covers only urban->urban so is not included in the interactions)
ds, has(varl Conversion*)

local thetavars_base
foreach use in Crop Forest Urban Other {
  	if "`use'" != "CRP" local loweruse = lower("`use'")
	else local loweruse = "`use'"
	local thetavars_base "`thetavars_base' c.`loweruse'Xlcc*#c.d`use'"
	}
di "`thetavars_base'"

local etavars_base
foreach iuse in Crop Forest Urban Other {
    foreach fuse in Crop Forest Urban Other {
	    capture confirm var `iuse'to`fuse'
		if !_rc != 0 local etavars_base "`etavars_base' c.lcc*#c.`iuse'to`fuse'"
		}
}
di "`etavars_base'"

* specification: transition-LCC specific intercepts
reg y_st `thetavars_base' `etavars_base' [aweight = initial_acres] , noconstant robust cluster(fips)

est save "`dir'regs_${date}.est" , replace
outreg2 using "`dir'regs_${date}_lcc.xls", e(r2_a) stats(coef se) bdec(3) ///
addtext(weights, NO, eta, transitionxLCC, theta, landusexLCC) append

preserve

predict resid, r
keep fips year LCC initial_use final_use resid
export delimited "`dir'resid_${date}_lcc.csv", replace

restore

// count if missing(other_nr)

keep if e(sample)==1
save "processing\combined\reg_diagnostics", replace



