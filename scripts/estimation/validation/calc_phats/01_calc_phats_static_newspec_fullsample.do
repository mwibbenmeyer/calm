cd "L:/Project-Land_Use/"

run "scripts/estimation/regression/program_make_conv_dummies.do"

use "processing\combined\ddc_data_urbancal_eco", clear

/******************************************************************************/
/* ADD DUMMY VARIABLES 					   						 			*/

make_conv_dummies
drop UrbantoCRP UrbantoCrop UrbantoForest UrbantoOther // no variation

/******************************************************************************/
/* CALC PREDICTED VALUES				   						 			*/

/* Specification 1 */
local dir "results\initial_estimation\regs_2022-05\"
est use "`dir'regs_2022-05-04_unweighted_lcc_st1_fullsample.est"
est replay

gen V = .

* Assign the conversion cost to V first
foreach iuse in "CRP" "Crop" "Forest" "Other" "Urban" {
	foreach fuse in "CRP" "Crop" "Forest" "Other" "Urban" {
		forvalues lccval = 1/4 {
			
			di "c.lcc`lccval'#c.`iuse'to`fuse'"
			
			local names : colfullnames e(b)
			
			if "`iuse'" == "`fuse'" {
				/* For obs staying in same use, conversion term = 0*/
				replace V = 0 if initial_use == "`iuse'" & final_use == "`fuse'" & lcc`lccval' == 1
			}
			
			else if strpos("`names'", "c.lcc`lccval'#c.`iuse'to`fuse'") > 0 {		
				/* Replace V with conversion constant term */
				replace V = _b[c.lcc`lccval'#c.`iuse'to`fuse'] if `iuse'to`fuse' == 1 & lcc`lccval' == 1
			}
			
// 			else {
// 				/* For obs representing conversions we don't observe, replace V = . */
// 				replace V = . if final_use == "`fuse'" & lcc`lccval' == 1
// 			}
		}
	}
}

* Then add the net returns term
foreach fuse in "CRP" "Crop" "Forest" "Other" "Urban" {
	forvalues lccval = 1/4 {

	if "`fuse'" == "CRP" local fuselower = "CRP" 
	else local fuselower = lower("`fuse'")
	
	/* Add final use net returns to V*/
	replace V = V + _b[c.`fuselower'Xlcc`lccval'#c.d`fuse']*`fuselower'_nr if final_use == "`fuse'" & lcc`lccval' == 1			
	}
}

bysort fips year lcc initial_use: egen denom = sum(exp(V))
gen phat = exp(V)/denom

* Save results
preserve
local dir "processing\simulation\predicted_probs\"
keep fips year initial_use final_use lcc initial_acres final_acres  *_nr weighted_ccp phat
export delimited "`dir'preds_2022-05-04_lcc_st_newspec1_fullsample.csv", replace
restore

/* Specification 3 */
local dir "results\initial_estimation\regs_2022-05\"
est use "`dir'regs_2022-05-04_unweighted_lcc_st3_fullsample.est"
est replay

replace V = .
levelsof P, local(Ps)

local names : colfullnames e(b)
local names = subinstr("`names'", "b.P", ".P", .) //recode the base group labels
di "`names'"

* Assign the conversion cost to V first
foreach iuse in "CRP" "Crop" "Forest" "Other" "Urban" {
	foreach fuse in "CRP" "Crop" "Forest" "Other" "Urban" {
		forvalues lccval = 1/4 {
			foreach Pval in `Ps' {
			
				di "`Pval'.P#c.lcc`lccval'#c.`iuse'to`fuse'"

				if "`iuse'" == "`fuse'" {
					/* For obs staying in same use, conversion term = 0*/
					replace V = 0 if initial_use == "`iuse'" & final_use == "`fuse'" & lcc`lccval' == 1 & P == `Pval'
				}
				
				else if strpos("`names'", "`Pval'.P#c.lcc`lccval'#c.`iuse'to`fuse'") > 0 {		
					/* Replace V with conversion constant term */
					replace V = _b[`Pval'.P#c.lcc`lccval'#c.`iuse'to`fuse'] if `iuse'to`fuse' == 1 & lcc`lccval' == 1 & P == `Pval'
				}
				
// 				else {
// 					/* For obs representing conversions we don't observe, replace V = . */
// 					replace V = . if final_use == "`fuse'" & lcc`lccval' == 1 & P == `Pval'
// 				}
			}
		}
	}
}

* Then add the net returns term
foreach fuse in "CRP" "Crop" "Forest" "Other" "Urban" {
	forvalues lccval = 1/4 {

	if "`fuse'" == "CRP" local fuselower = "CRP" 
	else local fuselower = lower("`fuse'")
	
	/* Add final use net returns to V*/
	replace V = V + _b[c.`fuselower'Xlcc`lccval'#c.d`fuse']*`fuselower'_nr if final_use == "`fuse'" & lcc`lccval' == 1			
	}
}

capture drop denom phat
bysort fips year lcc initial_use: egen denom = sum(exp(V))
gen phat = exp(V)/denom

* Save results
preserve
local dir "processing\simulation\predicted_probs\"
keep fips year initial_use final_use lcc initial_acres final_acres  *_nr weighted_ccp phat
export delimited "`dir'preds_2022-05-04_lcc_st_newspec3_fullsample.csv", replace
restore

/* Specification 5 */
local dir "results\initial_estimation\regs_2022-05\"
est use "`dir'regs_2022-05-04_unweighted_lcc_st5_fullsample.est"
est replay

replace V = .
levelsof P, local(Ps)

local names : colfullnames e(b)
local names = subinstr("`names'", "b.P", ".P", .) //recode the base group labels
di "`names'"

* Assign the conversion cost to V first
foreach iuse in "CRP" "Crop" "Forest" "Other" "Urban" {
	foreach fuse in "CRP" "Crop" "Forest" "Other" "Urban" {
		forvalues lccval = 1/4 {
			foreach Pval in `Ps' {
			
				di "`Pval'.P#c.lcc`lccval'#c.`iuse'to`fuse'"

				if "`iuse'" == "`fuse'" {
					/* For obs staying in same use, conversion term = 0*/
					replace V = 0 if initial_use == "`iuse'" & final_use == "`fuse'" & lcc`lccval' == 1 & P == `Pval'
				}
				
				else if strpos("`names'", "`Pval'.P#c.lcc`lccval'#c.`iuse'to`fuse'") > 0 {		
					/* Replace V with conversion constant term */
					replace V = _b[`Pval'.P#c.lcc`lccval'#c.`iuse'to`fuse'] if `iuse'to`fuse' == 1 & lcc`lccval' == 1 & P == `Pval'
				}
				
// 				else {
// 					/* For obs representing conversions we don't observe, replace V = . */
// 					replace V = . if final_use == "`fuse'" & lcc`lccval' == 1 & P == `Pval'
// 				}
			}
		}
	}
}

* Then add the net returns term
foreach fuse in "CRP" "Crop" "Forest" "Other" "Urban" {
	forvalues lccval = 1/4 {
		foreach Pval in `Ps' {

			if "`fuse'" == "CRP" local fuselower = "CRP" 
			else local fuselower = lower("`fuse'")
			
			if strpos("`names'", "`Pval'.P#c.`fuselower'Xlcc`lccval'#c.d`fuse'") > 0 {
			/* Add final use net returns to V*/
			replace V = V + _b[`Pval'.P#c.`fuselower'Xlcc`lccval'#c.d`fuse']*`fuselower'_nr if final_use == "`fuse'" & lcc`lccval' == 1 & P == `Pval'			
			}
// 			else {
// 			/* For obs representing conversions we don't observe, replace V = . */
// 			replace V = . if final_use == "`fuse'" & lcc`lccval' == 1 & P == `Pval'
// 			}
		}
	}
}

capture drop denom phat
bysort fips year lcc initial_use: egen denom = sum(exp(V))
gen phat = exp(V)/denom

* Save results
preserve
local dir "processing\simulation\predicted_probs\"
keep fips year initial_use final_use lcc initial_acres final_acres  *_nr weighted_ccp phat
export delimited "`dir'preds_2022-05-04_lcc_st_newspec5_fullsample.csv", replace
restore
