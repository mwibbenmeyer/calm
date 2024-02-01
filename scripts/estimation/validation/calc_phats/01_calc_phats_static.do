cd "L:/Project-Land_Use/"

run "scripts/estimation/program_make_conv_dummies.do"

use "processing\combined\ddc_data_urbancal", clear

/******************************************************************************/
/* ADD DUMMY VARIABLES 					   						 			*/

make_conv_dummies
drop CRPtoUrban ForesttoCRP UrbantoCRP UrbantoCrop UrbantoForest UrbantoOther // no variation

/******************************************************************************/
/* CALC PREDICTED VALUES				   						 			*/

local dir "results\initial_estimation\regs_2022-02\"
est use "`dir'regs_2022-02-04_unweighted_lcc_st.est"
est replay

gen V = 0

foreach iuse in "CRP" "Crop" "Forest" "Other" "Urban" {
	foreach fuse in "CRP" "Crop" "Forest" "Other" "Urban" {
		forvalues lccval = 1/4 {
			
			di "lcc`lccval'#c.`iuse'to`fuse'"
			
			local names : colfullnames e(b)
			
			/* For obs staying in same use, conversion term = 0*/
			if "`iuse'" == "`fuse'" replace V = 0 if initial_use == "`iuse'" & final_use == "`fuse'" & lcc`lccval' == 1
			/* Replace V with conversion constant term */
			else if strpos("`names'","`iuse'to`fuse'") > 0 {
				replace V = _b[c.lcc`lccval'#c.`iuse'to`fuse'] if `iuse'to`fuse' == 1 & lcc`lccval' == 1
			}
			/* For obs representing conversions we don't observe, replace V = . */
			else replace V = . if initial_use == "`iuse'" & final_use == "`fuse'" & lcc`lccval' == 1
		
		}
	}
}

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

/******************************************************************************/
/* SAVE RESULTS							   						 			*/

keep fips year initial_use final_use lcc initial_acres final_acres  *_nr weighted_ccp phat
export delimited "`dir'preds_2022-02-04_unweighted_lcc_st.csv", replace