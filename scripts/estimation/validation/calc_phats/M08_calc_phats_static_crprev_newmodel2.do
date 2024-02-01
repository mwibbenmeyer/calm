cd "L:/Project-Land_Use/"

run "scripts/estimation/regression/program_make_conv_dummies_newmodel2.do"

use "processing\combined\ddc_data_urbancal_crprev", clear

/******************************************************************************/
/* ADD DUMMY VARIABLES 					   						 			*/

make_conv_dummies
drop UrbantoCrop UrbantoForest UrbantoOther // no variation

/******************************************************************************/
/* CALC PREDICTED VALUES FOR MODELS WITHOUT LCCS				 			*/

local dir "results\initial_estimation\regs_2022-07\"
est use "`dir'regs_2022-07-12_nolcc_st_newmodel2.est"
est replay

gen V = 0
drop if lcc == "0"

foreach iuse in "Crop" "Forest" "Other" "Urban" {
	foreach fuse in "Crop" "Forest" "Other" "Urban" {
			
			di "`iuse'to`fuse'"
			
			local names : colfullnames e(b)
			
			/* For obs staying in same use, conversion term = 0*/
			if "`iuse'" == "`fuse'" replace V = 0 if initial_use == "`iuse'" & final_use == "`fuse'" 
			/* Replace V with conversion constant term */
			else if strpos("`names'","`iuse'to`fuse'") > 0 {
				di "has coef"
				replace V = _b[`iuse'to`fuse'] if `iuse'to`fuse' == 1 
			}
			/* For obs representing conversions we don't observe, replace V = . */
			else {
			di "no coef"
			replace V = . if initial_use == "`iuse'" & final_use == "`fuse'" 
			}
		
	}
}

foreach fuse in "Crop" "Forest" "Other" "Urban" {

	if "`fuse'" == "CRP" local fuselower = "CRP" 
	else local fuselower = lower("`fuse'")
	
	/* Add final use net returns to V*/
	replace V = V + _b[c.`fuselower'_nr#c.d`fuse']*`fuselower'_nr if final_use == "`fuse'" & final_use != "Other" 		
	
}

bysort fips year lcc initial_use: egen denom = sum(exp(V))
gen phat = exp(V)/denom

keep fips year initial_use final_use lcc initial_acres final_acres  *_nr weighted_ccp phat
export delimited "`dir'preds_2022-07-12_nolcc_st_newmodel2.csv", replace



/******************************************************************************/
/* CALC PREDICTED VALUES FOR MODELS WITH LCCS				 			*/

use "processing\combined\ddc_data_urbancal_crprev", clear
make_conv_dummies
drop UrbantoCrop UrbantoForest UrbantoOther // no variation

local dir "results\initial_estimation\regs_2022-07\"
est use "`dir'regs_2022-07-12_lcc_st_newmodel2.est"
est replay

gen V = 0
drop if lcc == "0"

foreach iuse in "Crop" "Forest" "Other" "Urban" {
	foreach fuse in "Crop" "Forest" "Other" "Urban" {
		forvalues lccval = 1/4 {
			
			di "c.lcc`lccval'#c.`iuse'to`fuse'"
			
			local names : colfullnames e(b)
			
			/* For obs staying in same use, conversion term = 0*/
			if "`iuse'" == "`fuse'" replace V = 0 if initial_use == "`iuse'" & final_use == "`fuse'" & lcc`lccval' == 1
			/* Replace V with conversion constant term */
			else if strpos("`names'","c.lcc`lccval'#c.`iuse'to`fuse'") > 0 {
				di "has coef"
				replace V = _b[c.lcc`lccval'#c.`iuse'to`fuse'] if `iuse'to`fuse' == 1 & lcc`lccval' == 1
			}
			/* For obs representing conversions we don't observe, replace V = . */
			else {
			di "no coef"
			replace V = . if initial_use == "`iuse'" & final_use == "`fuse'" & lcc`lccval' == 1
			}
		
		}
	}
}

foreach fuse in "Crop" "Forest" "Other" "Urban" {
	forvalues lccval = 1/4 {

	if "`fuse'" == "CRP" local fuselower = "CRP" 
	else local fuselower = lower("`fuse'")
	
	/* Add final use net returns to V*/
	replace V = V + _b[c.`fuselower'Xlcc`lccval'#c.d`fuse']*`fuselower'_nr if final_use == "`fuse'" & lcc`lccval' == 1 & final_use != "Other"			
	}
}

bysort fips year lcc initial_use: egen denom = sum(exp(V))
gen phat = exp(V)/denom		   						 			

keep fips year initial_use final_use lcc initial_acres final_acres  *_nr weighted_ccp phat
tab initial_use final_use, sum(phat)
export delimited "`dir'preds_2022-07-12_lcc_st_newmodel2.csv", replace
