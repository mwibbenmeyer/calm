cd "L:/Project-Land_Use/"

run "scripts/estimation/regression/program_make_conv_dummies.do"

use "processing\combined\ddc_data_dave", clear

/******************************************************************************/
/* ADD DUMMY VARIABLES 					   						 			*/

make_conv_dummies

/******************************************************************************/
/* CALC PREDICTED VALUES				   						 			*/

/******************************************************************************/
* Our data
/******************************************************************************/
local dir "results\initial_estimation\regs_2022-05\"
est use "`dir'regs_2022-05-19_unweighted_nolcc_st_southeast_olddata.est"
est replay

gen V = .

* Assign the conversion cost to V first
foreach iuse in "Crop" "Forest" "Urban" {
	foreach fuse in "Crop" "Forest" "Urban" {
			
			di "`iuse'to`fuse'"
			
			local names : colfullnames e(b)
			
			if "`iuse'" == "`fuse'" {
				/* For obs staying in same use, conversion term = 0*/
				replace V = 0 if initial_use == "`iuse'" & final_use == "`fuse'"
			}
			
			else if strpos("`names'", "`iuse'to`fuse'") > 0 {		
				/* Replace V with conversion constant term */
				replace V = _b[`iuse'to`fuse'] if `iuse'to`fuse' == 1
			}

	}
}

* Then add the net returns term
foreach fuse in "Crop" "Forest" "Urban" {
	if "`fuse'" == "CRP" local fuselower = "CRP" 
	else local fuselower = lower("`fuse'")
	replace V = V + _b[c.`fuselower'_nr#c.d`fuse']*`fuselower'_nr if final_use == "`fuse'"		
}

* Then add the fixed effects
levelsof P, local(Pvalues)
foreach p in `Pvalues' {
	local names : colfullnames e(b)
	if strpos("`names'", "`p'.P") > 0 {
		replace V = V + _b[`p'.P] if P == `p'	
		}
}

levelsof year, local(Years)
foreach y in `Years' {
	local names : colfullnames e(b)
	if strpos("`names'", "`y'.year") > 0 {
		replace V = V + _b[`y'.year] if year == `y'	
		}
}

bysort fips year lcc initial_use: egen denom = sum(exp(V))
gen phat = exp(V)/denom

sum phat, d

* Save results
preserve
local dir "processing\simulation\predicted_probs\"
keep fips year initial_use final_use lcc initial_acres final_acres  *_nr weighted_ccp phat
export delimited "`dir'preds_2022-05-19_nolcc_st_southeast_olddata.csv", replace
restore


/******************************************************************************/
* Dave's data
/******************************************************************************/
local dir "results\initial_estimation\regs_2022-05\"
est use "`dir'regs_2022-05-19_unweighted_nolcc_st_southeast_newdata.est"
est replay

replace V = .

* Assign the conversion cost to V first
foreach iuse in "Crop" "Forest" "Urban" {
	foreach fuse in "Crop" "Forest" "Urban" {
			
			di "`iuse'to`fuse'"
			
			local names : colfullnames e(b)
			
			if "`iuse'" == "`fuse'" {
				/* For obs staying in same use, conversion term = 0*/
				replace V = 0 if initial_use == "`iuse'" & final_use == "`fuse'"
			}
			
			else if strpos("`names'", "`iuse'to`fuse'") > 0 {		
				/* Replace V with conversion constant term */
				replace V = _b[`iuse'to`fuse'] if `iuse'to`fuse' == 1
			}

	}
}

* Then add the net returns term
replace V = V + _b[c.ma_rent_f#c.dForest]*forest_nr +_b[c.mv_rent_f#c.dForest]*mv_rent_f + _b[dplant_cost]*plant_cost + _b[c.corr_a_f#c.dForest]*corr_a_f if final_use == "Forest"
replace V = V + _b[c.ma_nrev_ag#c.dCrop]*crop_nr + _b[c.mv_rev_ag#c.dCrop]*mv_rev_ag + _b[dwet]*wet if final_use == "Crop"
replace V = V + _b[dpop_den]*dpop_den + _b[dpipc]*dpipc if final_use == "Urban"

* Then add the fixed effects
levelsof P, local(Pvalues)
foreach p in `Pvalues' {
	local names : colfullnames e(b)
	if strpos("`names'", "`p'.P") > 0 {
		replace V = V + _b[`p'.P] if P == `p'	
		}
}

levelsof year, local(Years)
foreach y in `Years' {
	local names : colfullnames e(b)
	if strpos("`names'", "`y'.year") > 0 {
		replace V = V + _b[`y'.year] if year == `y'	
		}
}

capture drop denom phat
bysort fips year lcc initial_use: egen denom = sum(exp(V))
gen phat = exp(V)/denom

sum phat, d

* Save results
preserve
local dir "processing\simulation\predicted_probs\"
keep fips year initial_use final_use lcc initial_acres final_acres  *_nr weighted_ccp phat
export delimited "`dir'preds_2022-05-19_nolcc_st_southeast_newdata.csv", replace
restore
