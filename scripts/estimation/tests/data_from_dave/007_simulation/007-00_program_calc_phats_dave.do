cd "L:/Project-Land_Use/"
**cd "F:/Projects/land-use/"
run "scripts/estimation/regression/program_make_conv_dummies.do"

/******************************************************************************/
/* PROGRAM TO CALCULATE PREDICTED PROBABILITIES		    		 			*/

capture program drop calc_phat
program calc_phat

syntax using/, 

qui {

/* Variable definitions 

using - Set using as the .est containing desired estimates


NOTE: Data frame with county x transition x lcc observations and returns should 
be set prior to running script
*/
make_conv_dummies

est use `"`using'"'
est replay


/******************************************************************************/
/* CALC PREDICTED VALUES				   						 			*/

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
replace V = V + _b[c.ma_rent_f#c.dForest]*ma_rent_f +_b[c.mv_rent_f#c.dForest]*mv_rent_f + _b[dplant_cost]*plant_cost + _b[c.corr_a_f#c.dForest]*corr_a_f if final_use == "Forest"
replace V = V + _b[c.ma_nrev_ag#c.dCrop]*ma_nrev_ag + _b[c.mv_rev_ag#c.dCrop]*mv_rev_ag + _b[dwet]*wet if final_use == "Crop"
replace V = V + _b[dpop_den]*pop_den + _b[dpipc]*pipc if final_use == "Urban"

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

/******************************************************************************/
/* SAVE RESULTS							   						 			*/

*keep fips year initial_use final_use lcc initial_acres final_acres *_nr phat
}
end
