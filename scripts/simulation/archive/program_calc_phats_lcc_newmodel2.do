cd "L:/Project-Land_Use/"
**cd "F:/Projects/land-use/"

/******************************************************************************/
/* PROGRAM TO CALCULATE PREDICTED PROBABILITIES		    		 			*/
run "scripts/estimation/regression/program_make_conv_dummies_newmodel2.do"

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
drop UrbantoCrop UrbantoForest UrbantoOther // no variation

est use `"`using'"'
est replay


/******************************************************************************/
/* CALC PREDICTED VALUES				   						 			*/

gen V = 0
drop if lcc == 0

gen lcc1 = (lcc == 1)
gen lcc2 = (lcc == 2)
gen lcc3 = (lcc == 3)
gen lcc4 = (lcc == 4)

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

}

/******************************************************************************/
/* SAVE RESULTS							   						 			*/

*keep fips year initial_use final_use lcc initial_acres final_acres *_nr phat

end
