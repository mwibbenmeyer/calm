cd "L:/Project-Land_Use/"
**cd "F:/Projects/land-use/"

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

est use `"`using'"'


/******************************************************************************/
/* CALC PREDICTED VALUES				   						 			*/

gen V = 0

/* Intercepts/Conversion costs */

foreach iuse in "CRP" "Crop" "Forest" "Other" "Urban" {
	foreach fuse in "CRP" "Crop" "Forest" "Other" "Urban" {
		forvalues lccval = 1/4 {
						
			local names : colfullnames e(b)
			
			/* For obs staying in same use, conversion term = 0*/
			if "`iuse'" == "`fuse'" replace V = 0 if initial_use == "`iuse'" & final_use == "`fuse'" & lcc == `lccval'
			/* Replace V with conversion constant term */
			else if strpos("`names'","`iuse'to`fuse'") > 0 {
				replace V = _b[c.lcc`lccval'#c.`iuse'to`fuse'] if initial_use == "`iuse'" & final_use == "`fuse'" & lcc == `lccval'
			}
			/* For obs representing conversions we don't observe, replace V = . */
			else replace V = . if initial_use == "`iuse'" & final_use == "`fuse'" & lcc == `lccval'
		}
	}
}

/* Returns */

foreach fuse in "CRP" "Crop" "Forest" "Other" "Urban" {
	forvalues lccval = 1/4 {

	if "`fuse'" == "CRP" local fuselower = "CRP" 
	else local fuselower = lower("`fuse'")
			
	/* Add final use net returns to V*/
	replace V = V + _b[c.`fuselower'Xlcc`lccval'#c.d`fuse']*`fuselower'_nr if final_use == "`fuse'" & lcc == `lccval'			
	}
}

/* Calculate logit probabilities */

bysort fips year lcc initial_use: egen denom = sum(exp(V))
gen phat = exp(V)/denom

}

/******************************************************************************/
/* SAVE RESULTS							   						 			*/

*keep fips year initial_use final_use lcc initial_acres final_acres *_nr phat

end
