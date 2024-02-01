cd L:/Project-Land_Use/

use "processing\combined\ddc_data_cropcrp", clear

/******************************************************************************/
/* RUN REGRESSION						   						 			*/

local dir "results\initial_estimation\regs_2021-10\"
capture mkdir `dir'
log using "`dir'regs_2021-10-19_cropcrp.log", replace

*keep if inlist(stateabbrev,"AR","AL","FL","GA","KY","LA") | ///
*			inlist(stateabbrev,"MO","MS","NC","SC","TN")

/* This run uses updated CCPs in which we code y as missing if there is no land in initial
use in year t-5, it adjusts 2015 CCPs from 3-year to 5-year probabilities, and it replaces zero CCPs with minimum CCPs. */

foreach i_use in crop CRP {
	foreach f_use in crop CRP {
	
		if "`i_use'" != "`f_use'" {

			local sentiUse = upper(substr("`i_use'",1,1)) + substr("`i_use'",2,.) 
			local sentfUse = upper(substr("`f_use'",1,1)) + substr("`f_use'",2,.) 
				
			di , _n
			di "Transitions `sentiUse' to `sentfUse'...", _n		
					
			reg y `f_use'_nr `i_use'_nr i.year if initial_use == "`sentiUse'" & final_use == "`sentfUse'" [aweight = initial_acres], robust cluster(fips)
			}
	}
}


foreach i_use in crop CRP {
	foreach f_use in crop CRP {
	
		if "`i_use'" != "`f_use'" {

			local sentiUse = upper(substr("`i_use'",1,1)) + substr("`i_use'",2,.) 
			local sentfUse = upper(substr("`f_use'",1,1)) + substr("`f_use'",2,.) 
				
			di , _n
			di "Transitions `sentiUse' to `sentfUse'...", _n		
					
			reg y lcc2 lcc3 lcc4 lcc0 ///
				`f_use'_nr `f_use'Xlcc2 `f_use'Xlcc3 `f_use'Xlcc4 `f_use'Xlcc0 ///
				`i_use'_nr `i_use'Xlcc2 `i_use'Xlcc3 `i_use'Xlcc4 `i_use'Xlcc0 ///
				if initial_use == "`sentiUse'" & final_use == "`sentfUse'" [aweight = initial_acres], robust cluster(fips)
			}
	}
}

log close
