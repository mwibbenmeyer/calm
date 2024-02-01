cd "F:/Projects/land-use/"

use "processing_output\pointpanel_estimation_unb.dta", clear

foreach var in initial_use final_use {
	replace `var' = "Other" if `var' == "CRP"
	replace `var' = "Other" if `var' == "Pasture"
	replace `var' = "Other" if `var' == "Range"
	}

drop if inlist(initial_use,"Federal","Rural","Water")

