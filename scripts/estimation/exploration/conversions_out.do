cd L:/Project-Land_Use/

local dir "results\initial_exploration\regs_2021-09\"
capture mkdir `dir'
log using "`dir'regs_2021-09-09-conversions-out.log", replace

use "processing\combined\ddc_data", clear

gen outforest = (initial_use == "Forest" & final_use != "Forest")
gen outother = (initial_use == "Other" & final_use != "Other")
gen outcrop = (initial_use == "Crop" & final_use != "Crop")

gen south = 1 if inlist(stateabbrev,"AR","AL","FL","GA","KY","LA") | ///
			inlist(stateabbrev,"MO","MS","NC","SC","TN")
egen st = group(statefips)

levelsof lcc, local(lcc_vals)

foreach use in crop forest other  { 

	di "Conversions out of `use'"

	foreach lcc in `lcc_vals' {

		if "`lcc'" != "0" {
		di "LCC == `lcc'"
			reg weighted_ccp `use'_nr i.year [aweight = initial_acres] if lcc == "`lcc'" & out`use' == 1
			*reg weighted_ccp forest_nr [aweight = initial_acres] if lcc == "`lcc'" & outforest == 1
		
		}
	}
}

log close
