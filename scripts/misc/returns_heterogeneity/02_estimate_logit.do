cd "L:/Project-Land_Use/"

import delimited "raw_data/net_returns/forest/forest_plot_rents/plot_rents_by_county_South.csv", clear
tostring county, replace 
rename c_rent R_f


global target_rate = 0.05
global C_cf = 1000
global r = 0.10
global R_c = 75
set seed 97401

*Assume that plots are equal in area

*Generate errors
gen e_f = log(-log(runiform()))
gen e_c = log(-log(runiform()))
gen e_diff = e_c - e_f

local diff = 1e6
local b_guess = 1
local change = 0.1
local iter = 1

while `diff' > 1e-4 {

	preserve
	
	gen convert = (e_diff < `b_guess'*R_f - $R_c - $r*$C_cf)
	summ convert
	local diff = $target_rate - `r(mean)'
	di `diff'
	
	if `iter' == 1 { local increment = 0.1*sign(`diff')*(-1) }
	else {
		local slope = -(`diff' - `diff_prev')/`increment'
		local increment <- `diff'/`slope'		
		}
		
	local diff_prev = `diff'
	local b_guess = `b_guess' + `increment'
	local iter = `iter' + 1

	restore
	
}

di "`b_guess'"
