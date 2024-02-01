cd L:/Project-Land_Use/
set matsize 10000

run "scripts/estimation/regression/program_make_conv_dummies_newmodel2.do"

use "processing\combined\ddc_data_urbancal_crprev", clear

keep if (initial_use == "Crop" & year == 2002) | (initial_use == "Forest" & year == 2002)

gen stay_in = (final_use == "Crop" & initial_use == "Crop") | (final_use == "Forest" & initial_use == "Forest")

bysort fips year lcc stay_in initial_use: egen prob = sum(weighted_ccp)
bysort fips year lcc stay_in initial_use: egen final_acres2 = sum(final_acres)
drop final_acres
rename final_acres2 final_acres
order stay_in prob, after(weighted_ccp)
order final_acres, after(initial_acres)

/* Calculate actual total final cropland */

egen total_acres = sum(final_acres) if final_use == "Crop"
summ total_acres 

/* Create shares data set */

collapse (mean) prob final_acres initial_acres, by(fips year initial_use lcc stay_in)

drop if stay_in == 0
drop if lcc == "0"
 
noi summ prob if prob != 1 & prob != 0 [aweight = initial_acres]

gen y = ln(prob) - ln(1-prob)

egen iuse_num = group(initial_use)

reg y i.iuse_num if initial_acres != . & initial_acres != 0 & stay_in == 1 [aweight = initial_acres]
predict resid, resid


gen phat = exp(_b[_cons] + resid)/(1 + exp(_b[_cons] + resid)) if initial_acres != . & initial_acres != 0 & stay_in == 1

noi summ phat [aweight = initial_acres]

egen total_initial_acres = sum(initial_acres)
egen total_acres_hat = sum(phat*initial_acres)
summ total_acres_hat
drop phat total_initial_acres total_acres_hat

restore

gen y = ln(prob) - ln(1-prob)
reg y stay_in [aweight = initial_acres]

gen phat = exp(_b[_cons] + _b[stay_in]*stay_in)/(1 + exp(_b[_cons] + _b[stay_in]*stay_in))
summ phat
bysort stay_in: summ phat
keep if stay_in == 1
egen total_initial_acres = sum(initial_acres)
egen total_acres_hat = sum(phat*initial_acres)
summ total_acres_hat

}
