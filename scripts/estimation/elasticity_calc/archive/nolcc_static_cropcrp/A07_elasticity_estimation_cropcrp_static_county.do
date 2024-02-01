clear

cd L:/Project-Land_Use/


/*** GET INITIAL ACRES ***/
import delimited "processing\elasticity\initial_acres_county.csv"


/******************* ORIGINAL EQUILIBRIUM ***************************/
/*** GET PROBABILITY ESTIMATES ***/
merge 1:1 fips year initial_use using "processing\elasticity\nolcc_static_cropcrp\implied_prob_cropcrp_static.dta"
keep if year == 2015
keep year fips initial_use initial_acres p1 p2


/*** FIND THE EQUILIBRIUM ***/
/*First iteration*/

gen acres = initial_acres
gen acres_next = .
gen diff = .
	
by fips: replace acres_next = acres * p1 + acres[_n-1] * p1[_n-1] if initial_use == "Crop"
by fips: replace acres_next = acres * p2 + acres[_n+1] * p2[_n+1] if initial_use == "CRP"

by fips: replace diff = (acres_next - acres)^2 + (acres_next[_n-1] - acres[_n-1])^2 if initial_use == "Crop"
sum diff
local maxdiff = `r(max)'


/*Iterate until convergence*/
local iter = 1

while `maxdiff' > 0.0000001 {
	
	local iter = `iter' + 1
	replace acres = acres_next
	
	by fips: replace acres_next = acres * p1 + acres[_n-1] * p1[_n-1] if initial_use == "Crop"
	by fips: replace acres_next = acres * p2 + acres[_n+1] * p2[_n+1] if initial_use == "CRP"

	by fips: replace diff = (acres_next - acres)^2 + (acres_next[_n-1] - acres[_n-1])^2 if initial_use == "Crop"
	sum diff
	local maxdiff = `r(max)'

}

di `iter'


/******************* NEW EQUILIBRIUM ***************************/
/*** GET PROBABILITY ESTIMATES ***/
rename acres acres_baseline
drop acres_next

foreach k in crop crp {
	
	keep year fips initial_use initial_acres acres_*

	merge 1:1 fips year initial_use using "processing\elasticity\nolcc_static_cropcrp\implied_prob_cropcrp_static_`k'10pct.dta"
	keep if year == 2015
	keep year fips initial_use initial_acres acres_* p1 p2


	/*** FIND THE EQUILIBRIUM ***/
	/*First iteration*/

	gen acres_`k'10pct = initial_acres
	gen acres_`k'10pct_next = .
	gen diff = .
		
	by fips: replace acres_`k'10pct_next = acres_`k'10pct * p1 + acres_`k'10pct[_n-1] * p1[_n-1] if initial_use == "Crop"
	by fips: replace acres_`k'10pct_next = acres_`k'10pct * p2 + acres_`k'10pct[_n+1] * p2[_n+1] if initial_use == "CRP"

	by fips: replace diff = (acres_`k'10pct_next - acres_`k'10pct)^2 + (acres_`k'10pct_next[_n-1] - acres_`k'10pct[_n-1])^2 if initial_use == "Crop"
	sum diff
	local maxdiff = `r(max)'

	/*Iterate until convergence*/
	local iter = 1

	while `maxdiff' > 0.0000001 {

		local iter = `iter' + 1
		replace acres_`k'10pct = acres_`k'10pct_next
		
		by fips: replace acres_`k'10pct_next = acres_`k'10pct * p1 + acres_`k'10pct[_n-1] * p1[_n-1] if initial_use == "Crop"
		by fips: replace acres_`k'10pct_next = acres_`k'10pct * p2 + acres_`k'10pct[_n+1] * p2[_n+1] if initial_use == "CRP"

		by fips: replace diff = (acres_`k'10pct_next - acres_`k'10pct)^2 + (acres_`k'10pct_next[_n-1] - acres_`k'10pct[_n-1])^2 if initial_use == "Crop"
		sum diff
		local maxdiff = `r(max)'

	}

	di `iter'
	
	drop acres_`k'10pct_next

}

drop p1 p2 diff
tabstat acres_baseline acres_crop10pct acres_crp10pct, stat(sum) by(initial_use) save
return list //r(Stat2) is crop and r(Stat1) is crp


/******************* ELASTICITY ***************************/
di "crop_elasticity = " (r(Stat2)[1,2] - r(Stat2)[1,1])/r(Stat2)[1,1]/0.1
di "crp_elasticity = " (r(Stat1)[1,3] - r(Stat1)[1,1])/r(Stat1)[1,1]/0.1

di "crop_cross_elasticity = " (r(Stat2)[1,3] - r(Stat2)[1,1])/r(Stat2)[1,1]/0.1
di "crp_cross_elasticity = " (r(Stat1)[1,2] - r(Stat1)[1,1])/r(Stat1)[1,1]/0.1
