clear

cd L:/Project-Land_Use/


/*** GET INITIAL ACRES ***/
import delimited "processing\elasticity\initial_acres_county.csv"


/******************* ORIGINAL EQUILIBRIUM ***************************/
/*** GET PROBABILITY ESTIMATES ***/
merge 1:1 fips year initial_use using "processing\elasticity\nolcc_cropcrp\implied_prob_cropcrp.dta"
keep if year == 2015
keep year fips initial_use initial_acres p1 p2

ds, has(type numeric)
recast double `r(varlist)'

/*** FIND THE EQUILIBRIUM ***/
/*First iteration*/

gen initial_p1 = p1
gen next_p1 = .
gen initial_p2 = p2
gen next_p2 = .
gen diff = .
	
by fips: replace next_p1 = p1 * p1 + p2 * p1[_n-1] if initial_use == "Crop"
by fips: replace next_p1 = p1 * p1[_n+1] + p2 * p1 if initial_use == "CRP"
by fips: replace next_p2 = p1 * p2 + p2 * p2[_n-1] if initial_use == "Crop"
by fips: replace next_p2 = p1 * p2[_n+1] + p2 * p2 if initial_use == "CRP"

by fips: replace diff = (next_p1 - p1)^2 + (next_p1[_n-1] - p1[_n-1])^2 + (next_p2 - p2)^2 + (next_p2[_n-1] - p2[_n-1])^2 if initial_use == "Crop"
sum diff
local maxdiff = `r(max)'


/*Iterate until convergence*/
local iter = 1

while `maxdiff' > 0.0000001 {
	
	local iter = `iter' + 1
	replace p1 = next_p1
	replace p2 = next_p2
	
	by fips: replace next_p1 = p1 * p1 + p2 * p1[_n-1] if initial_use == "Crop" & diff > 0.0000001 
	by fips: replace next_p1 = p1 * p1[_n+1] + p2 * p1 if initial_use == "CRP" & diff[_n+1] > 0.0000001
	by fips: replace next_p2 = p1 * p2 + p2 * p2[_n-1] if initial_use == "Crop" & diff > 0.0000001
	by fips: replace next_p2 = p1 * p2[_n+1] + p2 * p2 if initial_use == "CRP" & diff[_n+1] > 0.0000001

	by fips: replace diff = (next_p1 - p1)^2 + (next_p1[_n-1] - p1[_n-1])^2 + (next_p2 - p2)^2 + (next_p2[_n-1] - p2[_n-1])^2 if initial_use == "Crop"
	sum diff
	local maxdiff = `r(max)'

}

di `iter'


/******************* NEW EQUILIBRIUM ***************************/
/*** GET PROBABILITY ESTIMATES ***/
rename p1 p1_baseline
rename p2 p2_baseline


foreach k in crop crp {
	
	keep year fips initial_use initial_acres p*

	merge 1:1 fips year initial_use using "processing\elasticity\nolcc_cropcrp\implied_prob_cropcrp_`k'10pct.dta"
	keep if year == 2015
	keep year fips initial_use initial_acres p*

	ds, has(type numeric)
	recast double `r(varlist)'

	/*** FIND THE EQUILIBRIUM ***/
	/*First iteration*/
	
	gen p1_`k'10pct = p1
	gen p2_`k'10pct = p2
		
	gen next_p1_`k'10pct = .
	gen next_p2_`k'10pct = .
	gen diff = .
	
	
	by fips: replace next_p1_`k'10pct = p1_`k'10pct * p1_`k'10pct + p2_`k'10pct * p1_`k'10pct[_n-1] if initial_use == "Crop"
	by fips: replace next_p1_`k'10pct = p1_`k'10pct * p1_`k'10pct[_n+1] + p2_`k'10pct * p1_`k'10pct if initial_use == "CRP"
	by fips: replace next_p2_`k'10pct = p1_`k'10pct * p2_`k'10pct + p2_`k'10pct * p2_`k'10pct[_n-1] if initial_use == "Crop"
	by fips: replace next_p2_`k'10pct = p1_`k'10pct * p2_`k'10pct[_n+1] + p2_`k'10pct * p2_`k'10pct if initial_use == "CRP"

	by fips: replace diff = (next_p1_`k'10pct - p1_`k'10pct)^2 + (next_p1_`k'10pct[_n-1] - p1_`k'10pct[_n-1])^2 + (next_p2_`k'10pct - p2_`k'10pct)^2 + (next_p2_`k'10pct[_n-1] - p2_`k'10pct[_n-1])^2 if initial_use == "Crop"
	sum diff
	local maxdiff = `r(max)'

	/*Iterate until convergence*/
	local iter = 1

	while `maxdiff' > 0.0000001 {

		local iter = `iter' + 1
		replace p1_`k'10pct = next_p1_`k'10pct
		replace p2_`k'10pct = next_p2_`k'10pct
		
		by fips: replace next_p1_`k'10pct = p1_`k'10pct * p1_`k'10pct + p2_`k'10pct * p1_`k'10pct[_n-1] if initial_use == "Crop" & diff > 0.0000001
		by fips: replace next_p1_`k'10pct = p1_`k'10pct * p1_`k'10pct[_n+1] + p2_`k'10pct * p1_`k'10pct if initial_use == "CRP" & diff[_n+1] > 0.0000001
		by fips: replace next_p2_`k'10pct = p1_`k'10pct * p2_`k'10pct + p2_`k'10pct * p2_`k'10pct[_n-1] if initial_use == "Crop" & diff > 0.0000001
		by fips: replace next_p2_`k'10pct = p1_`k'10pct * p2_`k'10pct[_n+1] + p2_`k'10pct * p2_`k'10pct if initial_use == "CRP" & diff[_n+1] > 0.0000001

		by fips: replace diff = (next_p1_`k'10pct - p1_`k'10pct)^2 + (next_p1_`k'10pct[_n-1] - p1_`k'10pct[_n-1])^2 + (next_p2_`k'10pct - p2_`k'10pct)^2 + (next_p2_`k'10pct[_n-1] - p2_`k'10pct[_n-1])^2 if initial_use == "Crop"
		sum diff
		local maxdiff = `r(max)'

	}

	di `iter'
	
	drop next_p1_`k'10pct
	drop p1 p2

}

drop diff
tabstat p1_baseline p1_crop10pct p1_crp10pct, stat(sum) by(initial_use) save
tabstat p2_baseline p2_crop10pct p2_crp10pct, stat(sum) by(initial_use) save


/*** GET FINAL ACRES ***/

foreach scenario in baseline crop10pct crp10pct {
	by fips: gen acres_`scenario' = initial_acres * p1_`scenario' + initial_acres[_n-1] * p1_`scenario'[_n-1] if initial_use == "Crop"
	by fips: replace acres_`scenario' = initial_acres * p2_`scenario' + initial_acres[_n+1] * p2_`scenario'[_n+1] if initial_use == "CRP"
}

tabstat acres_baseline acres_crop10pct acres_crp10pct, stat(sum) by(initial_use) save
return list //r(Stat2) is crop and r(Stat1) is crp

/******************* ELASTICITY ***************************/
di "crop_elasticity = " (r(Stat2)[1,2] - r(Stat2)[1,1])/r(Stat2)[1,1]/0.1
di "crp_elasticity = " (r(Stat1)[1,3] - r(Stat1)[1,1])/r(Stat1)[1,1]/0.1

di "crop_cross_elasticity = " (r(Stat2)[1,3] - r(Stat2)[1,1])/r(Stat2)[1,1]/0.1
di "crp_cross_elasticity = " (r(Stat1)[1,2] - r(Stat1)[1,1])/r(Stat1)[1,1]/0.1
