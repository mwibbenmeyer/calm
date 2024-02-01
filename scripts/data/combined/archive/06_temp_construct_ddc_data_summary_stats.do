cd L:/Project-Land_Use/

use "processing\combined\full_combined_returns", clear

global beta = 0.9

/******************************************************************************/
/* MAKE ADJUSTMENTS TO CCPS			   										*/ 

/*Adjust 3-year conversion probabilities in 2012-2015 to be 5-year conversion probabilities*/
gen one_year_ccp = 1 - (1-weighted_ccp)^(1/3) if initial_use != final_use & year == 2015 
replace weighted_ccp = 1 - (1 - one_year_ccp)^5 if initial_use != final_use & year == 2015 
bysort fips lcc initial_use: egen sum_ccp = sum(weighted_ccp) if year == 2015 & initial_use != final_use 
bysort fips lcc initial_use: egen max_ccp = max(sum_ccp) if year == 2015 
replace weighted_ccp = 1 - max_ccp if year == 2015 & initial_use == final_use
drop sum_ccp max_ccp one_year_ccp

/* Replace zero CCPs with minimum values */

bysort initial_use final_use lcc year: egen min_ccp = min(weighted_ccp) 
replace weighted_ccp = min_ccp if weighted_ccp == 0


/******************************************************************************/
/* CONSTRUCT DEPENDENT VARIABLE		   										*/ 

/* Dropping conversions out of urban   										*/ 
drop if initial_use == "Urban" & final_use != "Urban"

preserve 

keep if final_use == initial_use
rename weighted_ccp pkk
label var pkk "CCP of remaining in same use from year t-5 to year t"
tempfile pkk
save `pkk'

restore

/*Merge on initial use because here because this is probability of staying in initial use*/
merge m:1 fips year lcc initial_use using `pkk', keepusing(pkk) keep(match master)

preserve 

gen next_year = cond(year <= 2012,year-5,year-3)

rename weighted_ccp pjj
label var pjj "CCP of remaining in use j in year t+5 conditional on converting to use j in year t"
keep if final_use == initial_use
drop year
rename next_year year
tempfile pjj
save `pjj'

restore

/*Merge on final use because here use j is chosen in t and point continues in j next period*/
drop _merge
merge m:1 fips year lcc final_use using `pjj', keepusing(pjj) keep(match master) 

preserve 

gen next_year = cond(year <= 2012,year-5,year-3)

rename weighted_ccp pjk
label var pjk "CCP of converting to use j in year t+5 conditional on staying in k in year t"
drop year
rename next_year year
tempfile pjk
save `pjk'

restore

/*Merge on initial use and final use here*/
drop _merge
merge m:1 fips year lcc initial_use final_use using `pjk', keepusing(pjk) keep(match master)

gen y = ln(weighted_ccp/pkk) + $beta * ln(pjj/pjk)
gen y_st = ln(weighted_ccp/pkk) + 0 * ln(pjj/pjk)


/********************************************************************************/
/* Some summary statistics */

gen frac = (y*y_st < 0)
summ frac

bysort initial_use: summ y y_st if initial_use != final_use

summ weighted_ccp pjk if initial_use == "Crop"
summ pkk pjj if initial_use == "Crop"

gen diff = y - y_st

bysort year: summ y if initial_use == "Crop"
bysort final_use : summ y if initial_use == "Crop"
bysort final_use year: summ y if initial_use == "Crop"

br fips year lcc initial_use final_use weighted_ccp pkk pjj pjk y y_st if initial_use == "Crop" & final_use =="Other"