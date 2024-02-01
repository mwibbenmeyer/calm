clear

cd L:/Project-Land_Use/


/*** GET INITIAL ACRES ***/
import delimited "processing\elasticity\initial_acres_county_lcc_all.csv"


/******************* ORIGINAL EQUILIBRIUM ***************************/
/*** GET PROBABILITY ESTIMATES ***/
merge 1:1 fips year initial_use lcc using "processing\elasticity\lcc_static_all_no2015\implied_prob_lcc_static.dta"
keep if year == 2015
keep year fips initial_use lcc initial_acres p1 p2 p3 p4 p5 k
sort fips lcc k
drop if p1 == . & p2 == . & p3 == . & p4 == . & p5 == .
drop if fips == 21125 //too large value functions (from crp_nr) and missing probabilities

ds, has(type numeric)
recast double `r(varlist)'

/*Calibrate the probabilities*/
gen p_check = .
replace p_check = p1 + p2 + p3 + p4 + p5 if initial_use == "Crop" | initial_use == "Other"
replace p_check = p2 + p3 + p4 + p5 if initial_use == "CRP"
replace p_check = p1 + p3 + p4 + p5 if initial_use == "Forest"
replace p_check = p1 if initial_use == "Urban"

replace p5 = 1 - p1 - p2 - p3 - p4 if p1 != . & p2 != . & p3 != . & p4 != .
replace p5 = 1 - p2 - p3 - p4 if p1 == . & p2 != . & p3 != . & p4 != .
replace p5 = 1 - p1 - p3 - p4 if p1 != . & p2 == . & p3 != . & p4 != .
replace p1 = 1 if p2 == . & p3 == . & p4 == . & p1 == .

replace p_check = p1 + p2 + p3 + p4 + p5 if initial_use == "Crop" | initial_use == "Other"
replace p_check = p2 + p3 + p4 + p5 if initial_use == "CRP"
replace p_check = p1 + p3 + p4 + p5 if initial_use == "Forest"
replace p_check = p1 if initial_use == "Urban"

/*** FIND THE EQUILIBRIUM ***/
/*First iteration*/

gen acres = initial_acres
gen acres_next = .
gen diff = .
ds, has(type numeric)
recast double `r(varlist)'

/*Acres depend on the number of feasible conversions*/	
by fips lcc: replace acres_next = acres * p1 + acres[_n+2] * p1[_n+2] + acres[_n+3] * p1[_n+3] + acres[_n+4] * p1[_n+4] if initial_use == "Urban"
by fips lcc: replace acres_next = acres * p2 + acres[_n+1] * p2[_n+1] + acres[_n+3] * p2[_n+3] if initial_use == "CRP"
by fips lcc: replace acres_next = acres * p3 + acres[_n-1] * p3[_n-1] + acres[_n+1] * p3[_n+1] + acres[_n+2] * p3[_n+2] if initial_use == "Crop"
by fips lcc: replace acres_next = acres * p4 + acres[_n-2] * p4[_n-2] + acres[_n-1] * p4[_n-1] + acres[_n+1] * p4[_n+1] if initial_use == "Forest"
by fips lcc: replace acres_next = acres * p5 + acres[_n-3] * p5[_n-3] + acres[_n-2] * p5[_n-2] + acres[_n-1] * p5[_n-1] if initial_use == "Other"

by fips lcc: replace diff = (acres_next - acres)^2 + (acres_next[_n+1] - acres[_n+1])^2 + (acres_next[_n+2] - acres[_n+2])^2 + (acres_next[_n+3] - acres[_n+3])^2 + (acres_next[_n+4] - acres[_n+4])^2 if initial_use == "Urban"
sum diff
local maxdiff = `r(max)'

replace acres = acres_next

// egen initial_acres_total = sum(initial_acres), by(fips)

/*Iterate until convergence*/
local iter = 1

 while `iter' <= 9 {

// while `maxdiff' > 0.00001  {
	
	local iter = `iter' + 1
	
	/*Acres depend on the number of feasible conversions*/	
	by fips lcc: replace acres_next = acres * p1 + acres[_n+2] * p1[_n+2] + acres[_n+3] * p1[_n+3] + acres[_n+4] * p1[_n+4] if initial_use == "Urban" & diff > 0.00001
	by fips lcc: replace acres_next = acres * p2 + acres[_n+1] * p2[_n+1] + acres[_n+3] * p2[_n+3] if initial_use == "CRP" & diff[_n-1] > 0.00001
	by fips lcc: replace acres_next = acres * p3 + acres[_n-1] * p3[_n-1] + acres[_n+1] * p3[_n+1] + acres[_n+2] * p3[_n+2] if initial_use == "Crop" & diff[_n-2] > 0.00001
	by fips lcc: replace acres_next = acres * p4 + acres[_n-2] * p4[_n-2] + acres[_n-1] * p4[_n-1] + acres[_n+1] * p4[_n+1] if initial_use == "Forest" & diff[_n-3] > 0.00001 
	by fips lcc: replace acres_next = acres * p5 + acres[_n-3] * p5[_n-3] + acres[_n-2] * p5[_n-2] + acres[_n-1] * p5[_n-1] if initial_use == "Other" & diff[_n-4] > 0.00001 

	by fips lcc: replace diff = (acres_next - acres)^2 + (acres_next[_n+1] - acres[_n+1])^2 + (acres_next[_n+2] - acres[_n+2])^2 + (acres_next[_n+3] - acres[_n+3])^2 + (acres_next[_n+4] - acres[_n+4])^2 if initial_use == "Urban"
	sum diff
	local maxdiff = `r(max)'

	replace acres = acres_next

}

di `iter'

// egen acres_total = sum(acres), by(fips)

tabstat acres initial_acres, stat(sum) by(initial_use)

// br if abs((acres_total-initial_acres_total)/initial_acres_total) >= 1 & initial_acres_total != 0 & acres_total != .

/******************* NEW EQUILIBRIUM ***************************/
/*** GET PROBABILITY ESTIMATES ***/
rename acres acres_baseline
drop acres_next

foreach k in urban crp crop forest other {
	
	keep year fips initial_use lcc initial_acres acres_*

	merge 1:1 fips year initial_use lcc using "processing\elasticity\lcc_static_all_no2015\implied_prob_lcc_static_`k'10pct.dta"
	keep if year == 2015
	keep year fips initial_use lcc initial_acres acres_* p1 p2 p3 p4 p5 k
	sort fips lcc k
	drop if p1 == . & p2 == . & p3 == . & p4 == . & p5 == .
	drop if fips == 21125 //too large value functions and missing probabilities
	
	ds, has(type numeric)
	recast double `r(varlist)'

	/*Calibrate the probabilities*/
	gen p_check = .
	replace p_check = p1 + p2 + p3 + p4 + p5 if initial_use == "Crop" | initial_use == "Other"
	replace p_check = p2 + p3 + p4 + p5 if initial_use == "CRP"
	replace p_check = p1 + p3 + p4 + p5 if initial_use == "Forest"
	replace p_check = p1 if initial_use == "Urban"

	replace p5 = 1 - p1 - p2 - p3 - p4 if p1 != . & p2 != . & p3 != . & p4 != .
	replace p5 = 1 - p2 - p3 - p4 if p1 == . & p2 != . & p3 != . & p4 != .
	replace p5 = 1 - p1 - p3 - p4 if p1 != . & p2 == . & p3 != . & p4 != .
	replace p1 = 1 if p2 == . & p3 == . & p4 == . & p1 == .

	replace p_check = p1 + p2 + p3 + p4 + p5 if initial_use == "Crop" | initial_use == "Other"
	replace p_check = p2 + p3 + p4 + p5 if initial_use == "CRP"
	replace p_check = p1 + p3 + p4 + p5 if initial_use == "Forest"
	replace p_check = p1 if initial_use == "Urban"

	/*** FIND THE EQUILIBRIUM ***/
	/*First iteration*/

	gen acres_`k'10pct = initial_acres
	gen acres_`k'10pct_next = .
	gen diff = .
	ds, has(type numeric)
	recast double `r(varlist)'

	/*Acres depend on the number of feasible conversions*/	
	by fips lcc: replace acres_`k'10pct_next = acres_`k'10pct * p1 + acres_`k'10pct[_n+2] * p1[_n+2] + acres_`k'10pct[_n+3] * p1[_n+3] + acres_`k'10pct[_n+4] * p1[_n+4] if initial_use == "Urban"
	by fips lcc: replace acres_`k'10pct_next = acres_`k'10pct * p2 + acres_`k'10pct[_n+1] * p2[_n+1] + acres_`k'10pct[_n+3] * p2[_n+3] if initial_use == "CRP"
	by fips lcc: replace acres_`k'10pct_next = acres_`k'10pct * p3 + acres_`k'10pct[_n-1] * p3[_n-1] + acres_`k'10pct[_n+1] * p3[_n+1] + acres_`k'10pct[_n+2] * p3[_n+2] if initial_use == "Crop"
	by fips lcc: replace acres_`k'10pct_next = acres_`k'10pct * p4 + acres_`k'10pct[_n-2] * p4[_n-2] + acres_`k'10pct[_n-1] * p4[_n-1] + acres_`k'10pct[_n+1] * p4[_n+1] if initial_use == "Forest"
	by fips lcc: replace acres_`k'10pct_next = acres_`k'10pct * p5 + acres_`k'10pct[_n-3] * p5[_n-3] + acres_`k'10pct[_n-2] * p5[_n-2] + acres_`k'10pct[_n-1] * p5[_n-1] if initial_use == "Other"
	
	by fips lcc: replace diff = (acres_`k'10pct_next - acres_`k'10pct)^2 + (acres_`k'10pct_next[_n+1] - acres_`k'10pct[_n+1])^2 + (acres_`k'10pct_next[_n+2] - acres_`k'10pct[_n+2])^2 + (acres_`k'10pct_next[_n+3] - acres_`k'10pct[_n+3])^2 + (acres_`k'10pct_next[_n+4] - acres_`k'10pct[_n+4])^2 if initial_use == "Urban"
	sum diff
	local maxdiff = `r(max)'
	replace acres_`k'10pct = acres_`k'10pct_next

	/*Iterate until convergence*/
	local iter = 1

	while `iter' <= 9 {

	// while `maxdiff' > 0.00001 {

		local iter = `iter' + 1
		
		/*Acres depend on the number of feasible conversions*/	
		by fips lcc: replace acres_`k'10pct_next = acres_`k'10pct * p1 + acres_`k'10pct[_n+2] * p1[_n+2] + acres_`k'10pct[_n+3] * p1[_n+3] + acres_`k'10pct[_n+4] * p1[_n+4] if initial_use == "Urban" & diff > 0.00001 
		by fips lcc: replace acres_`k'10pct_next = acres_`k'10pct * p2 + acres_`k'10pct[_n+1] * p2[_n+1] + acres_`k'10pct[_n+3] * p2[_n+3] if initial_use == "CRP" & diff[_n-1] > 0.00001 
		by fips lcc: replace acres_`k'10pct_next = acres_`k'10pct * p3 + acres_`k'10pct[_n-1] * p3[_n-1] + acres_`k'10pct[_n+1] * p3[_n+1] + acres_`k'10pct[_n+2] * p3[_n+2] if initial_use == "Crop" & diff[_n-2] > 0.00001 
		by fips lcc: replace acres_`k'10pct_next = acres_`k'10pct * p4 + acres_`k'10pct[_n-2] * p4[_n-2] + acres_`k'10pct[_n-1] * p4[_n-1] + acres_`k'10pct[_n+1] * p4[_n+1] if initial_use == "Forest" & diff[_n-3] > 0.00001 
		by fips lcc: replace acres_`k'10pct_next = acres_`k'10pct * p5 + acres_`k'10pct[_n-3] * p5[_n-3] + acres_`k'10pct[_n-2] * p5[_n-2] + acres_`k'10pct[_n-1] * p5[_n-1] if initial_use == "Other" & diff[_n-4] > 0.00001 
		
		by fips lcc: replace diff = (acres_`k'10pct_next - acres_`k'10pct)^2 + (acres_`k'10pct_next[_n+1] - acres_`k'10pct[_n+1])^2 + (acres_`k'10pct_next[_n+2] - acres_`k'10pct[_n+2])^2 + (acres_`k'10pct_next[_n+3] - acres_`k'10pct[_n+3])^2 + (acres_`k'10pct_next[_n+4] - acres_`k'10pct[_n+4])^2 if initial_use == "Urban"
		sum diff
		local maxdiff = `r(max)'
		replace acres_`k'10pct = acres_`k'10pct_next

	}

	di `iter'
	
	drop acres_`k'10pct_next

}

drop p1 p2 p3 p4 p5 diff
tabstat acres_baseline acres_urban10pct acres_crp10pct acres_crop10pct acres_forest10pct acres_other10pct, stat(sum) by(initial_use) save
return list


/******************* ELASTICITY ***************************/
// di "crop_elasticity = " (r(Stat2)[1,2] - r(Stat2)[1,1])/r(Stat2)[1,1]/0.1
// di "crp_elasticity = " (r(Stat1)[1,3] - r(Stat1)[1,1])/r(Stat1)[1,1]/0.1
//
// di "crop_cross_elasticity = " (r(Stat2)[1,3] - r(Stat2)[1,1])/r(Stat2)[1,1]/0.1
// di "crp_cross_elasticity = " (r(Stat1)[1,2] - r(Stat1)[1,1])/r(Stat1)[1,1]/0.1

putexcel set processing\elasticity\lcc_static_all_no2015\final_acres_lcc_static, replace
putexcel A2 = "Urban"
putexcel A3 = "CRP"
putexcel A4 = "Crop"
putexcel A5 = "Forest"
putexcel A6 = "Other"
putexcel B1 = "baseline"
putexcel C1 = "urban10pct"
putexcel D1 = "crp10pct"
putexcel E1 = "crop10pct"
putexcel F1 = "forest10pct"
putexcel G1 = "other10pct"
putexcel B2 = matrix(r(Stat5))
putexcel B3 = matrix(r(Stat1))
putexcel B4 = matrix(r(Stat2))
putexcel B5 = matrix(r(Stat3))
putexcel B6 = matrix(r(Stat4))


/******************* CHECK DATA QUALITY ***************************/

// preserve
// keep if initial_use == "Crop" &  (acres_baseline + acres_baseline[_n-1] - 1.05*(initial_acres + initial_acres[_n-1]) > 0)
// levelsof fips, local(FIPS)
// restore
//
// br if abs(acres_baseline - acres_crop10pct)/acres_baseline >= 1.05 & acres_baseline != 0

// capture drop error
// gen error = 1
// foreach m of local FIPS {
// 	di `m'
// 	replace error = 1 if !error & fips == "`m'"
// }
