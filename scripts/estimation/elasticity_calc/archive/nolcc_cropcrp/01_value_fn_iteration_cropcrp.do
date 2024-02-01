clear

cd L:/Project-Land_Use/


/*** GET ESTIMATION RESULTS ***/
import delimited "results\initial_estimation\regs_2021-10\regs_2021-10-29_unweighted_cropcrp.txt", varnames(2) 
keep if variables != "" & y != ""
rename (variables y) (regressor coef)

gen coef_n = subinstr(coef, "*", "", .)
replace coef_n = subinstr(coef_n, ",", "", .)
replace coef_n = subinstr(coef_n, "NO", "0", .)
replace coef_n = subinstr(coef_n, "YES", "1", .)
destring coef_n, replace force


/* Extract coefficients and save as locals */
* Unweighted results are used below
sum coef_n if regressor == "Constant"
local eta_1to2 = r(mean) /* Conversion cost - Crops to CRP */
sum coef_n if regressor == "CRPtoCrop"
local eta_2to1 = r(mean) + `eta_1to2' /* Conversion cost - CRP to Crops */
sum coef_n if regressor == "c.crop_nr#c.dCrop"
local theta_1 = r(mean) /* Coefficient of net returns - Crops */
sum coef_n if regressor == "c.CRP_nr#c.dCRP"
local theta_2 = r(mean) /* Coefficient of net returns - CRP */

/*** GET COUNTY RENTS ***/
clear 
use "processing\combined\ddc_data_cropcrp", clear
keep fips year initial_use crop_nr CRP_nr
keep if CRP_nr != . & crop_nr != .

/* Calculate probabilities for 2015 only */
keep if year==2015
duplicates drop fips year initial_use, force

/* Dummy rents */

gen P_1 = crop_nr /* Crops */
gen P_2 = CRP_nr /* CRP */

gen k = .
replace k = 1 if initial_use == "Crop"
replace k = 2 if initial_use == "CRP"
sort fips k

tempfile returns
save `returns'

/***************************************************************/
/*** SET UP DATA SET FOR ITERATION ***/

/* Data set for value function iteration */

gen v = 0
gen vnext = v

/* Set coefficients and constants */
gen theta_1 = `theta_1' /* Crops */
gen theta_2 = `theta_2' /* CRP */
gen beta = 0.90
gen gamma = 0.577216

gen eta_1 = .
gen eta_2 = .

/*Conversions to crops*/
replace eta_1 = 0 if k == 1
replace eta_1 = `eta_2to1' if k == 2

/*Conversions to CRP*/
replace eta_2 = `eta_1to2' if k == 1
replace eta_2 = 0 if k == 2

/***************************************************************/
/*** VALUE FUNCTION ITERATION ***/

/*First iteration*/

gen vcond1 = .
gen vcond2 = .
gen diff = .
// keep if fips == 1001 //temporary

forvalues j = 1/2 {	
	
	by fips: replace vcond`j' = eta_`j' + theta_`j'*P_`j' + beta*v[`j']
		
}
		
replace vnext = ln(exp(vcond1) + exp(vcond2)) + gamma
replace diff = abs(v - vnext)
sum diff
local maxdiff = `r(max)'


/*Iterate until convergence*/
local iter = 1

while `maxdiff' > 0.00000001 {

	local iter = `iter' + 1
	replace v = vnext

	forvalues j = 1/2 {	
		
		by fips: replace vcond`j' = eta_`j' + theta_`j'*P_`j' + beta*v[`j']
			
	}
			
	replace vnext = ln(exp(vcond1) + exp(vcond2)) + gamma
	replace diff = abs(v - vnext)
	sum diff
	local maxdiff = `r(max)'
	
}

di "`iter'"

/*** CALCULATE IMPLIED PROBABILITIES ***/

gen num = .
gen p1 = .
gen p2 = .
gen denom = exp(vcond1) + exp(vcond2)

ds, has(type numeric)
recast double `r(varlist)'

forvalues j = 1/2 {
		replace num = exp(vcond`j')
		replace p`j' = num/denom
	}
	
/*** VISUALIZE THE PROBABILITIES ***/
sum p1, detail
sum p2, detail

tab initial_use, sum(p1)
tab initial_use, sum(p2)

tabstat p1 p2, stat(mean) by(initial_use) nototal save

putexcel set processing\elasticity\nolcc_cropcrp\dy_prob, replace
putexcel A2 = "Crop"
putexcel A3 = "CRP"
putexcel B1 = "p1"
putexcel C1 = "p2"
putexcel B2 = matrix(r(Stat2))
putexcel B3 = matrix(r(Stat1))

save "processing\elasticity\nolcc_cropcrp\implied_prob_cropcrp.dta", replace
