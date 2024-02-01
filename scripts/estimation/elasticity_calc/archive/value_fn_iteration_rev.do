clear

cd L:/Project-Land_Use/

/*** GET COUNTY RENTS ***/

/* Dummy rents */

qui {
set obs 5
gen k = _n

/* Set prices in $1000s USD because otherwise exp(vcond) gets too big for Stata*/
gen P_1 = .2 /* Crops */
gen P_2 = .012 /* Pasture */
gen P_3 = .017 /* Forest */
gen P_4 = 4 /* Urban */
gen P_5 = .010 /* Range */

gen eta_1 = .
gen eta_2 = .
gen eta_3 = .
gen eta_4 = .
gen eta_5 = .

/*Conversions to crops*/
replace eta_1 = 0 if k == 1
replace eta_1 = -.1 if k == 2
replace eta_1 = -.5 if k == 3
replace eta_1 = -5 if k == 4
replace eta_1 = -.2 if k == 5

/*Conversions to pasture*/
replace eta_2 = -.1 if k == 1
replace eta_2 = 0 if k == 2
replace eta_2 = -.5 if k == 3
replace eta_2 = -5 if k == 4
replace eta_2 = -.1 if k == 5

/*Conversions to forest*/
replace eta_3 = -.2 if k == 1
replace eta_3 = -.2 if k == 2
replace eta_3 = 0 if k == 3
replace eta_3 = -5 if k == 4
replace eta_3 = -.15 if k == 5

/*Conversions to urban*/
replace eta_4 = -1 if k == 1
replace eta_4 = -1 if k == 2
replace eta_4 = -1.5 if k == 3
replace eta_4 = 0 if k == 4
replace eta_4 = -1.2 if k == 5

/*Conversions to range*/
replace eta_5 = -.5 if k == 1
replace eta_5 = -.4 if k == 2
replace eta_5 = -.2 if k == 3
replace eta_5 = -5 if k == 4
replace eta_5 = 0 if k == 5

tempfile returns
save `returns'

/***************************************************************/
/*** SET UP DATA SET FOR ITERATION ***/

/* Data set for value function iteration */

clear
set obs 5

gen k = _n
gen v = 0 
gen vnext = v

merge 1:1 k using `returns', nogen

/* Set coefficients and constants */
gen theta_1 = 0.2
gen theta_2 = 0.05
gen theta_3 = 0.1
gen theta_4 = 0.15
gen theta_5 = 0.25
gen beta = 0.90
gen gamma = 0.577216

}

/***************************************************************/
/*** VALUE FUNCTION ITERATION ***/

/*First iteration*/
gen vcond1 = .
gen vcond2 = .
gen vcond3 = .
gen vcond4 = .
gen vcond5 = .

forvalues j = 1/5 {
	replace vcond`j' = eta_`j' + theta_`j'*P_`j' + beta*v[`j']
	}

gen lnsum = ln(exp(vcond1 + vcond2 + vcond3 + vcond4 + vcond5))
replace vnext = lnsum + gamma
gen diff = v - vnext
summ diff
local maxdiff = `r(max)'

/*Iterate until convergence*/
local iter = 1
qui while abs(`maxdiff') > 0.00000001 {

	local iter = `iter' + 1
	replace v = vnext

	forvalues j = 1/5 {
		replace vcond`j' = eta_`j' + theta_`j'*P_`j' + beta*v[`j']
		}
	replace lnsum = ln(exp(vcond1) + exp(vcond2) + exp(vcond3) + exp(vcond4) + exp(vcond5))
	replace vnext = lnsum + gamma
	replace diff = v - vnext
	summ diff
	local maxdiff = `r(max)'

}

di "`iter'"

/*** CALCULATE IMPLIED PROBABILITIES ***/

gen denom = exp(vcond1) + exp(vcond2) + exp(vcond3) + exp(vcond4) + exp(vcond5)

forvalues j = 1/5 {
	gen num = exp(vcond`j')
	gen p`j' = num/denom
	drop num
	}

list p*

