/****************************************************************************/
/* 																			*/
/*Program to simulate dynamic land use decisions and estimate dynamic model */
/* 																			*/
/****************************************************************************/
clear

cd "C:\Users\bleard\Dropbox\Sloan\Land Use\Dynamic model\Monte Carlo estimation"

use "C:\Users\bleard\Dropbox\Sloan\Land Use\Dynamic model\Monte Carlo estimation\simulated_data.dta"

/*
/* Estimate model using simple conditional logit */
egen cfinal_use = group(final_use)
xi: clogit choice i.cfinal_use i.cfinal_use*R, group(id) vce(cluster group)
drop _I*

/* Result: This seems to do okay at estimating theta_A and theta_B, at least
when beta is close to 1. The estimated intercept term is not accurate. */
*/


/* Estimate shares model */
collapse (mean) choice R, by(initial_use final_use group)
sort group initial_use final_use


// ADD THIS LATER IF NECESSARY: Impute 0 and 1 shares

////////////////////////////////////////////////////////////////////////////////
// Added 6-8-22: estimate static shares model
gen choice_outside = 1 - choice if final_use ~= initial_use 

gen R_outside_temp = R if final_use == initial_use
bysort group initial_use: egen R_outside = max(R_outside_temp)

keep if initial_use ~= final_use

gen lnshare = ln(choice)
gen lnshare_outside = ln(choice_outside)
gen ln_share_diff = lnshare - lnshare_outside

reg ln_share_diff R R_outside if initial_use == "A"
reg ln_share_diff R R_outside if initial_use == "B"


////////////////////////////////////////////////////////////////////////////////
// Added 6-20-22: estimate dynamic model

gen lnshare_outside_A_temp = lnshare_outside if initial_use == "A" 
gen lnshare_outside_B_temp = lnshare_outside if initial_use == "B" 
bysort group: egen lnshare_outside_A = mean(lnshare_outside_A_temp)
bysort group: egen lnshare_outside_B = mean(lnshare_outside_B_temp)
drop lnshare_outside_A_temp lnshare_outside_B_temp

gen lnshare_outside_other=.
replace lnshare_outside_other = lnshare_outside_A if initial_use == "B"
replace lnshare_outside_other = lnshare_outside_B if initial_use == "A"

// This is the second term in Araujo et al. Equation (9) in difference form, which is equivalent to the fractional form in the paper, since ln(x) - ln(y) = ln(x/y)
gen beta_ln_share_diff = $beta*(lnshare - lnshare_outside_other)

// This is the dependent variable 
gen y = ln_share_diff - beta_ln_share_diff

// Dynamic model regressions
reg y R R_outside if initial_use == "A"
reg y R R_outside if initial_use == "B"