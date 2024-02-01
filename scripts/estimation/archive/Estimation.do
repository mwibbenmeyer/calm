clear all

use "L:\Project-Land_Use\processing_output\countypanel_estimation_bal.dta"

// Keep one transition
keep if year == 2002

// Keep crop-to-crop and crop-to-forest transitions
keep if initial_use == "Crop"
keep if final_use == "Crop" | final_use == "Forest"
drop if nr ==.
drop if initial_use == "Crop" & final_use == "Crop" & acresk == 0
drop if lcc == "0"



// ======================== Create Variables ================================ //

// Dependent variable: Create ratio of acres converted to forest and acres staying in forest
gen outside_acres_temp = acresk if initial_use == final_use
bysort year county: egen outside_acres = max(outside_acres_temp)
drop if outside_acres == .
drop if initial_use == final_use
gen ratio = acresk/outside_acres


// Create land capability class (lcc) dummies
gen lcc_3_4 = 0
replace lcc_3_4 = 1 if lcc == "3_4"
gen lcc_5_6 = 0
replace lcc_5_6 = 1 if lcc == "5_6"
gen lcc_7_8 = 0
replace lcc_7_8 = 1 if lcc == "7_8"


// Create net returns interactions
gen nr_lcc_3_4 = nr*lcc_3_4
gen nr_lcc_5_6 = nr*lcc_5_6
gen nr_lcc_7_8 = nr*lcc_7_8


// ======================== Estimate Poisson ================================ //
poisson ratio nr nr_lcc_3_4 nr_lcc_5_6 nr_lcc_7_8 lcc_3_4 lcc_5_6 lcc_7_8, vce(boot)

local beta_nr =_b[nr]
local beta_nr_lcc_3_4 = _b[nr_lcc_3_4]
local beta_nr_lcc_5_6 = _b[nr_lcc_5_6]
local beta_nr_lcc_7_8 = _b[nr_lcc_7_8]
local beta_lcc_3_4 = _b[lcc_3_4]
local beta_lcc_5_6 = _b[lcc_5_6]
local beta_lcc_7_8 = _b[lcc_7_8]
local beta_cons = _b[_cons]


// ======================== Calculate Elasticity ============================ //

// Utility/profit
gen u = nr*`beta_nr' + nr_lcc_3_4*`beta_nr_lcc_3_4' + nr_lcc_5_6*`beta_nr_lcc_5_6' + ///
        nr_lcc_7_8*`beta_nr_lcc_7_8' + lcc_3_4*`beta_lcc_3_4' + lcc_5_6*`beta_lcc_5_6' + ///
		lcc_7_8*`beta_lcc_7_8' + `beta_cons'

// Choice probability, conditional on choosing either to stay in land use or to transition to a particular land use
// Note that this should be similar to the broad choice set probability since 
// most land stays in its original use
gen prob_hat = exp(u)/(1+exp(u))		
		
// Elasticity formula is on bottom of page 59 of Train (2009)		
gen elasticity = `beta_nr' + lcc_3_4*`beta_nr_lcc_3_4' + lcc_5_6*`beta_nr_lcc_5_6' + lcc_7_8*`beta_nr_lcc_7_8' 
replace elasticity = elasticity*nr*(1 - prob_hat)

gen wt = acresk + outside_acres
summarize elasticity [w=wt], detail
summarize elasticity if lcc_7_8 == 0 [w=wt], detail
