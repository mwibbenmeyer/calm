clear all

use "L:\Project-Land_Use\processing_output\countypanel_estimation_bal.dta"
cd "L:\Project-Land_Use"


// Keep crop-to-crop and crop-to-forest transitions
keep if initial_use == "Crop"
keep if final_use == "Crop" | final_use == "Forest"
drop if nr ==.
drop if lcc == "0"

// Create land capability class (lcc) dummies
gen lcc_3_4 = 0
replace lcc_3_4 = acresk if lcc == "3_4"
gen lcc_5_6 = 0
replace lcc_5_6 = acresk if lcc == "5_6"
gen lcc_7_8 = 0
replace lcc_7_8 = acresk if lcc == "7_8"


// Collapse data to year by county
collapse nr lcc_3_4 lcc_5_6 lcc_7_8 (sum) acresk, by(stateAbbrev countyName fips year initial_use final_use)


drop if initial_use == "Crop" & final_use == "Crop" & acresk == 0



// ======================== Create Variables ================================ //

// Create ratio of acres converted to forest and acres staying in forest
gen outside_acres_temp = acresk if initial_use == final_use
bysort year county: egen outside_acres = max(outside_acres_temp)
drop if outside_acres ==.
drop if initial_use == final_use
gen ratio = acresk/outside_acres


// ==================== Estimate Hyperbolic Sine ============================ //
// The hyperbolic sine transformation and implied elasticities are found here:
// https://marcfbellemare.com/wordpress/wp-content/uploads/2019/02/BellemareWichmanIHSFebruary2019.pdf


gen ratio_h = ln(ratio + sqrt(ratio^2 + 1))
gen nr_h = ln(nr + sqrt(nr^2 + 1))

// OLS
xi: reg ratio_h nr_h i.year, vce(cluster stateAbbrev)
outreg2 using estimation_results.xls, excel replace label ctitle("OLS")

predict xb
local beta_nr_h =_b[nr_h]


// Elasticity
gen y = sinh(xb)
gen e = `beta_nr_h'*sqrt(y^2 + 1)/y*nr/(sqrt(nr^2+1))

summarize e [w=outside_acres], detail


// IV 
bysort stateAbbrev: egen total_nr = sum(nr)
gen count = 1
bysort stateAbbrev: egen total_counties = sum(count)
gen hausman = (total_nr - nr)/(total_counties - 1)


xi: ivreg ratio_h (nr_h = hausman) i.year, robust
outreg2 using estimation_results.xls, excel append label ctitle("IV")
predict xb_IV
local beta_nr_h_IV =_b[nr_h]


// Elasticity
gen y_IV = sinh(xb_IV)
gen e_IV = `beta_nr_h_IV'*sqrt(y_IV^2 + 1)/y_IV*nr/(sqrt(nr^2+1))

summarize e_IV [w=outside_acres], detail
