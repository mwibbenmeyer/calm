cd "L:/Project-Land_Use/"

/* Import and format returns data */
import delimited "processing/net_returns/combined_returns_panel.csv", clear
gen str5 fips_str = string(fips,"%05.0f")
drop fips
rename fips_str fips
tempfile tmp
save "`tmp'"

/* Import CCPs */
import delimited "processing/ccp/ccps_crprev.csv", clear

*Convert fips to string
gen str5 fips_str = string(fips,"%05.0f")
drop fips
rename fips_str fips
order fips, before(year)

*Convert acres to float
destring initial_acres, replace ignore("NA")
destring total_acres, replace ignore("NA")
rename total_acres final_acres


/* Merge CCPs with returns data */
merge m:1 fips year using "`tmp'"
drop if _merge == 2 /*Drop obs for years we don't have CCPs*/

/* Format and organize data set */

local varlist fips year lcc initial_use final_use initial_acres final_acres weighted_ccp crop_nr forest_nr other_nr urban_nr
keep `varlist'
order `varlist'

/* Label variables */
label variable fips "FIPS Code"
label variable year "Year"
label variable lcc "Land Capability Class"
label variable initial_use "Land use in year t-5 (year t-3 in 2015)"
label variable final_use "Land use in year t"
label var initial_acres "Acres in initial use (county x LCC) in year t-5"
label var final_acres "Acres converted from initial to final use (county x LCC) from year t-5 to year t"
label variable weighted_ccp "Conditional choice probability"

/* Destring numeric variables */
label variable other_nr "2010USD average net returns per acre on other land uses (CRP, range, pasture)"
label variable forest_nr "2010USD annualized net returns/acre of bare forestland [Wear/RFF]"
label variable crop_nr "Current USD annualized net returns/acre of cropland [USDA/RFF]"
label variable urban_nr "Average annual percentage population growth"

save "processing/combined/combined_ccp_returns.dta", replace 
