cd "L:/Project-Land_Use/"

*log using "C:/Users/Matt/Downloads/ddc_test.log", replace

global beta_annual = 0.9

/* Import and format returns data */
use "processing/net_returns/combined_returns_panel_other.dta", clear
tempfile tmp
save "`tmp'"

/* Import CCPs */
import delimited "processing/ccp/ccps.csv", clear

*Convert fips to string
gen str5 fips_str = string(fips,"%05.0f")
drop fips
rename fips_str fips
order fips, before(year)

*Convert acres to float
destring initial_acres, replace ignore("NA")
destring total_acres, replace ignore("NA")
rename total_acres final_acres

*Correct county code fips code change
replace fips = "12025" if fips == "12086"

/* Merge CCPs with returns data */
merge m:1 fips year using "`tmp'"
drop if _merge == 2 /*Drop obs for years we don't have CCPs*/

/* Format and organize data set */

local varlist fips year lcc initial_use final_use initial_acres final_acres weighted_ccp statefips stateName stateAbbrev crop_nr forest_nr urban_nr other_nr 
keep `varlist'
order `varlist'

save "processing/combined/combined_ccp_returns.dta", replace 
