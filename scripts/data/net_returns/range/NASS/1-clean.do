<<<<<<< HEAD
/* Programmer: Alexandra Thompson
Start Date: November 24, 2020
Objective: Import, manage NASS data
Variable of interest: survey-economics-expenses-rent: rent, cash, pastureland, expense, measured in $/acre
*/

********************************************************************************
************SETUP************
********************************************************************************
set more off
clear

* working dir
global workingdir "L:\Project-Land_Use"
cd $workingdir

********************************************************************************
************IMPORT, MANAGE, SAVE************
********************************************************************************
* import
import delimited raw_data\net_returns\NASS\qs.economics_20201117.txt, clear

* keep data items (variables) of interest
keep if group_desc == "EXPENSES"
compress
keep if short_desc == "RENT, CASH, PASTURELAND - EXPENSE, MEASURED IN $ / ACRE"
compress
drop if value == "(S)" // drop state rows (no data)
drop if agg_level_desc == "NATIONAL"

* keep variables of interest
keep source_desc short_desc agg_level_desc state_alpha state_name state_fips_code county_code county_name region_desc asd_desc asd_code year value

* destring value variable
destring value, replace

* manage variables
ren value pasture_nr
label variable pasture_nr "RENT, CASH, PASTURELAND - EXPENSE, MEASURED IN $ / ACRE"
drop short_desc
assert source_desc == "SURVEY"
drop source_desc

* generate fips code
gen statefips2 = state_fips_code * 1000
gen fips = statefips2 + county_code
drop county_code statefips2

/* tag values which correspond to "OTHER COMBINED COUNTIES."
	When county data is not disclosed, rates are reported by NASS Crop Reporting District. See Table 2 for a listing of counties in each crop reporting districts.
			https://www.sites.ext.vt.edu/newsletter-archive/nass/2014.pdf */
replace agg_level_desc = "OTHER COMB COUNTIES" if county_name == "OTHER (COMBINED) COUNTIES"
replace fips = . if agg_level_desc == "OTHER COMB COUNTIES"

* drop alaska and hawaii
drop if state_fips_code == 2 | state_fips_code == 15

* save
compress
save processing\net_returns\NASS\pasturerents_all, replace
use processing\net_returns\NASS\pasturerents_all, clear

********************************************************************************
************MAKE A BLANK COUNTY PANEL************
**************with asd (agricultural stastitics district)***********************
* county-asd dictionary
	import excel "raw_data\net_returns\NASS\Listing of Counties and Districts used by USDA-NASS.xlsx", sheet("data") firstrow clear
	* generate fips code
	gen statefips2 = state * 1000
	gen fips = statefips2 + county
	ren state state_fips_code
	* new name
	gen county_name2 = name + E
	* drop if not a county
	drop if county == 0 // state-level
	drop if county == 888 // asd-combined counties
	drop if county == 999 // asd-level
	* drop if historical assignment
	drop if historyflag == 2
	* drop vars
	drop name E county statefips2 historyflag
	ren asd asd_code
	* drop alaska and hawaii
	drop if state_fips_code == 2 | state_fips_code == 15
	* save
	compress
	save processing\net_returns\NASS\county_list, replace
* merge county-asd dictionary to data counties
	use processing\net_returns\NASS\pasturerents_all, clear
	keep if agg_level_desc == "COUNTY"
	keep county_name state_alpha state_name fips asd* state_fips_code
	duplicates drop
	merge 1:1 state_fips_code asd_code fips using processing\net_returns\NASS\county_list
	assert _merge != 1 // check no unmatched from data
	drop _merge
	* check no duplicate counties
	duplicates tag fips, gen(tag)
	assert tag == 0
	drop tag
	* save
	compress
	save processing\net_returns\NASS\county_list, replace
* merge in state names
	use processing\net_returns\NASS\county_list, clear
	keep state_fips_code state_alpha state_name
	duplicates drop
	drop if state_alpha == ""
	ren state_alpha state_alpha2
	ren state_name state_name2
	merge 1:m state_fips_code using processing\net_returns\NASS\county_list
	assert _merge != 1 // check no unmatched from state list
	drop _merge
	drop state_alpha state_name
	ren state_alpha2 state_alpha
	ren state_name2 state_name
	* a couple of manual edits
	replace state_alpha = "AK" if state_fips_code == 2
		replace state_name = "ALASKA" if state_alpha == "AK"
	replace state_alpha = "CT" if state_fips_code == 9
		replace state_name = "CONNECTICUT" if state_alpha == "CT"
	replace state_alpha = "ME" if state_fips_code == 23
		replace state_name = "MAINE" if state_alpha == "ME"
	replace state_alpha = "NH" if state_fips_code == 33
		replace state_name = "NEW HAMPSHIRE" if state_alpha == "NH"
	replace state_alpha = "RI" if state_fips_code == 44
		replace state_name = "RHODE ISLAND" if state_alpha == "RI"
	* save
	compress
	save processing\net_returns\NASS\county_list, replace
* merge in multi-state regions
	import excel "raw_data\net_returns\NASS\Farm Production Regions.xlsx", sheet("Sheet1") firstrow clear
	merge 1:m state_alpha using processing\net_returns\NASS\county_list
	assert _merge == 3
	drop _merge
	* save
	compress
	save processing\net_returns\NASS\county_list, replace
* year list
	use processing\net_returns\NASS\pasturerents_all, clear
	keep year
	duplicates drop
	sort year
	gen n1 = _n
	su
	local nyears = r(N)
	di `nyears'
	save processing\net_returns\NASS\year_list, replace
* merge to years
	use processing\net_returns\NASS\county_list, clear
	expand `nyears'
	bysort fips: gen n1 = _n
	merge m:1 n1 using processing\net_returns\NASS\year_list
	assert _merge == 3
	drop _merge n1
	compress
	order multistateregion_desc state_fips_code state* asd* fips county* year
	save processing\net_returns\NASS\county_panel_empty, replace
	use processing\net_returns\NASS\county_panel_empty, clear
* clean
	erase processing\net_returns\NASS\county_list.dta
	erase processing\net_returns\NASS\year_list.dta
	
********************************************************************************
************DATA MANAGEMENT************
********************************************************************************
* 1 - county-level data: merge county-level data to blank panel
	use processing\net_returns\NASS\pasturerents_all, clear
	keep if agg_level_desc == "COUNTY"
	keep fips year pasture_nr
	merge 1:1 fips year using processing\net_returns\NASS\county_panel_empty
	assert _merge != 1 // check no unmatched from alldata
	gen pasture_nr_level = "county" if _merge == 3
	replace pasture_nr_level = "nodata" if _merge == 2
	drop _merge
	ren pasture_nr pasture_nr0
	compress
	save processing\net_returns\NASS\pasturerents, replace
/* 2 - other counties in the asd: 
	entries described as "OTHER (COMBINED) COUNTIES" are associated with all
	counties in an asd which are missing individual values*/
	* load "OTHER (COMBINED) COUNTIES"
	use processing\net_returns\NASS\pasturerents_all, clear
	keep if agg_level_desc == "OTHER COMB COUNTIES"
	keep state_fips_code asd_code year pasture_nr
	* merge to county dataset by state, year, and asd
	merge 1:m state_fips_code asd_code year using  processing\net_returns\NASS\pasturerents
	assert _merge != 1 // check no unmatched from "other comb counties" data (bc matching to full empty panel)
	* replace value with asd-level value if value is missing
	replace pasture_nr_level = "othercombcounties" if pasture_nr0 == . & pasture_nr != . & _merge == 3 & pasture_nr_level == "nodata"
	replace pasture_nr0 = pasture_nr if pasture_nr_level == "othercombcounties"
	drop pasture_nr _merge
	* save
	compress
	save processing\net_returns\NASS\pasturerents, replace
* 3 - merge in agricultural statistics district-level data
	* load agricultural district data
	use processing\net_returns\NASS\pasturerents_all, clear
	keep if agg_level_desc == "AGRICULTURAL DISTRICT"
	drop if asd_code == 98 // combined county code
	keep pasture_nr state_fips_code asd_code year
	* merge to working dataset
	merge 1:m state_fips_code asd_code year using processing\net_returns\NASS\pasturerents
	* replace value with asd-level value if value is missing
	replace pasture_nr_level = "asd" if pasture_nr0 == . & pasture_nr != . & _merge == 3 & pasture_nr_level == "nodata"
	replace pasture_nr0 = pasture_nr if pasture_nr_level == "asd"
	drop pasture_nr _merge
	* save
	compress
	save processing\net_returns\NASS\pasturerents, replace
* 4 - merge in state-level data
	* load state-level data
	use processing\net_returns\NASS\pasturerents_all, clear
	keep if agg_level_desc == "STATE"
	keep state_fips_code year pasture_nr
	* merge to working dataset
	merge 1:m state_fips_code year using processing\net_returns\NASS\pasturerents
	assert _merge != 1 // no unmatched from state-level data
	* replace value with state-level value if value is missing
	replace pasture_nr_level = "state" if pasture_nr0 == . & pasture_nr != . & _merge == 3 & pasture_nr_level == "nodata"
	replace pasture_nr0 = pasture_nr if pasture_nr_level == "state"
	drop pasture_nr _merge
	* save
	compress
	save processing\net_returns\NASS\pasturerents, replace
* 5 - multi-state region level data
	use processing\net_returns\NASS\pasturerents_all, clear
	keep if agg_level_desc == "REGION : MULTI-STATE"
	keep pasture_nr year region_desc 
	ren region_desc multistateregion_desc
	* merge to working dataset
	merge 1:m multistateregion_desc year using processing\net_returns\NASS\pasturerents
	assert _merge != 1 // check no unmathced from regional data
	replace pasture_nr_level = "multistate" if pasture_nr0 == . & pasture_nr != . & _merge == 3 & pasture_nr_level == "nodata"
	replace pasture_nr0 = pasture_nr if pasture_nr_level == "multistate"
	drop pasture_nr _merge
* manage
	order multistateregion_desc state_fips_code state* asd* fips county* year
	sort state_fips_code fips year
	ren pasture_nr0 pasture_nr
	replace fips = 46113 if fips == 46102 // other data do not reflect this change (2015, Shannon County (46113) was renamed Oglala Lakota (46102))
	label variable pasture_nr_level "Pasture rents (NASS) data level"
	* save
	compress
	save processing\net_returns\NASS\pasturerents, replace
	use processing\net_returns\NASS\pasturerents, clear

* inflation adjust (to 2010 dollars)
import excel raw_data\CPIinflationFactors.xlsx, sheet("Sheet1") firstrow clear
merge 1:m year using processing\net_returns\NASS\pasturerents
assert _merge != 2
drop if _merge == 1
drop _merge
replace pasture_nr = pasture_nr * Inflation2010Factor
label variable pasture_nr "2010USD pastureland rent/acre (NASS)"
drop Inflation2010Factor

* save
sort fips year
compress
save processing\net_returns\NASS\pasturerents, replace
use processing\net_returns\NASS\pasturerents, clear
=======
/* Programmer: Alexandra Thompson
Start Date: November 24, 2020
Objective: Import, manage NASS data
Variable of interest: survey-economics-expenses-rent: rent, cash, pastureland, expense, measured in $/acre
*/

********************************************************************************
************SETUP************
********************************************************************************
set more off
clear

* working dir
global workingdir "L:\Project-Land_Use"
cd $workingdir

********************************************************************************
************IMPORT, MANAGE, SAVE************
********************************************************************************
* import
import delimited raw_data\net_returns\NASS\qs.economics_20201117.txt, clear

* keep data items (variables) of interest
keep if group_desc == "EXPENSES"
compress
keep if short_desc == "RENT, CASH, PASTURELAND - EXPENSE, MEASURED IN $ / ACRE"
compress
drop if value == "(S)" // drop state rows (no data)
drop if agg_level_desc == "NATIONAL"

* keep variables of interest
keep source_desc short_desc agg_level_desc state_alpha state_name state_fips_code county_code county_name region_desc asd_desc asd_code year value

* destring value variable
destring value, replace

* manage variables
ren value pasture_nr
label variable pasture_nr "RENT, CASH, PASTURELAND - EXPENSE, MEASURED IN $ / ACRE"
drop short_desc
assert source_desc == "SURVEY"
drop source_desc

* generate fips code
gen statefips2 = state_fips_code * 1000
gen fips = statefips2 + county_code
drop county_code statefips2

/* tag values which correspond to "OTHER COMBINED COUNTIES."
	When county data is not disclosed, rates are reported by NASS Crop Reporting District. See Table 2 for a listing of counties in each crop reporting districts.
			https://www.sites.ext.vt.edu/newsletter-archive/nass/2014.pdf */
replace agg_level_desc = "OTHER COMB COUNTIES" if county_name == "OTHER (COMBINED) COUNTIES"
replace fips = . if agg_level_desc == "OTHER COMB COUNTIES"

* drop alaska and hawaii
drop if state_fips_code == 2 | state_fips_code == 15

* save
compress
save processing\net_returns\NASS\pasturerents_all, replace
use processing\net_returns\NASS\pasturerents_all, clear

********************************************************************************
************MAKE A BLANK COUNTY PANEL************
**************with asd (agricultural stastitics district)***********************
* county-asd dictionary
	import excel "raw_data\net_returns\NASS\Listing of Counties and Districts used by USDA-NASS.xlsx", sheet("data") firstrow clear
	* generate fips code
	gen statefips2 = state * 1000
	gen fips = statefips2 + county
	ren state state_fips_code
	* new name
	gen county_name2 = name + E
	* drop if not a county
	drop if county == 0 // state-level
	drop if county == 888 // asd-combined counties
	drop if county == 999 // asd-level
	* drop if historical assignment
	drop if historyflag == 2
	* drop vars
	drop name E county statefips2 historyflag
	ren asd asd_code
	* drop alaska and hawaii
	drop if state_fips_code == 2 | state_fips_code == 15
	* save
	compress
	save processing\net_returns\NASS\county_list, replace
* merge county-asd dictionary to data counties
	use processing\net_returns\NASS\pasturerents_all, clear
	keep if agg_level_desc == "COUNTY"
	keep county_name state_alpha state_name fips asd* state_fips_code
	duplicates drop
	merge 1:1 state_fips_code asd_code fips using processing\net_returns\NASS\county_list
	assert _merge != 1 // check no unmatched from data
	drop _merge
	* check no duplicate counties
	duplicates tag fips, gen(tag)
	assert tag == 0
	drop tag
	* save
	compress
	save processing\net_returns\NASS\county_list, replace
* merge in state names
	use processing\net_returns\NASS\county_list, clear
	keep state_fips_code state_alpha state_name
	duplicates drop
	drop if state_alpha == ""
	ren state_alpha state_alpha2
	ren state_name state_name2
	merge 1:m state_fips_code using processing\net_returns\NASS\county_list
	assert _merge != 1 // check no unmatched from state list
	drop _merge
	drop state_alpha state_name
	ren state_alpha2 state_alpha
	ren state_name2 state_name
	* a couple of manual edits
	replace state_alpha = "AK" if state_fips_code == 2
		replace state_name = "ALASKA" if state_alpha == "AK"
	replace state_alpha = "CT" if state_fips_code == 9
		replace state_name = "CONNECTICUT" if state_alpha == "CT"
	replace state_alpha = "ME" if state_fips_code == 23
		replace state_name = "MAINE" if state_alpha == "ME"
	replace state_alpha = "NH" if state_fips_code == 33
		replace state_name = "NEW HAMPSHIRE" if state_alpha == "NH"
	replace state_alpha = "RI" if state_fips_code == 44
		replace state_name = "RHODE ISLAND" if state_alpha == "RI"
	* save
	compress
	save processing\net_returns\NASS\county_list, replace
* merge in multi-state regions
	import excel "raw_data\net_returns\NASS\Farm Production Regions.xlsx", sheet("Sheet1") firstrow clear
	merge 1:m state_alpha using processing\net_returns\NASS\county_list
	assert _merge == 3
	drop _merge
	* save
	compress
	save processing\net_returns\NASS\county_list, replace
* year list
	use processing\net_returns\NASS\pasturerents_all, clear
	keep year
	duplicates drop
	sort year
	gen n1 = _n
	su
	local nyears = r(N)
	di `nyears'
	save processing\net_returns\NASS\year_list, replace
* merge to years
	use processing\net_returns\NASS\county_list, clear
	expand `nyears'
	bysort fips: gen n1 = _n
	merge m:1 n1 using processing\net_returns\NASS\year_list
	assert _merge == 3
	drop _merge n1
	compress
	order multistateregion_desc state_fips_code state* asd* fips county* year
	save processing\net_returns\NASS\county_panel_empty, replace
	use processing\net_returns\NASS\county_panel_empty, clear
* clean
	erase processing\net_returns\NASS\county_list.dta
	erase processing\net_returns\NASS\year_list.dta
	
********************************************************************************
************DATA MANAGEMENT************
********************************************************************************
* 1 - county-level data: merge county-level data to blank panel
	use processing\net_returns\NASS\pasturerents_all, clear
	keep if agg_level_desc == "COUNTY"
	keep fips year pasture_nr
	merge 1:1 fips year using processing\net_returns\NASS\county_panel_empty
	assert _merge != 1 // check no unmatched from alldata
	gen pasture_nr_level = "county" if _merge == 3
	replace pasture_nr_level = "nodata" if _merge == 2
	drop _merge
	ren pasture_nr pasture_nr0
	compress
	save processing\net_returns\NASS\pasturerents, replace
/* 2 - other counties in the asd: 
	entries described as "OTHER (COMBINED) COUNTIES" are associated with all
	counties in an asd which are missing individual values*/
	* load "OTHER (COMBINED) COUNTIES"
	use processing\net_returns\NASS\pasturerents_all, clear
	keep if agg_level_desc == "OTHER COMB COUNTIES"
	keep state_fips_code asd_code year pasture_nr
	* merge to county dataset by state, year, and asd
	merge 1:m state_fips_code asd_code year using  processing\net_returns\NASS\pasturerents
	assert _merge != 1 // check no unmatched from "other comb counties" data (bc matching to full empty panel)
	* replace value with asd-level value if value is missing
	replace pasture_nr_level = "othercombcounties" if pasture_nr0 == . & pasture_nr != . & _merge == 3 & pasture_nr_level == "nodata"
	replace pasture_nr0 = pasture_nr if pasture_nr_level == "othercombcounties"
	drop pasture_nr _merge
	* save
	compress
	save processing\net_returns\NASS\pasturerents, replace
* 3 - merge in agricultural statistics district-level data
	* load agricultural district data
	use processing\net_returns\NASS\pasturerents_all, clear
	keep if agg_level_desc == "AGRICULTURAL DISTRICT"
	drop if asd_code == 98 // combined county code
	keep pasture_nr state_fips_code asd_code year
	* merge to working dataset
	merge 1:m state_fips_code asd_code year using processing\net_returns\NASS\pasturerents
	* replace value with asd-level value if value is missing
	replace pasture_nr_level = "asd" if pasture_nr0 == . & pasture_nr != . & _merge == 3 & pasture_nr_level == "nodata"
	replace pasture_nr0 = pasture_nr if pasture_nr_level == "asd"
	drop pasture_nr _merge
	* save
	compress
	save processing\net_returns\NASS\pasturerents, replace
* 4 - merge in state-level data
	* load state-level data
	use processing\net_returns\NASS\pasturerents_all, clear
	keep if agg_level_desc == "STATE"
	keep state_fips_code year pasture_nr
	* merge to working dataset
	merge 1:m state_fips_code year using processing\net_returns\NASS\pasturerents
	assert _merge != 1 // no unmatched from state-level data
	* replace value with state-level value if value is missing
	replace pasture_nr_level = "state" if pasture_nr0 == . & pasture_nr != . & _merge == 3 & pasture_nr_level == "nodata"
	replace pasture_nr0 = pasture_nr if pasture_nr_level == "state"
	drop pasture_nr _merge
	* save
	compress
	save processing\net_returns\NASS\pasturerents, replace
* 5 - multi-state region level data
	use processing\net_returns\NASS\pasturerents_all, clear
	keep if agg_level_desc == "REGION : MULTI-STATE"
	keep pasture_nr year region_desc 
	ren region_desc multistateregion_desc
	* merge to working dataset
	merge 1:m multistateregion_desc year using processing\net_returns\NASS\pasturerents
	assert _merge != 1 // check no unmathced from regional data
	replace pasture_nr_level = "multistate" if pasture_nr0 == . & pasture_nr != . & _merge == 3 & pasture_nr_level == "nodata"
	replace pasture_nr0 = pasture_nr if pasture_nr_level == "multistate"
	drop pasture_nr _merge
* manage
	order multistateregion_desc state_fips_code state* asd* fips county* year
	sort state_fips_code fips year
	ren pasture_nr0 pasture_nr
	replace fips = 46113 if fips == 46102 // other data do not reflect this change (2015, Shannon County (46113) was renamed Oglala Lakota (46102))
	label variable pasture_nr_level "Pasture rents (NASS) data level"
	* save
	compress
	save processing\net_returns\NASS\pasturerents, replace
	use processing\net_returns\NASS\pasturerents, clear

* inflation adjust (to 2010 dollars)
import excel raw_data\CPIinflationFactors.xlsx, sheet("Sheet1") firstrow clear
merge 1:m year using processing\net_returns\NASS\pasturerents
assert _merge != 2
drop if _merge == 1
drop _merge
replace pasture_nr = pasture_nr * Inflation2010Factor
label variable pasture_nr "2010USD pastureland rent/acre (NASS)"
drop Inflation2010Factor

* save
sort fips year
compress
save processing\net_returns\NASS\pasturerents, replace
use processing\net_returns\NASS\pasturerents, clear
>>>>>>> a8c9ab564c2c9d9baefd847c71ba3cbd6038b467
