********************************************************************************
************SETUP************
********************************************************************************
set more off
clear

* working dir
global workingdir "L:\Project-Land_Use"
cd $workingdir

use processing\combined\pointpanel.dta, clear
/*
keep riad_id
duplicates drop
sample 1
sample 1
merge 1:m riad_id using processing\combined\pointpanel.dta
keep if _merge == 3
drop _merge
compress
save processing\combined\pointpanel_sample, replace
use processing\combined\pointpanel_sample, clear
*/
keep riad fipsstr year *acresk stateAbbrev countyName *_nr
ren fipsstr fips
sort riad year
drop CRPacresk

* gen initial use (previous)
su year
gen minyear = r(min)
sort riad year
local vars CRP Forest Pasture Range Urban Crop Water Rural Federal
foreach var in `vars' {
gen initial_`var'land_acresk = `var'land_acresk[_n-1]
replace initial_`var'land_acresk = . if year == minyear
}
gen initial_use = "999"
local vars CRP Forest Pasture Range Urban Crop Water Rural Federal
foreach var in `vars' {
	replace initial_use = "`var'" if initial_`var'land_acresk == point_acresk
	}
replace initial_use = "." if initial_use == "999" & year == minyear
assert initial_use != "999"
/*drop if year == minyear
drop minyear*/

* gen final use (current)
gen final_use = "999"
local vars CRP Forest Pasture Range Urban Crop Water Rural Federal
foreach var in `vars' {
	replace final_use = "`var'" if `var'land_acresk == point_acresk
}
assert final_use != "999"

label variable initial_use "previous (yeart-5) land use, missing in 1982"
label variable final_use "current land use"

* gen LCC var
gen lcc = "999"
	replace lcc = "1_2" if lccL12_acresk == point_acresk
	replace lcc = "3_4" if lccL34_acresk == point_acresk
	replace lcc = "5_6" if lccL56_acresk == point_acresk
	replace lcc = "7_8" if lccL78_acresk == point_acresk
* check that only zeros are if lu is water, rural, urban, federal
gen tag = final_use == "Water" | final_use == "Federal" | final_use == "Urban" | final_use == "Rural"
assert tag == 1 if lcc == "999"
replace lcc = "0" if lcc == "999"
drop tag
label variable lcc "Land Capability Class"

* save intermediate
compress
save processing\combined\pointpanel_temp, replace

* make rent variable, based on final land use
keep fips year *nr
duplicates drop
expand 9
bysort fips year: gen n = _n
gen final_use = "999"
gen nr = 999
	replace final_use = "CRP" if n == 1
		replace nr = CRP_nr if final_use == "CRP"
	replace final_use = "Crop" if n == 2
		replace nr = crop_nr if final_use == "Crop" 
	replace final_use = "Forest" if n == 3
		replace nr = forest_nr if final_use == "Forest"
	replace final_use = "Urban" if n == 4
		replace nr = urban_nr if final_use == "Urban"
	replace final_use = "Range" if n == 5
		replace nr = range_nr if final_use == "Range"
	replace final_use = "Pasture" if n == 6
		replace nr = pasture_nr if final_use == "Pasture"
	replace final_use = "Federal" if n == 7
		replace nr = . if final_use == "Federal"
	replace final_use = "Rural" if n == 8
		replace nr = . if final_use == "Rural"
	replace final_use = "Water" if n == 9
		replace nr = . if final_use == "Water"
assert final_use != "999"
assert nr != 999
label variable nr "Net returns to final (current) land use in county-year (2010USD)"
keep fips year final_use nr
compress
save processing\combined\nr_temp, replace
use processing\combined\pointpanel_temp, clear
merge m:1 fips year final_use using processing\combined\nr_temp
assert _merge != 1 // check no unmatched from [unbalanced]
drop if _merge == 2
drop _merge
compress

* save point-level
ren point_acresk acresk
keep riad_id year initial_use final_use lcc acresk fips countyName stateAbbrev nr
order stateAbbrev countyName fips riad_id year acresk initial_use final_use
compress
save processing_output\pointpanel_estimation_unb, replace
erase processing\combined\pointpanel_temp.dta
	
* prep for balancing by making lists of all variables
	* lcc list
	use processing_output\pointpanel_estimation_unb, clear
	keep lcc
	duplicates drop
	sort lcc
	gen n = _n // 5
	su n
	gen maxn = r(max)
	assert maxn == 5
	drop maxn
	save processing\combined\temp_lcc, replace
	* initial use list
	use processing_output\pointpanel_estimation_unb, clear
	keep initial_use
	duplicates drop
	sort initial_use
	drop if initial_use == "."
	gen n = _n // 9
	su n
	gen maxn = r(max)
	assert maxn == 9
	drop maxn
	save processing\combined\temp_initial, replace
	* final use list
	use processing_output\pointpanel_estimation_unb, clear
	keep final_use
	duplicates drop
	sort final_use
	drop if final_use == "."
	gen n = _n // 9
	su n
	gen maxn = r(max)
	assert maxn == 9
	drop maxn
	save processing\combined\temp_final, replace

* make county-level dataset
	* save net returns for merging after balancing
	use processing_output\pointpanel_estimation_unb, clear
	keep fips year final_use nr
	duplicates drop
	save processing\combined\temp_nr, replace
	* collapse by parcels/area
	use processing_output\pointpanel_estimation_unb, clear
	gen parcels = 1
	collapse(sum) parcels acresk, by (stateAbbrev countyName fips year initial_use final_use lcc)
	* var management
	label variable parcels "n parcels in county with these attributes (initial LU, final LU, lcc)"
	label variable acresk "thousand acres in county with these attributes (initial LU, final LU, lcc)"
	sort fips year
	* save unbalanced county-level
	order stateAbbrev countyName fips year parcels acresk initial_use final_use
	compress
	sort fips year final_* lcc*
	save processing\combined\countypanel_estimation_unb, replace
	* balance
		* lcc
		use processing\combined\countypanel_estimation_unb, clear
		keep stateAbbrev countyName fips year
		duplicates drop
		expand 5
		bysort fips year: gen n = _n
		merge m:1 n using processing\combined\temp_lcc
		assert _merge == 3
		drop _merge n
		* initial use
		expand 9
		bysort fips year lcc: gen n = _n
		merge m:1 n using processing\combined\temp_initial
		assert _merge == 3
		drop _merge n
		* final use
		expand 9
		bysort fips year lcc initial_use: gen n = _n
		merge m:1 n  using processing\combined\temp_final
		assert _merge == 3
		drop _merge n
		* merge to data
		merge 1:1 fips year lcc initial_use final_use using processing\combined\countypanel_estimation_unb
		assert year == 1982 if _merge == 2
		replace acresk = 0 if _merge == 1
		replace parcels = 0 if _merge == 1
		drop _merge
	* merge net returns
	merge m:1 fips year final_use using processing\combined\temp_nr
	assert _merge != 2
	drop _merge
	* save balanced county-level
	order stateAbbrev countyName fips year parcels acresk initial_use final_use
	compress
	sort fips year final_* lcc* initial*
	save processing_output\countypanel_estimation_bal, replace
	use processing_output\countypanel_estimation_bal, clear
	
* CLEANUP
erase processing\combined\nr_temp.dta
erase processing\combined\temp_nr.dta
erase processing\combined\temp_final.dta
erase processing\combined\temp_initial.dta
erase processing\combined\temp_lcc.dta

