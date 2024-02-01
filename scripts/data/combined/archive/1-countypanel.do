<<<<<<< HEAD
/* Programmer: Alexandra Thompson
Start Date: October 5, 2020
Objective: Merge cleaned net_returns, NRI, and CRP county-level data to create panel
*/

********************************************************************************
************SETUP************
********************************************************************************
set more off
clear

* working dir
global workingdir "L:\Project-Land_Use"
cd $workingdir

**************************************COUNTY PANEL**************************************
* make a fips dictionary
* NRI vars
use processing\nri\nri15_county_panel, clear
keep state statefips stateName stateAbbrev fips county
duplicates drop
* CRP vars
merge 1:m fips using processing\CRP\CRPmerged
drop if CRPstate == "HAWAII" | CRPstate == "ALASKA" | CRPstate == "PUERTO RICO"
/*keep if _merge == 3
drop _merge*/
keep state statefips stateName stateAbbrev fips CRPcounty _merge
duplicates drop
ren CRPcounty countyName
drop state
replace countyName = proper(countyName)
drop _merge
* NASS vars
merge 1:m fips using processing\NASS\pasturerents
keep *state* *county* fips _merge
duplicates drop
replace statefips = state_fips_code if _merge != 1
replace stateAbbrev = state_alpha if _merge != 1
replace stateName = proper(state_name) if _merge != 1
replace countyName = county_name2 if _merge != 1
drop state_fips_code state_alpha state_name county_name county_name2 _merge
replace multistateregion_desc = "SOUTHEAST" if fips == 12025 & multistateregion_desc == ""
replace multistateregion_desc = "MOUNTAIN" if fips == 56047 & multistateregion_desc == ""
* NR vars (should only be DC)
merge 1:m fips using processing\net_returns\clean
drop year *_nr
duplicates drop
assert fips == 11001 if _merge == 2
replace countyName = "Washington" if fips == 11001
drop _merge
* save
order multistateregion_desc state* county* fips
label variable multistateregion_desc "NASS region"
ren multistateregion_desc USDA_region
save processing\combined\fips_dictionary, replace

********************************************************************************
************ASSESS NRI-NR MERGE ISSUES************
********************************************************************************
* determine which fips do merge, regardless of year
* resources: 
	* https://www.nrcs.usda.gov/wps/portal/nrcs/detail/national/technical/nra/nri/results/?cid=nrcs143_013710
	* https://www.ddorn.net/data/FIPS_County_Code_Changes.pdf
use processing\nri\nri15_county_panel, clear
keep fips state*
duplicates drop
merge 1:m fips using processing\net_returns\combined_returns_panel
keep fips _merge state*
duplicates drop
keep if _merge != 3
ta _merge
* notes
gen NRI_nr_mergenote = "999"
* unmatched from master
replace NRI_nr_mergenote = "NRI drop, no counterpart in nr" if fips == 56047 & _merge == 1 // "parts of 56029 & 56039 were used to create 56047" (but both 56029 & 56039 exist in both datasets)
replace NRI_nr_mergenote = "NRI leave, merged with adjacent counties (30031 and 30067) in 1997" if fips == 30113 & _merge == 1 // Yellowstone National Park territory (FIPS 30113) is merged into Gallantin (FIPS 30031) and Park (FIPS 30067) counties. Action: no adjustment of the source data required since all three territories map to the same CZ 34402.
replace NRI_nr_mergenote = "NRI leave, addressed in nr" if fips == 12025 & _merge == 1 // fips replaced in nr data to match
* unmatched from using
replace NRI_nr_mergenote = "nr replace 12086 with 12025 to match NRI" if fips == 12086  & _merge == 2 // 12025 (Dade County) renamed as 12086 (Miami-Dade), rev. to match NRI data
replace NRI_nr_mergenote = "nr drop, no counterpart in NRI" if fips == 8014 & _merge == 2 // Broomfield county created in 2001, doesn't exist in NRI data
replace NRI_nr_mergenote = "nr drop, no counterpart in NRI" if fips == 8031 & _merge == 2 // Denver county, small so not in NRI
replace NRI_nr_mergenote = "nr drop, no counterpart in NRI" if fips == 29510 & _merge == 2 // 29510 was collapsed into 29189 in NRI
replace NRI_nr_mergenote = "nr drop, DC not in NRI" if stateAbbrev == "DC"
replace NRI_nr_mergenote = "NASS drop, no counterpart in NRI" if fips == 32025 & _merge == 2 // tiny city in IA
replace NRI_nr_mergenote = "NASS drop, no counterpart in NRI" if fips == 36501 & _merge == 2 // new york city, ny
assert stateAbbrev != "VA" if _merge == 1 // check no potential Virginia counterparts in NRI data
replace NRI_nr_mergenote = "nr drop, Virginia, no counterpart in NRI" if stateAbbrev == "VA" & _merge == 2
* check no others
assert NRI_nr_mergenote != "999"
drop _merge
compress
drop state
save processing\combined\nri_nr_mergenotes, replace

********************************************************************************
************IMPLEMENT NR-NRI MERGE************
********************************************************************************
* load and make changes to nr data
use processing\net_returns\combined_returns_panel, clear
merge m:1 fips using processing\combined\nri_nr_mergenotes
drop if _merge == 2 // drop if notes only relevant to NRI data
ta NRI_nr_mergenote // list notes
* implement notes changes ONLY IF INCREASE MERGE RATE, NO DROPS
	replace fips = 12025 if fips == 12086
drop NRI_nr_mergenote _merge
/*gen data_NR = 1 // tag if NR data (all obs in this dataset)
	label variable data_NR "obs has NR data"*/
* NR components
local nrvars forest urban crop CRP pasture
foreach var in `nrvars' {
	gen data_NR`var' = `var'_nr != .
	label variable data_NR`var' "obs has NR`var' data"
	}
	ren data_NRCRP data_CRP
	label variable data_CRP "obs has CRP data (if mi, CRP acres is zero or very low)"
	label variable data_NRpasture "obs has NASS (pasture rents) data"

* merge to NRI data
merge 1:1 fips year using processing\nri\nri15_county_panel
drop state county
gen data_NRI = _merge != 1 // tag if NRI data (all obs other than MASTER ONLY)
	label variable data_NRI "obs has NRI data"
gen data_NRI6classes = acresk_6classes != 0 & acresk_6classes != .
	label variable data_NRI6classes "obs has NRI LU data in 1/6 classes of interest"
drop _merge
merge m:1 fips using processing\combined\nri_nr_mergenotes
* drop if notes no longer relevant
	drop if _merge == 2 & fips == 12086
drop _merge

* drop years not in nri data
	gen tag = year == 1982 ///
			| year == 1987 ///
			| year == 1992 ///
			| year == 1997 ///
			| year == 2002 ///
			| year == 2007 ///
			| year == 2012 ///
			| year == 2015
	drop if tag == 0
	drop tag

ta NRI_nr_mergenote // list notes
* save
compress
save processing\combined\nri_nr_county_panel, replace

********************************************************************************
************FINALIZE************
********************************************************************************
use processing\combined\nri_nr_county_panel, clear

* data availability vars
local datavars NRforest NRcrop NRurban NRI NRI6classes CRP NRpasture
foreach var in `datavars' {
	replace data_`var' = 0 if data_`var' == .
	gen datami_`var' = data_`var' == 0
	label variable datami_`var' "Obs is missing `var'"
	}
gen data_NRNRICRP = data_NRI6classes + data_CRP + data_NRforest + data_NRcrop + data_NRurban == 5
label variable data_NRNRICRP "obs has NRI(6classes), CRP, and 3 NR data components"

gen data_NRNRICRPNASS = data_NRI6classes + data_CRP + data_NRforest + data_NRcrop + data_NRurban + data_NRpasture == 6
label variable data_NRNRICRPNASS "obs has NRI(6classes), CRP, and 4 NR data components"

* merge to state/county dictionary
capture drop *state*
capture drop *county*
merge m:1 fips using processing\combined\fips_dictionary
drop if fips == 12086
assert _merge == 3
drop _merge

* finalize
label variable stateAbbrev "state abbreviation"
label variable countyName "county name"
label variable statefips "state fips code"
label variable fips "state+county fips code"

* drop if no NRI data
drop if data_NRI == 0
ta year

* save
drop *mergenote
order USDA_region state* *county* fips* year acresk* data* *_nr
sort fips year
compress
save processing\combined\countypanel, replace

********************************************************************************
************APPENDIX************
********************************************************************************
* compare CRP acreage values between NRCS and NRI data
use processing\combined\countypanel, clear
ren CRPland_acresk NRICRPacresk
ren CRPacresk NRCSCRPacresk
keep year fips NRCSCRPacresk NRICRPacresk
gen diff = NRICRPacresk- NRCSCRPacresk
collapse(sum) NRCSCRP* NRICRP*, by (year)
gen pcntdiff = (abs( NRICRPacresk- NRCSCRPacresk))/(( NRICRPacresk+ NRCSCRPacresk)/2)*100
su pcntdiff

* clean up
erase processing\combined\nri_nr_county_panel.dta
erase processing\combined\nri_nr_mergenotes.dta
=======
/* Programmer: Alexandra Thompson
Start Date: October 5, 2020
Objective: Merge cleaned net_returns, NRI, and CRP county-level data to create panel
*/

********************************************************************************
************SETUP************
********************************************************************************
set more off
clear

* working dir
global workingdir "L:\Project-Land_Use"
cd $workingdir

**************************************COUNTY PANEL**************************************
* make a fips dictionary
* NRI vars
use processing\nri\nri15_county_panel, clear
keep state statefips stateName stateAbbrev fips county
duplicates drop
* CRP vars
merge 1:m fips using processing\CRP\CRPmerged
drop if CRPstate == "HAWAII" | CRPstate == "ALASKA" | CRPstate == "PUERTO RICO"
/*keep if _merge == 3
drop _merge*/
keep state statefips stateName stateAbbrev fips CRPcounty _merge
duplicates drop
ren CRPcounty countyName
drop state
replace countyName = proper(countyName)
drop _merge
* NASS vars
merge 1:m fips using processing\NASS\pasturerents
keep *state* *county* fips _merge
duplicates drop
replace statefips = state_fips_code if _merge != 1
replace stateAbbrev = state_alpha if _merge != 1
replace stateName = proper(state_name) if _merge != 1
replace countyName = county_name2 if _merge != 1
drop state_fips_code state_alpha state_name county_name county_name2 _merge
replace multistateregion_desc = "SOUTHEAST" if fips == 12025 & multistateregion_desc == ""
replace multistateregion_desc = "MOUNTAIN" if fips == 56047 & multistateregion_desc == ""
* NR vars (should only be DC)
merge 1:m fips using processing\net_returns\clean
drop year *_nr
duplicates drop
assert fips == 11001 if _merge == 2
replace countyName = "Washington" if fips == 11001
drop _merge
* save
order multistateregion_desc state* county* fips
label variable multistateregion_desc "NASS region"
ren multistateregion_desc USDA_region
save processing\combined\fips_dictionary, replace

********************************************************************************
************ASSESS NRI-NR MERGE ISSUES************
********************************************************************************
* determine which fips do merge, regardless of year
* resources: 
	* https://www.nrcs.usda.gov/wps/portal/nrcs/detail/national/technical/nra/nri/results/?cid=nrcs143_013710
	* https://www.ddorn.net/data/FIPS_County_Code_Changes.pdf
use processing\nri\nri15_county_panel, clear
keep fips state*
duplicates drop
merge 1:m fips using processing\net_returns\combined_returns_panel
keep fips _merge state*
duplicates drop
keep if _merge != 3
ta _merge
* notes
gen NRI_nr_mergenote = "999"
* unmatched from master
replace NRI_nr_mergenote = "NRI drop, no counterpart in nr" if fips == 56047 & _merge == 1 // "parts of 56029 & 56039 were used to create 56047" (but both 56029 & 56039 exist in both datasets)
replace NRI_nr_mergenote = "NRI leave, merged with adjacent counties (30031 and 30067) in 1997" if fips == 30113 & _merge == 1 // Yellowstone National Park territory (FIPS 30113) is merged into Gallantin (FIPS 30031) and Park (FIPS 30067) counties. Action: no adjustment of the source data required since all three territories map to the same CZ 34402.
replace NRI_nr_mergenote = "NRI leave, addressed in nr" if fips == 12025 & _merge == 1 // fips replaced in nr data to match
* unmatched from using
replace NRI_nr_mergenote = "nr replace 12086 with 12025 to match NRI" if fips == 12086  & _merge == 2 // 12025 (Dade County) renamed as 12086 (Miami-Dade), rev. to match NRI data
replace NRI_nr_mergenote = "nr drop, no counterpart in NRI" if fips == 8014 & _merge == 2 // Broomfield county created in 2001, doesn't exist in NRI data
replace NRI_nr_mergenote = "nr drop, no counterpart in NRI" if fips == 8031 & _merge == 2 // Denver county, small so not in NRI
replace NRI_nr_mergenote = "nr drop, no counterpart in NRI" if fips == 29510 & _merge == 2 // 29510 was collapsed into 29189 in NRI
replace NRI_nr_mergenote = "nr drop, DC not in NRI" if stateAbbrev == "DC"
replace NRI_nr_mergenote = "NASS drop, no counterpart in NRI" if fips == 32025 & _merge == 2 // tiny city in IA
replace NRI_nr_mergenote = "NASS drop, no counterpart in NRI" if fips == 36501 & _merge == 2 // new york city, ny
assert stateAbbrev != "VA" if _merge == 1 // check no potential Virginia counterparts in NRI data
replace NRI_nr_mergenote = "nr drop, Virginia, no counterpart in NRI" if stateAbbrev == "VA" & _merge == 2
* check no others
assert NRI_nr_mergenote != "999"
drop _merge
compress
drop state
save processing\combined\nri_nr_mergenotes, replace

********************************************************************************
************IMPLEMENT NR-NRI MERGE************
********************************************************************************
* load and make changes to nr data
use processing\net_returns\combined_returns_panel, clear
merge m:1 fips using processing\combined\nri_nr_mergenotes
drop if _merge == 2 // drop if notes only relevant to NRI data
ta NRI_nr_mergenote // list notes
* implement notes changes ONLY IF INCREASE MERGE RATE, NO DROPS
	replace fips = 12025 if fips == 12086
drop NRI_nr_mergenote _merge
/*gen data_NR = 1 // tag if NR data (all obs in this dataset)
	label variable data_NR "obs has NR data"*/
* NR components
local nrvars forest urban crop CRP pasture
foreach var in `nrvars' {
	gen data_NR`var' = `var'_nr != .
	label variable data_NR`var' "obs has NR`var' data"
	}
	ren data_NRCRP data_CRP
	label variable data_CRP "obs has CRP data (if mi, CRP acres is zero or very low)"
	label variable data_NRpasture "obs has NASS (pasture rents) data"

* merge to NRI data
merge 1:1 fips year using processing\nri\nri15_county_panel
drop state county
gen data_NRI = _merge != 1 // tag if NRI data (all obs other than MASTER ONLY)
	label variable data_NRI "obs has NRI data"
gen data_NRI6classes = acresk_6classes != 0 & acresk_6classes != .
	label variable data_NRI6classes "obs has NRI LU data in 1/6 classes of interest"
drop _merge
merge m:1 fips using processing\combined\nri_nr_mergenotes
* drop if notes no longer relevant
	drop if _merge == 2 & fips == 12086
drop _merge

* drop years not in nri data
	gen tag = year == 1982 ///
			| year == 1987 ///
			| year == 1992 ///
			| year == 1997 ///
			| year == 2002 ///
			| year == 2007 ///
			| year == 2012 ///
			| year == 2015
	drop if tag == 0
	drop tag

ta NRI_nr_mergenote // list notes
* save
compress
save processing\combined\nri_nr_county_panel, replace

********************************************************************************
************FINALIZE************
********************************************************************************
use processing\combined\nri_nr_county_panel, clear

* data availability vars
local datavars NRforest NRcrop NRurban NRI NRI6classes CRP NRpasture
foreach var in `datavars' {
	replace data_`var' = 0 if data_`var' == .
	gen datami_`var' = data_`var' == 0
	label variable datami_`var' "Obs is missing `var'"
	}
gen data_NRNRICRP = data_NRI6classes + data_CRP + data_NRforest + data_NRcrop + data_NRurban == 5
label variable data_NRNRICRP "obs has NRI(6classes), CRP, and 3 NR data components"

gen data_NRNRICRPNASS = data_NRI6classes + data_CRP + data_NRforest + data_NRcrop + data_NRurban + data_NRpasture == 6
label variable data_NRNRICRPNASS "obs has NRI(6classes), CRP, and 4 NR data components"

* merge to state/county dictionary
capture drop *state*
capture drop *county*
merge m:1 fips using processing\combined\fips_dictionary
drop if fips == 12086
assert _merge == 3
drop _merge

* finalize
label variable stateAbbrev "state abbreviation"
label variable countyName "county name"
label variable statefips "state fips code"
label variable fips "state+county fips code"

* drop if no NRI data
drop if data_NRI == 0
ta year

* save
drop *mergenote
order USDA_region state* *county* fips* year acresk* data* *_nr
sort fips year
compress
save processing\combined\countypanel, replace

********************************************************************************
************APPENDIX************
********************************************************************************
* compare CRP acreage values between NRCS and NRI data
use processing\combined\countypanel, clear
ren CRPland_acresk NRICRPacresk
ren CRPacresk NRCSCRPacresk
keep year fips NRCSCRPacresk NRICRPacresk
gen diff = NRICRPacresk- NRCSCRPacresk
collapse(sum) NRCSCRP* NRICRP*, by (year)
gen pcntdiff = (abs( NRICRPacresk- NRCSCRPacresk))/(( NRICRPacresk+ NRCSCRPacresk)/2)*100
su pcntdiff

* clean up
erase processing\combined\nri_nr_county_panel.dta
erase processing\combined\nri_nr_mergenotes.dta
>>>>>>> a8c9ab564c2c9d9baefd847c71ba3cbd6038b467
