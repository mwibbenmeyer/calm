* this script creates a BALANCED 1% PARCEL SAMPLE of the point panel estimation dataset, which is too big to be balanced with all units
********************************************************************************
************SETUP************
********************************************************************************
set more off
clear

* working dir
global workingdir "M:\GitRepos\land-use"
cd $workingdir
********************************************************************************
* MAKE AN EMPTY PANEL from the balanced county panel
use processing_output\countypanel_estimation_bal, clear
keep countyName stateAbbrev fips year initial_use final_use lcc
save processing\combined\emptycountybaltemp, replace

* MAKE NR DATASET from county panel
use processing_output\countypanel_estimation_bal, clear
keep fips year final_use nr
duplicates drop
save processing\combined\nr_temp2, replace

* MAKE A SAMPLE OF POINTS
use processing\pointpanel_estimation_unb, clear
keep riad_id fips
duplicates drop
**
sample 0.01
**

* MERGE TO EMPTY PANEL
merge m:m fips using processing\combined\emptycountybaltemp
assert _merge != 1
keep if _merge == 3
drop _merge

* MERGE TO PARCEL DATA
merge 1:1 riad_id year initial_use final_use lcc using processing\pointpanel_estimation_unb
drop if _merge == 2
replace acresk = 0 if _merge == 1
drop nr _merge

* MERGE TO NR DATA
merge m:1 fips final_use year using processing\combined\nr_temp2
drop if _merge == 2
drop _merge

* SAVE
compress
order riad_id stateAbbrev countyName fips year acresk initial_use final_use
sort riad_id year final_* lcc* initial*
save processing_output\pointpanel_estimation_bal_sample, replace
use processing_output\pointpanel_estimation_bal_sample, clear

* CLEANUP
erase processing\combined\emptycountybaltemp.dta
erase processing\combined\nr_temp2.dta
