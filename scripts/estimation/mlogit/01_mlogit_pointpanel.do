***********************************************************
**                   Mlogit Model                        **
***********************************************************

/* INSPECT POINT-LEVEL DATA */ 

use "L:\Project-Land_Use\processing_output\pointpanel_estimation_unb.dta", clear

by riad_id lcc, sort: gen new_lccs = _n == 1
by riad_id: gen sum_lccs = sum(new_lccs)
by riad_id: egen num_lccs = max(sum_lccs)
sort riad_id year
drop new_lccs sum_lccs
br if num_lccs != 1

levelsof riad_id if num_lccs!=1, local(lcc_change)
local count_lcc_change: word count of `lcc_change'

capture drop id
by riad_id: gen id = _n == 1
tab id if id==1

di "Number of parcels where the lcc has changed: `count_lcc_change'"
di "Number of parcels in the dataset: `r(N)'"
di "Percentage of parcels with a lcc change: `count_lcc_change'/`r(N)'" //0.57%


/* PREPARE THE DATASET FOR THE MLOGIT MODEL */

* drop observations that have initial_use or final_use outside the five types
replace initial_use = "Other" if initial_use == "Pasture"
replace initial_use = "Other" if initial_use == "Range"
replace final_use = "Other" if final_use == "Pasture"
replace final_use = "Other" if final_use == "Range"

drop if initial_use != "Crop" & initial_use != "CRP" & initial_use != "Forest" & initial_use != "Urban" & initial_use != "Other"
drop if initial_use != "Crop" & initial_use != "CRP" & initial_use != "Forest" & initial_use != "Urban" & initial_use != "Other"
drop if final_use != "Crop" & final_use != "CRP" & final_use != "Forest" & final_use != "Urban" & final_use != "Other"
drop if final_use != "Crop" & final_use != "CRP" & final_use != "Forest" & final_use != "Urban" & final_use != "Other"

by riad_id: egen count_id = count(riad_id)
tab count_id
drop if count_id != 7

* summarize the new sample
levelsof riad_id if num_lccs!=1, local(lcc_change)
local count_lcc_change: word count of `lcc_change'

capture drop id
by riad_id: gen id = _n == 1
tab id if id==1

di "Number of parcels where the lcc has changed: `count_lcc_change'"
di "Number of parcels in the dataset: `r(N)'"
di "Percentage of parcels with a lcc change: `count_lcc_change'/`r(N)'" //0.7%

* generate the conversion dummy variable
capture drop conv
gen conv = .
replace conv = 1 if initial_use != final_use & initial_use != "" & final_use != ""
replace conv = 0 if initial_use == final_use & initial_use != "" & final_use != ""
tab conv // 4.9% converted

* generate the dependent variable
* for conv ==1, create a variable of the type of conversions
capture drop y_conv
gen y_conv = ""
replace y_conv = initial_use + "to" + final_use if initial_use != final_use
replace y_conv = "NoConversions" if initial_use == final_use

rename fips fips_s
encode fips_s, gen(fips) 

tempfile POINT
save `POINT'

* recover the net returns for initial land uses
// capture drop nr_initial
// gen nr_initial = .
// by riad_id: replace nr_initial = -nr[_n-1]

* prepare the county-level net returns data
preserve

use "L:\Project-Land_Use\processing\combined\ddc_data_urbancal_st", clear
keep fips year lcc CRP_nr forest_nr urban_nr other_nr crop_nr
duplicates drop fips year lcc, force
tempfile NR
save `NR'

restore

* merge with the county-level net returns
merge m:1 fips year lcc using `NR'


/* RUN THE MLOGIT MODEL */

* run the regressions
encode y_conv, gen(y_conv2)
encode lcc, gen(lcc2)
mlogit y_conv2 c.CRP_nr#c.dCRP c.crop_nr#c.dCrop c.forest_nr#c.dForest c.other_nr#c.dOther c.urban_nr#c.dUrban i.lcc2, robust cluster(fips) noconstant
