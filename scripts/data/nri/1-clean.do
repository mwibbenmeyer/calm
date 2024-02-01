/* Programmer: Alexandra Thompson
Start Date: October 5, 2020
Objective: transform raw NRI data into panel format
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
***********IMPORT, SAVE RAW DATASETS*************
********************************************************************************
* load dataset, save version with reduced n variables (reduced size from ~1,500 MB to ~ 200 MB)
import delimited raw_data\nri\2015\nri15_cty_082019.csv, clear
format %10.0g riad_id
keep state county fips lcc* xfact crp9* crp0* crp1* broad* landu* riad_id // keep vars of interest
order riad_id
* rename variables with years from 2 digits to 4 to facilitate reshape
local prefix lcc crp landu broad
foreach var of local prefix {
rename `var'1* `var'201*
capture rename `var'7* `var'197*
capture rename `var'8* `var'198*
rename `var'9* `var'199*
rename `var'0* `var'200*
}
gen acres = xfact * 100
gen acresk = acres / 1000
drop acres xfact
* keep only 48 contiguous US states
drop if state == 72 // PR
drop if state == 15 // HI

save processing\nri\nri15_reduced.dta, replace
* create a riad-state-county-fips crosswalk
keep riad_id state county fips
save processing\nri\nri15_pointcounty_crosswalk, replace

* import, save, trim classification table
import excel raw_data\nri\2015\nri15_layout_csv_082019.xlsx, sheet("Broad & Specific Land Use Codes") cellrange(A4:B16) firstrow clear
rename Code code
save processing\nri\broadLUcodes.dta, replace

********************************************************************************
*************INITIAL DATA EXPLORATION TO GET SENSE OF MEASUREMENT PREVALENCE****
********************************************************************************
* LANDU
use processing\nri\nri15_reduced.dta, clear
	* overall
	gen broad = broad1982 != . | broad1987 != . | broad1992 != . | broad1997 != . | broad2002 != . | broad2007 != . | broad2012 != . // tag if any broad data
	assert broad == 1 // check that all points have landu data at least 1 year

* LCC
use processing\nri\nri15_reduced.dta, clear
	* overall
	gen lcc = lcc1982 != "" | lcc1987 != "" | lcc1992 != "" | lcc1997 != "" | lcc2002 != "" | lcc2007 != "" | lcc2012 != "" // tag if any LCC data
	ta lcc // n points with any LCC data
	bysort lcc: egen sumlccacresk = sum(acresk) // calculcate area with and without LCC data
	ta sumlccacresk if lcc == 1 // total area with LCC data
	ta sumlccacresk if lcc == 0 // total area without LCC data
	* by county
	bysort fips: egen sumacresk = sum(acresk) // total fips area
	bysort lcc fips: egen sumlccfipsacresk = sum(acresk)
	keep lcc fips sumlccfipsacresk sumacresk
	duplicates drop
	gen pcntarealcc = sumlccfipsacresk/sumacresk * 100
	su pcntarealcc if lcc == 1 // percent of county area with LCC data
	hist pcntarealcc if lcc == 1, freq title(Percent of County Area with LCC Data) subtitle (In Any Year)
	window manage close graph

********************************************************************************
*************CREATE POINT-LEVEL PANEL OF LAND USE AND LAND COVER CLASS DATA*****
********************************************************************************
* manage landu variables
use processing\nri\nri15_reduced.dta, clear
keep state county fips riad_id acresk broad* lcc* // keep vars of interest

* merge with land use classification table, quality check that only classes intentionally omitted are omitted
foreach landuvar of varlist broad* {
rename `landuvar' code
replace code = 0 if code == . // in 1979, missing data are missing, not zero. replace with zero for consistency.
merge m:1 code using processing\nri\broadLUcodes.dta
assert _merge != 1 // check no unmatched from NRI data
drop if _merge == 2
drop _merge

* reclassify/aggregate
gen class = BroadLandCoverUse
replace class = "Cropland" if strpos(class, "cropland")
replace class = "Ruralland" if strpos(class, "rural") | strpos(class, "Rural")
replace class = "Waterland" if strpos(class, "water")
replace class = "Urbanland" if strpos(class, "Urban")
replace class = "Federalland" if class == "Federal land"
replace class = "Forestland" if class == "Forest land"
replace class = "CRPland" if strpos(class, "CRP")
drop BroadLandCoverUse code

rename class `landuvar'
compress
}

* collapse lu & lcc
collapse(sum) acresk, by (state county fips broad* lcc* riad_id)

* manage lcc variables
	* split lcc ("Land Capability Class & Subclass - source: current linked soil mapunit/component (The first character is the soil suitability rating for agriculture, between 1 and 8 - class 1 soil has few restrictions that limit its use, class 8 soil has limitations that nearly preclude its use for commercial crop production. The second character is the chief limitation of the soil: Blank = Not applicable, E = Erosion, W = Water, S = Shallow, drought, or stony, C = Climate))
	foreach var of varlist lcc* {
	replace `var' = "0" if `var' == "" // replace missing with zero
	gen `var'A = substr(`var', 1, 1) // remove letter, keep only number value
	drop `var'
	rename `var'A `var'
	destring `var', replace
	}
	* find max (i.e. nonmissing) lcc value for parcel
	*gen max
	gen which_max = "" 
	gen lccmax = 0 
	local xvars lcc1982 lcc1987 lcc1992 lcc1997 lcc2002 lcc2007 lcc2012 lcc2015
	foreach x of local xvars { 
		replace which_max = "`x'" if `x' > lccmax
		replace lccmax = `x' if `x' > lccmax
	}
	* gen non-zero min
	gen which_min = ""
	gen lccmin = 1
	local xvars lcc1982 lcc1987 lcc1992 lcc1997 lcc2002 lcc2007 lcc2012 lcc2015
	foreach x of local xvars { 
		replace which_min = "`x'" if `x' < lccmin
		replace lccmin = `x' if `x' > lccmin
	}
		replace lccmin = lccmax if lccmax == 0 // otherwise lccmin is falsely 1
	* check that non-zero min is equal to max
	assert lccmin == lccmax
	* replace value with max if value is zero
	local xvars lcc1982 lcc1987 lcc1992 lcc1997 lcc2002 lcc2007 lcc2012 lcc2015
	foreach x of local xvars { 
		replace `x' = lccmax if `x' == 0
		}
	drop which_max which_min lccmax lccmin
	* gen individual vars
	local xvars lcc1982 lcc1987 lcc1992 lcc1997 lcc2002 lcc2007 lcc2012 lcc2015
	foreach var of local xvars {
		forvalues x = 0/8 {
			gen lccL`x'_`var' = 0
			replace lccL`x'_`var' = acresk if `var' == `x'
			}
		*drop `var'
		}
drop lcc1982 lcc1987 lcc1992 lcc1997 lcc2002 lcc2007 lcc2012 lcc2015

* manage lu variables
	* generate variable for each land use (using 1997 classes)
	levelsof broad1997, local(levels)
	foreach l of local levels {
		gen lu_landu_`l' = .
		}
	* generate variables for total acresk for each landuse-year combo
	foreach landuvar of varlist broad* {
	levelsof `landuvar', local(levels)
		foreach l of local levels{
		gen _`l'_`landuvar' = 0
		replace _`l'_`landuvar' = acresk if `landuvar' == "`l'"
		}
	drop `landuvar'
	}
	drop lu*

* reshape
keep riad_id state county fips acresk *1982 *1987 *1992 *1997 *2002 *2007 *2012 *2015
reshape long _CRPland_broad _Cropland_broad _Forestland_broad _Ruralland_broad _Waterland_broad _Pastureland_broad _Rangeland_broad _Urbanland_broad _Federalland_broad lccL0_lcc lccL1_lcc lccL2_lcc lccL3_lcc lccL4_lcc lccL5_lcc lccL6_lcc lccL7_lcc lccL8_lcc, i(riad_id) j(year)
* keep only years with data ["1982, 1987, 1992, 1997, and annually from 2000 through 2017" (https://www.nrcs.usda.gov/wps/portal/nrcs/main/national/technical/nra/nri/)]
keep if year == 1982 | year == 1987 | year == 1992 | year == 1997 | year == 2002 | year == 2007 | year == 2012 | year == 2015
compress

ren lccL*_lcc lccL*_acresk
ren _*_broad *_acresk
ren lccL0_acresk lccNA_acresk

* CRP wasn't established until 1985. replace missing values prior to then with zero.
replace CRPland_acresk = 0 if year < 1985

* generate combined LCC (as in Lubowski 2006)
gen lccL12_acresk = lccL1_acresk + lccL2_acresk
gen lccL34_acresk = lccL3_acresk + lccL4_acresk
gen lccL56_acresk = lccL5_acresk + lccL6_acresk
gen lccL78_acresk = lccL7_acresk + lccL8_acresk

* merge to state-fips crosswalk
merge m:1 riad_id using processing\nri\nri15_pointcounty_crosswalk
assert _merge == 3
drop _merge

* generate state fips
tostring fips, gen(fipsstring)
gen statefips = substr(fipsstring, 1, 2)
replace statefips = substr(fipsstring, 1, 1) if length(fipsstring)==4
destring statefips, replace
merge m:1 statefips using processing\stateFips
assert statefips == 11 if _merge == 1
drop if _merge == 2
drop _merge
replace stateName = "District of Columbia" if statefips == 11
replace stateAbbrev = "DC" if statefips == 11
drop fipsstring 

* convert fips from num to string, with leading zeros
tostring fips, replace
gen zero = "0"
gen fipslength = strlen(fips)
gen fips2 = fips
//destring fips, replace
replace fips2 = zero + fips if fipslength == 4
drop zero fipslength fips
ren fips2 fips

* save
order state* fips* county riad* acresk year *land* lcc*
sort riad* year
compress
save processing\nri\nri15_point_panel, replace

********************************************************************************
*************CREATE COUNTY-LEVEL PANEL OF LAND USE AND LAND CAPABILITY CLASS DATA*****
********************************************************************************
use processing\nri\nri15_point_panel, clear
collapse(sum) *acresk, by (state* county fips* year)

	* % county area in each land use (using total area excluding federal, rural, water)
	gen acresk_6classes = CRPland_acresk+ Cropland_acresk+ Forestland_acresk+ Pastureland_acresk+ Rangeland_acresk+ Urbanland_acresk
		label variable acresk_6classes "total acresk in 6 lu classes of interest (CRP,crop,for,pasture,range,urb)"
	local vars CRPland Cropland Forestland Pastureland Rangeland Urbanland 
	foreach var of local vars {
	gen `var'_pcnt = `var'_acresk / acresk_6classes  * 100
	replace `var'_pcnt = 0 if `var'_pcnt == . & acresk_6classes  == 0
	label variable `var'_pcnt "landu acres / county acres in 6 classes of interest"
	compress
	}
	* % county area in each land use (including federal, rural, water)
	local vars CRPland Cropland Forestland Pastureland Rangeland Urbanland Federalland Waterland Ruralland
	foreach var of local vars {
	gen `var'_pcnt2 = `var'_acresk / acresk  * 100
	replace `var'_pcnt2 = 0 if `var'_pcnt2 == . & acresk  == 0
	label variable `var'_pcnt2 "landu acres / county acres"
	compress
	}
	* check percents add up to 100
	egen test = rowtotal(*_pcnt2) 
	assert test >= 99.9 & test <= 100.1
	drop test

	* % county area in each lcc (using total area with lcc data)
	foreach var of varlist lcc* {
	gen `var'_pcnt = `var' / acresk * 100
	label variable `var'_pcnt "LCC level acres / total county acres"
	}
	rename lcc*_acresk_pcnt lcc*_pcnt

compress
order state* fips* county acresk* year *land*
save processing\nri\nri15_county_panel, replace

********************************************************************************
*************TOTAL AREA CALCULATION**********************
********************************************************************************
use processing\nri\nri15_county_panel, clear
collapse(sum) acresk*, by(year)
* for qaqc, compare to results from adaptation of another programmer's code, "replicate_weilun_acres.do"

