cd "F:/Projects/land-use/"

*log using "C:/Users/Matt/Downloads/ddc_test.log", replace

global beta_annual = 0.9

use "processing/net_returns/combined_returns_panel.dta", clear
tostring fips, replace
tempfile tmp
save "`tmp'"

use "processing_output/pointpanel_estimation_unb.dta", clear
merge m:1 fips year using "`tmp'", keepusing(crop_nr forest_nr urban_nr)


/******************************************************************************/
/* Get observations - Keep observations that were cropland in 2002 and never  */
/* transitioned to anything other than urban 								  */

sort fips riad_id year
drop if year < 2002

*Drop observations that were not cropland in 2002
gen x = (year == 2002 & initial_use == "Crop")
bysort fips riad_id: egen keep = sum(x)
keep if keep == 1
drop x keep

*Drop observations that transitioned to something other than urban or crop
gen x = (final_use == "Crop" | final_use == "Urban")
bysort fips riad_id: egen keep = min(x)
keep if keep == 1
drop x keep

drop if initial_use == "Urban"

tostring riad_id, gen(riad_str)
gen parcel_id = fips + riad_str
destring parcel_id, gen(parcelnum)

tsset parcelnum year

encode stateAbbrev, gen(statenum)
encode fips, gen(fipsnum)
encode lcc, gen(lccnum)

/******************************************************************************/
/* Create dependent variable												  */

gen urban = (final_use == "Urban")
gen crop = (final_use == "Crop")

/*Calculate transition probability as state-year mean*/

preserve 

collapse (mean) urban [aw = acresk] , by(fipsnum year)
rename urban prU
gen prC = 1 - prU

tempfile tmp
save "`tmp'"

restore

preserve

collapse (mean) urban urban_nr crop_nr (first) fipsnum statenum lccnum [aw = acresk] , by(fips year lcc)
merge m:1 fipsnum year using "`tmp'", keepusing(prU  prC) 

gen x = fips + lcc
encode x, gen(id)
drop x
tsset id year

gen prUnext = prU[_n + 1] if id == id[_n+1]


/*Create dependent variable*/

*Calculate relevant discount rate for each obs. 
gen year_diff = year[_n + 1] - year if id == id[_n+1] 
gen beta = $beta_annual
replace beta = beta^year_diff

*Create dependent variable
gen y = ln(prU/prC) + beta*ln(prUnext)
gen logodds = ln(prU/prC)

/*Run second-stage regression*/
gen uNRxLCC = urban_nr*lccnum
gen cNRxLCC = crop_nr*lccnum

reg y urban_nr crop_nr uNRxLCC cNRxLCC lccnum, r

/******************************************************************************/
/* Calculate choice probabilities											  */

restore

gen profit = urban*(_b[urban_nr]*urban_nr + _b[uNRxLCC]*urban_nr*lccnum) + ///
				crop*(_b[crop_nr]*crop_nr + _b[cNRxLCC]*crop_nr*lccnum) + _b[_cons] + lccnum*_b[lccnum] 
				

collapse (mean) profit crop crop_nr urban_nr lccnum, by(fips year lcc urban)

*Get next period net returns for urban 				
preserve 

tostring year, gen(yrstr)
collapse (mean) urban_nr crop_nr (first) yrstr, by(fips year)
sort fips year
gen id = fips + yrstr
gen uNRnext = urban_nr[_n + 1] if fips == fips[_n + 1]
encode fips, gen(fipsnum)

tempfile tmp
save "`tmp'"

restore

merge m:1 fipsnum year using "`tmp'", keepusing(uNRnext) 




log close
