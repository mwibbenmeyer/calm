/* Programmer: Alexandra Thompson
Start Date: October 15, 2020
Objective: Generate summary tables
*/

********************************************************************************
************SETUP************
********************************************************************************
set more off
clear

* working dir
global workingdir "M:\GitRepos\land-use"
cd $workingdir

**** pasture rents levels ****
use processing\combined\countypanel, clear
net install http://www.stata.com/users/kcrow/tab2xl, replace
tab2xl year pasture_nr_level using results\initial_descriptives\combined\pasturerents_datalevel_crosstab, col(1) row(1)


*******mean percent county area & mean net return value*****************************
* by year
	use processing\combined\countypanel, clear
	* summarize
		* su landu
		su *land_pcnt
		* su LCC
		su lcc*pcnt
		* su nr
		su *_nr
	* create table
	collapse(mean) *pcnt* *_nr, by(year)
	sort year 
	order year Cropland* CRPland* Forestland* Pastureland* Rangeland* Urbanland* lccNA* lccL* *nr
	xpose, varname clear
	order _varname
	export excel using results\initial_descriptives\combined\sumtable_countymeans.xlsx, replace
	
* weighed by measured county area, by year
ssc inst _gwtmean
	use processing\combined\countypanel, clear
	* summarize 
		* su landu, weighed by measured land use area
		local vars Crop Forest Pasture Range CRP Urban
		levelsof year, local(years)
		foreach var in `vars' {
			foreach y of local years {
			di `y'
			su `var'land_pcnt [w=acresk_6classes] if year == `y'
			}
		}
		* su LCC, weighed by measured LCC area 
		foreach var of varlist lcc*pcnt {
		levelsof year, local(years)
			foreach y of local years {
			di `y'
			su `var' [w=acresk_6classes] if year == `y'
			}
		}
		* net returns, weighed by county acreage in each land use
		levelsof year, local(years)
		foreach y of local years {
		di `y'
		su crop_nr [w=Cropland_acresk] if year == `y'
		su forest_nr [w=Forestland_acresk] if year == `y'
		su urban_nr [w=Urbanland_acresk] if year == `y'
		su CRP_nr [w=CRPacresk] if year == `y'
		su pasture_nr [w=Pastureland_acresk] if year == `y'
		su range_nr [w=Rangeland_acresk] if year == `y'
		}
	* generate weighted means
		* landu
		local vars Crop Forest Pasture Range CRP Urban
		foreach var in `vars' {
		egen wtmean_`var'land_pcnt = wtmean(`var'land_pcnt), weight(acresk_6classes) by(year)
		}
		* LCC
		foreach var of varlist lcc*pcnt {
		egen wtmean_`var' = wtmean(`var'), weight(acresk_6classes) by(year)
		}
		* net returns
		egen wtmean_crop_nr = wtmean(crop_nr), weight(acresk_6classes) by(year)
		egen wtmean_forest_nr = wtmean(forest_nr), weight(acresk_6classes) by(year)
		egen wtmean_urban_nr = wtmean(urban_nr), weight(acresk_6classes) by(year)
		egen wtmean_CRP_nr = wtmean(CRP_nr), weight(acresk_6classes) by(year)
		egen wtmean_pasture_nr = wtmean(pasture_nr), weight(acresk_6classes) by(year)
		egen wtmean_range_nr = wtmean(range_nr), weight(acresk_6classes) by(year)
	* create table
	keep year wtmean*
	duplicates drop
	rename wtmean_* *
	sort year 
	order year Cropland* CRPland* Forestland* Pastureland* Rangeland* Urbanland* lccNA* lccL* *nr
	xpose, varname clear
	order _varname
	export excel using results\initial_descriptives\combined\sumtable_countymeans_weighted.xlsx, replace

*******LAND USE SUMMARY TABLE*****************************
use processing\combined\countypanel, clear
collapse(sum) *_acresk, by (year)
export excel using results\initial_descriptives\NRI\landu_lcc_totalarea.xlsx, firstrow(variables) replace

*******VARIABLES*****************************
* county
use processing\combined\countypanel, clear
describe, replace
drop vallab format
export excel using results\initial_descriptives\combined\countypanel_variables.xlsx, firstrow(variables) replace
* point-level
use processing\combined\pointpanel, clear
describe, replace
drop vallab format
export excel using results\initial_descriptives\combined\pointpanel_variables.xlsx, firstrow(variables) replace



