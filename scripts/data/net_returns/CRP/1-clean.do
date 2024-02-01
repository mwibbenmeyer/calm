/* Programmer: Alexandra Thompson
Start Date: November 6, 2020
Objective: Import, clean Conservation Reserve Program (CRP) data
*/

/* Modification: October 6, 2021 by Qinrui Xiahou
1. corrected the Excel sheet input for rent and average variables
2. modified the cell range to match the actual data (to reflect that rent is missing for 1986)
3. replaced the calculation of averages in the raw data*/

********************************************************************************
************SETUP************
********************************************************************************
set more off
clear

* working dir
global workingdir "L:\Project-Land_Use"
cd $workingdir

* IMPORT, MANAGE, SAVE
* acres
	import excel raw_data\net_returns\crp\HistoryCounty86-19.xlsx, sheet("ACRES") cellrange(A2:AK3079) firstrow allstring clear
	do scripts\data\net_returns\CRP\1-clean_sub1.do
	ren y CRPacres
	gen CRPacresk = CRPacres/1000
	drop CRPacres
	label variable CRPacresk "Thousand acres in CRP (USDA County Stats)"
	compress
	save processing\net_returns\CRP\acres, replace

* rent
	import excel raw_data\net_returns\crp\HistoryCounty86-19.xlsx, sheet("RENT") cellrange(A2:AJ3079) firstrow allstring clear
	do scripts\data\net_returns\CRP\1-clean_sub1.do
	ren y CRPrent
	label variable CRPrent "CRP Contract-based FY rental payments (not actuals) (USDA County Stats)"
	compress
	save processing\net_returns\CRP\rent, replace

* average
	import excel raw_data\net_returns\crp\HistoryCounty86-19.xlsx, sheet("AVERAGE") cellrange(A2:AK3079) firstrow allstring clear
	do scripts\data\net_returns\CRP\1-clean_sub1.do
	ren y CRP_nr
	label variable CRP_nr "avg per-CRPacre contract-based FY rent payments (not actuals) (USDA County Stats)"
	save processing\net_returns\CRP\avg, replace

* MERGE
use processing\net_returns\CRP\acres, clear
merge 1:1 fips year state county using processing\net_returns\CRP\rent
assert _merge != 2 // check no unmatched from rent data
assert year == 1986 if _merge == 1 // check that only unmatched acres data is from 1986
drop _merge
merge 1:1 fips year state county using processing\net_returns\CRP\avg
assert _merge == 3
drop _merge

* FINALIZE
ren county CRPcounty
ren state CRPstate

* save
compress
save processing\net_returns\CRP\CRPmerged, replace
use processing\net_returns\CRP\CRPmerged, clear

* inflation adjustment
import excel raw_data\CPIinflationFactors.xlsx, sheet("Sheet1") firstrow clear
merge 1:m year using processing\net_returns\CRP\CRPmerged
drop if _merge == 1
assert _merge != 2
drop _merge
replace CRPrent = CRPrent * Inflation2010Factor
	label variable CRPrent "2010USD CRP Contract-based FY rent payments (not actuals) (USDA)"
replace CRP_nr = CRP_nr * Inflation2010Factor
	label variable CRP_nr "2010USD avg per-CRPacre contract-based FY rent payments (not actuals) (USDA)"
drop Inflation2010Factor

* save
compress
save processing\net_returns\CRP\CRPmerged, replace
use processing\net_returns\CRP\CRPmerged, clear
