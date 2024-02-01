/* 
Author: Alexandra Thompson (RFF)
Date: September 21, 2020
Purpose: Initial Data Clean of Net Returns Data
Input data source(s): 
	Dave Lewis and Chris Mihiar (Oregon State)
	RFF (urban net returns)
*/ 

* working dir
global workingdir "L:\Project-Land_Use"
cd $workingdir

* load returns data
import delimited raw_data\net_returns\lewis-mihiar\landuse_net_returns.csv, clear

* destring
local valuevars crop_nr forest_nr urban_nr
foreach v of local valuevars {
	replace `v' = "." if `v' == "NA"
	destring `v', replace
	}

* label
label variable forest_nr "2010USD annualized net return/acre of bare forestland [L&M]"
label variable urban_nr "2010USD annualized net return/acre derived from price of recently dev. land[L&M]"
label variable crop_nr "2010USD annualized net return/acre net income deriving from crop production[L&M]"

* drop urban new returns from lewis-mihiar
drop urban_nr

* save
compress
save processing\net_returns\temp_lewismihiar_netreturns, replace
