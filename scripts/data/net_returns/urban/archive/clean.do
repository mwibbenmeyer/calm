/* 
Author: Alexandra Thompson (RFF)
Date: March 6, 2021
Purpose: Load, prep urban net returns data
*/ 
************* RFF Urban Net Returns data *************
global workingdir "L:\Project-Land_Use"
cd $workingdir
import delimited processing\net_returns\countylevel_urban_net_returns.csv, clear
ren county_fips fips
replace fips = 46113 if fips == 46102 // replace new fips with old fips, since old fips is used elsewhere (https://www.ddorn.net/data/FIPS_County_Code_Changes.pdf)
ren net_returns urban_nr
replace urban_nr = "." if urban_nr == "NA"
destring urban_nr, replace
drop if urban_nr == .
label variable urban_nr "2010USD annualized net return/acre [RFF]"
replace year = 2002 if year == 2000
replace fips = 12025 if fips == 12086
compress
duplicates drop
save processing\net_returns\urban\countylevel_urban_net_returns, replace
