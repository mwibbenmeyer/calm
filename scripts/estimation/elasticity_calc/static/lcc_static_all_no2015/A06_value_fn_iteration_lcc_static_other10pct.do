clear

cd L:/Project-Land_Use/


/*** GET ESTIMATION RESULTS ***/
import delimited "results\initial_estimation\regs_2022-03\regs_2022-03-29_unweighted_lcc_st_no2015.txt", varnames(2) 
keep if variables != "" & y != ""
rename (variables y) (regressor coef)

gen coef_n = subinstr(coef, "*", "", .)
replace coef_n = subinstr(coef_n, ",", "", .)
replace coef_n = subinstr(coef_n, "NO", "0", .)
replace coef_n = subinstr(coef_n, "YES", "1", .)
destring coef_n, replace force
replace regressor=lower(regressor)

/* Extract coefficients and save as locals */
* Unweighted results are used below

global lccs = "lcc1 lcc2 lcc3 lcc4"
global land_use = "urban crp crop forest other"

* Conversion costs
foreach lcc in $lccs {
	foreach k in $land_use {
		foreach j in $land_use {
			if "`k'" != "`j'" {
			
				di "`k'to`j'"
 				sum coef_n if regressor == "c.`lcc'#c.`k'to`j'"
 				local eta_`k'to`j'_`lcc' = r(mean)
				
			}
		}
	}
} // Note that there is no converstions 1) from urban to any, 2) from forest to CRP, and 3) from CRP to urban

* Net return coefficients
foreach lcc in $lccs {
	foreach k in $land_use {

		sum coef_n if regressor == "c.`k'x`lcc'#c.d`k'"
		local theta_`k'_`lcc' = r(mean)

	}
}

macro list 

/*** GET COUNTY RENTS ***/
clear 
use "processing\combined\ddc_data_urbancal_st", clear
keep fips year initial_use lcc CRP_nr forest_nr urban_nr other_nr crop_nr
keep if CRP_nr !=. & forest_nr !=. & urban_nr !=. & other_nr !=. & crop_nr !=. 

/* Calculate probabilities for 2015 only */
keep if year==2015
duplicates drop fips year initial_use lcc, force

/*** INCREASE OTHERS' RENTS BY 10% ***/
replace other_nr = other_nr*1.1

/* Dummy rents */

rename CRP_nr crp_nr

gen P_1 = urban_nr /* Urban */
gen P_2 = crp_nr /* CRP */
gen P_3 = crop_nr /* Crop */
gen P_4 = forest_nr /* Forest */
gen P_5 = other_nr /* Other */

gen k = .
replace k = 1 if initial_use == "Urban"
replace k = 2 if initial_use == "CRP"
replace k = 3 if initial_use == "Crop"
replace k = 4 if initial_use == "Forest"
replace k = 5 if initial_use == "Other"

sort fips lcc k

/* Recode lcc variables to be consistent with locals */
encode lcc, gen(lcc_num)
label drop lcc_num
recode lcc_num (1 = 0) (2 = 1) (3 = 2) (4 = 3) (5 = 4), 

tempfile returns
save `returns'

/***************************************************************/
/*** SET UP DATA SET FOR ITERATION ***/

/* Data set for value function iteration */

gen v = 0
gen vnext = v

/* Set coefficients and constants */
gen theta_1 = .
gen theta_2 = .
gen theta_3 = .
gen theta_4 = .
gen theta_5 = .

foreach num in 1 2 3 4 {
	replace theta_1 = `theta_urban_lcc`num'' if lcc_num == `num'
	replace theta_2 = `theta_crp_lcc`num'' if lcc_num == `num'
	replace theta_3 = `theta_crop_lcc`num'' if lcc_num == `num'
	replace theta_4 = `theta_forest_lcc`num'' if lcc_num == `num'
	replace theta_5 = `theta_other_lcc`num'' if lcc_num == `num'
}

gen beta = 0
gen gamma = 0.577216

gen eta_1 = .
gen eta_2 = .
gen eta_3 = .
gen eta_4 = .
gen eta_5 = .

foreach num in 1 2 3 4 {

	/*Conversions to Urban*/
	replace eta_1 = 0 if k == 1
// 	replace eta_1 = `eta_crptourban_lcc`num'' if k == 2 & lcc_num == `num'
	replace eta_1 = `eta_croptourban_lcc`num'' if k == 3 & lcc_num == `num'
	replace eta_1 = `eta_foresttourban_lcc`num'' if k == 4 & lcc_num == `num'
	replace eta_1 = `eta_othertourban_lcc`num'' if k == 5 & lcc_num == `num'

	/*Conversions to CRP*/
// 	replace eta_2 = `eta_urbantocrp_lcc`num'' if k == 1 & lcc_num == `num'
	replace eta_2 = 0 if k == 2
	replace eta_2 = `eta_croptocrp_lcc`num'' if k == 3 & lcc_num == `num'
// 	replace eta_2 = `eta_foresttocrp_lcc`num'' if k == 4 & lcc_num == `num'
	replace eta_2 = `eta_othertocrp_lcc`num'' if k == 5 & lcc_num == `num'

	/*Conversions to Crop*/
// 	replace eta_3 = `eta_urbantocrop_lcc`num'' if k == 1 & lcc_num == `num'
	replace eta_3 = `eta_crptocrop_lcc`num'' if k == 2 & lcc_num == `num'
	replace eta_3 = 0 if k == 3
	replace eta_3 = `eta_foresttocrop_lcc`num'' if k == 4 & lcc_num == `num'
	replace eta_3 = `eta_othertocrop_lcc`num'' if k == 5 & lcc_num == `num'

	/*Conversions to Forest*/
// 	replace eta_4 = `eta_urbantoforest_lcc`num'' if k == 1 & lcc_num == `num'
	replace eta_4 = `eta_crptoforest_lcc`num'' if k == 2 & lcc_num == `num'
	replace eta_4 = `eta_croptoforest_lcc`num'' if k == 3 & lcc_num == `num'
	replace eta_4 = 0 if k == 4
	replace eta_4 = `eta_othertoforest_lcc`num'' if k == 5 & lcc_num == `num'

	/*Conversions to Other*/
// 	replace eta_5 = `eta_urbantoother_lcc`num'' if k == 1 & lcc_num == `num'
	replace eta_5 = `eta_crptoother_lcc`num'' if k == 2 & lcc_num == `num'
	replace eta_5 = `eta_croptoother_lcc`num'' if k == 3 & lcc_num == `num'
	replace eta_5 = `eta_foresttoother_lcc`num'' if k == 4 & lcc_num == `num'
	replace eta_5 = 0 if k == 5

} // note that the lines commented out are the non-existing conversions

/***************************************************************/
/*** VALUE FUNCTION ITERATION ***/

/*First iteration*/

gen vcond1 = .
gen vcond2 = .
gen vcond3 = .
gen vcond4 = .
gen vcond5 = .

gen diff = .
drop if lcc_num == 0

forvalues j = 1/5 {	
	
	by fips lcc: replace vcond`j' = eta_`j' + theta_`j'*P_`j' + beta*v[`j']
		
}

/*Aggregation depends on the number of feasible conversions*/		
replace vnext = ln(exp(vcond1)) + gamma if initial_use == "Urban"
replace vnext = ln(exp(vcond2) + exp(vcond3) + exp(vcond4) + exp(vcond5)) + gamma if initial_use == "CRP"
replace vnext = ln(exp(vcond1) + exp(vcond2) + exp(vcond3) + exp(vcond4) + exp(vcond5)) + gamma if initial_use == "Crop"
replace vnext = ln(exp(vcond1) + exp(vcond3) + exp(vcond4) + exp(vcond5)) + gamma if initial_use == "Forest"
replace vnext = ln(exp(vcond1) + exp(vcond2) + exp(vcond3) + exp(vcond4) + exp(vcond5)) + gamma if initial_use == "Other"

replace diff = abs(v - vnext)
sum diff
local maxdiff = `r(max)'


/*Iterate until convergence*/
local iter = 1

while `maxdiff' > 0.00000001 {

	local iter = `iter' + 1
	replace v = vnext

	forvalues j = 1/5 {	
		
		by fips lcc: replace vcond`j' = eta_`j' + theta_`j'*P_`j' + beta*v[`j']
			
	}
			
	/*Aggregation depends on the number of feasible conversions*/		
	replace vnext = ln(exp(vcond1)) + gamma if initial_use == "Urban"
	replace vnext = ln(exp(vcond2) + exp(vcond3) + exp(vcond4) + exp(vcond5)) + gamma if initial_use == "CRP"
	replace vnext = ln(exp(vcond1) + exp(vcond2) + exp(vcond3) + exp(vcond4) + exp(vcond5)) + gamma if initial_use == "Crop"
	replace vnext = ln(exp(vcond1) + exp(vcond3) + exp(vcond4) + exp(vcond5)) + gamma if initial_use == "Forest"
	replace vnext = ln(exp(vcond1) + exp(vcond2) + exp(vcond3) + exp(vcond4) + exp(vcond5)) + gamma if initial_use == "Other"
	
	replace diff = abs(v - vnext)
	sum diff
	local maxdiff = `r(max)'
	
}

di "`iter'"

/*** CALCULATE IMPLIED PROBABILITIES ***/

gen num = .

gen p1 = .
gen p2 = .
gen p3 = .
gen p4 = .
gen p5 = .

gen denom = .
replace denom = exp(vcond1) if initial_use == "Urban"
replace denom = exp(vcond2) + exp(vcond3) + exp(vcond4) + exp(vcond5) if initial_use == "CRP"
replace denom = exp(vcond1) + exp(vcond2) + exp(vcond3) + exp(vcond4) + exp(vcond5) if initial_use == "Crop"
replace denom = exp(vcond1) + exp(vcond3) + exp(vcond4) + exp(vcond5) if initial_use == "Forest"
replace denom = exp(vcond1) + exp(vcond2) + exp(vcond3) + exp(vcond4) + exp(vcond5) if initial_use == "Other"

ds, has(type numeric)
recast double `r(varlist)'

forvalues j = 1/5 {
		replace num = exp(vcond`j')
		replace p`j' = num/denom
	}
	
/*** VISUALIZE THE PROBABILITIES ***/
sum p1, detail
sum p2, detail
sum p3, detail
sum p4, detail
sum p5, detail

tab initial_use, sum(p1)
tab initial_use, sum(p2)
tab initial_use, sum(p3)
tab initial_use, sum(p4)
tab initial_use, sum(p5)

tabstat p1 p2 p3 p4 p5, stat(mean) by(initial_use) nototal save

putexcel set processing\elasticity\lcc_static_all_no2015\st_prob_lcc_other10pct, replace
putexcel A2 = "Urban"
putexcel A3 = "CRP"
putexcel A4 = "Crop"
putexcel A5 = "Forest"
putexcel A6 = "Other"
putexcel B1 = "p1"
putexcel C1 = "p2"
putexcel D1 = "p3"
putexcel E1 = "p4"
putexcel F1 = "p5"
putexcel B2 = matrix(r(Stat5))
putexcel B3 = matrix(r(Stat1))
putexcel B4 = matrix(r(Stat2))
putexcel B5 = matrix(r(Stat3))
putexcel B6 = matrix(r(Stat4))

foreach num in 1 2 3 4 {

tabstat p1 p2 p3 p4 p5 if lcc_num == `num', stat(mean) by(initial_use) nototal save

putexcel set processing\elasticity\lcc_static_all_no2015\st_prob_lcc`num'_other10pct, replace
putexcel A2 = "Urban"
putexcel A3 = "CRP"
putexcel A4 = "Crop"
putexcel A5 = "Forest"
putexcel A6 = "Other"
putexcel B1 = "p1"
putexcel C1 = "p2"
putexcel D1 = "p3"
putexcel E1 = "p4"
putexcel F1 = "p5"
putexcel B2 = matrix(r(Stat5))
putexcel B3 = matrix(r(Stat1))
putexcel B4 = matrix(r(Stat2))
putexcel B5 = matrix(r(Stat3))
putexcel B6 = matrix(r(Stat4))

}

save "processing\elasticity\lcc_static_all_no2015\implied_prob_lcc_static_other10pct.dta", replace
