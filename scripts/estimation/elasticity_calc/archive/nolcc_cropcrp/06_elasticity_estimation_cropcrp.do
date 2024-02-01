clear

cd L:/Project-Land_Use/


/*** GET INITIAL ACRES ***/
import delimited "processing\elasticity\initial_acres.csv"

sum initial_acres if year == 2015 & initial_use == "Crop"
local crop_acres = r(mean) 
sum initial_acres if year == 2015 & initial_use == "CRP"
local crp_acres = r(mean) 

matrix input acres_initial = (`crop_acres', `crp_acres')
matrix input acres = (`crop_acres', `crp_acres')
matrix list acres


/******************* ORIGINAL EQUILIBRIUM ***************************/
/*** GET PROBABILITY ESTIMATES ***/
clear
import excel "processing\elasticity\nolcc_cropcrp\dy_prob.xlsx", first cellrange(B1)

list
mkmat p1 p2, matrix(imp_prob)
matrix list imp_prob


/*** FIND THE EQUILIBRIUM ***/
/*First iteration*/

matrix acres_next = acres*imp_prob
matrix list acres_next

local diff = (acres_next[1,1] - acres[1,1])^2 + (acres_next[1,2] - acres[1,2])^2
di `diff'

/*Iterate until convergence*/
local iter = 1

while `diff' > 0.0000001 {

	local iter = `iter' + 1
	matrix acres = acres_next
	matrix acres_next = acres*imp_prob
	local diff = (acres_next[1,1] - acres[1,1])^2 + (acres_next[1,2] - acres[1,2])^2
	// di "Iteration `iter': diff = `diff'"

}

di `iter'
matrix list acres



/******************* NEW EQUILIBRIUM ***************************/
/*** GET PROBABILITY ESTIMATES ***/
foreach k in crop crp {
	clear
	import excel "processing\elasticity\nolcc_cropcrp\dy_prob_`k'10pct.xlsx", first cellrange(B1)

	list
	mkmat p1 p2, matrix(imp_prob_`k'10pct)
	matrix list imp_prob_`k'10pct


	/*** FIND THE EQUILIBRIUM ***/
	/*First iteration*/

	matrix acres_`k'10pct = acres_initial
	matrix acres_`k'10pct_next = acres_`k'10pct*imp_prob_`k'10pct
	matrix list acres_`k'10pct_next

	local diff = (acres_`k'10pct_next[1,1] - acres_`k'10pct[1,1])^2 + (acres_`k'10pct_next[1,2] - acres_`k'10pct[1,2])^2
	di `diff'

	/*Iterate until convergence*/
	local iter = 1

	while `diff' > 0.0000001 {

		local iter = `iter' + 1
		matrix acres_`k'10pct = acres_`k'10pct_next
		matrix acres_`k'10pct_next = acres_`k'10pct*imp_prob_`k'10pct
		local diff = (acres_`k'10pct_next[1,1] - acres_`k'10pct[1,1])^2 + (acres_`k'10pct_next[1,2] - acres_`k'10pct[1,2])^2
		// di "Iteration `iter': diff = `diff'"

	}

	di "`k': `iter' iterations"

}

matrix list acres
matrix list acres_crop10pct
matrix list acres_crp10pct


/******************* ELASTICITY ***************************/
di "crop_elasticity = " (acres_crop10pct[1,1] - acres[1,1])/acres[1,1]/0.1
di "crp_elasticity = " (acres_crp10pct[1,2] - acres[1,2])/acres[1,2]/0.1

di "crop_cross_elasticity = " (acres_crp10pct[1,1] - acres[1,1])/acres[1,1]/0.1
di "crp_cross_elasticity = " (acres_crop10pct[1,2] - acres[1,2])/acres[1,2]/0.1
