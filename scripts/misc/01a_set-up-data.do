/* SET UP DATA SET */

/* Set up of problem:

Let there be two land uses (A and B). In each period, landowner chooses whether
to keep land in its current use or switch to the other use. Returns are a function
of the land's use.

*/
clear all

// SET THE NUMBER OF OBSERVATIONS
set obs 200000

// Use this small sample to diagnose problems
//set obs 20000

/* SET PARAMETERS */
global group_size = 2000 // Make this sufficiently large to eliminate zero shares
//global beta = 0.975
global beta = 0.9 // This assumption matches Scott (2013)
global eta_AB = -1 
global eta_BA = -2
//global R_A = 0.4 /*Initial mean return to A*/
//global R_B = 0.25 /*Initial mean return to B*/
global R_A = 0.1 /*Initial mean return to A*/
global R_B = 0 /*Initial mean return to B*/
global theta_A = 1 /*Initial return to A*/
global theta_B = 0.75 /*Initial return to B*/

global gamma = 0.5772156649 // Euler's gamma for computing value function iteration

gen id = _n
gen group = round((id + $group_size /2 - 1)/$group_size)

gen final_use1 = "A"
gen final_use2 = "B"
reshape long final_use, i(id) j(choice)
drop choice

set seed 20036

//gen xi = rnormal()
// Set xi to be negative to scale utility to be near zero. Otherwise the value
//function iteration blows up to large values
//gen xi = -0.8 + 0.05*rnormal()

gen R = 0.2*rnormal()
bysort group final_use: replace R = R[1]
replace R = R + $R_A if final_use == "A"
replace R = R + $R_B if final_use == "B"

gen initial_use1 = "A"
gen initial_use2 = "B"
reshape long initial_use, i(id final_use) j(choice)
drop choice
order initial_use, before(final_use)

*Generate land use specific errors for each observation

/* SOLVE DYNAMIC PROBLEM TO FIND V(K_A) and V(K_B) */

gen V = 0
gen Vnext = 0
gen theta = 0
//order xi, last
gen eta = 0
gen diff = 0 

// I took these out of the while loop since they don't change
replace theta = cond(final_use == "A", $theta_A, $theta_B) 
replace eta = cond(initial_use == "A" & final_use == "B", $eta_AB, ///
					cond(initial_use == "B" & final_use == "A", $eta_BA, 0))

local diff = 9999
di `diff'

// This loop computes the value function based on Equation (17) in Araujo et al.
qui while `diff' >= 1e-9 {
   
	gen exp_term = exp(theta*R + eta + $beta * V)		
	bysort id initial_use: egen sum_exp_term = sum(exp_term)
	//replace Vnext = log(sum_exp_term) + $gamma // Equation (17) in Araujo et al.
	replace Vnext = log(sum_exp_term) // 6-22: test to see the effect of removing gamma
	drop exp_term sum_exp_term
	
	replace diff = Vnext - V
	summ diff
	local diff = abs(`r(max)')
	
	replace V = Vnext	

	}

	
sort id final_use	
	
// Since V is computed by initial land use, we need to assign it based on final land use
gen V_A_temp = V if initial_use == "A"
gen V_B_temp = V if initial_use == "B"

bysort id: egen V_A = mean(V_A)
bysort id: egen V_B = mean(V_B)

gen Vprime =.
replace Vprime = V_A if final_use == "A"
replace Vprime = V_B if final_use == "B"

drop Vnext

/* CALCULATE CHOICE PROBABILITIES THAT WOULD BE OBSERVED GIVEN V(K_A) and V(K_B) */



// Create type 1 extreme value error
gen eps = ln(-ln(runiform())) + $gamma		

// Calculate utility, which includes error term		
//gen vj = theta*R + eta + xi + $beta * Vprime + eps
gen vj = theta*R + eta + $beta * Vprime + eps


// Create new variable, defined as the utility of alternative option, vk
gen vj_A_temp = vj if final_use == "A"
gen vj_B_temp = vj if final_use == "B"
bysort id initial_use: egen vj_A = mean(vj_A_temp)
bysort id initial_use: egen vj_B = mean(vj_B_temp)

gen vk =.
replace vk = vj_A if final_use == "B"
replace vk = vj_B if final_use == "A"
drop vj_A_temp vj_B_temp vj_A vj_B


// Generate land use choice: land use j is chosen if vj > vk
sort id initial_use final_use
gen choice = 0
replace choice = 1 if vj > vk	

/* SUMMARIZE CHOICES */
tab initial_use choice if final_use == "A"
tab initial_use choice if final_use == "B"

/* KEEP OBSERVED DATA */
//keep id group initial_use final_use choice R
keep id group initial_use final_use choice R eps vj vk Vprime V

save "C:\Users\bleard\Dropbox\Sloan\Land Use\Dynamic model\Monte Carlo estimation\simulated_data.dta", replace