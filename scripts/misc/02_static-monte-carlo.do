clear all

// SET THE NUMBER OF OBSERVATIONS
set obs 100000


/* SET PARAMETERS */
global group_size = 500 // Make this sufficiently large to eliminate zero shares
global eta_AB = -1 
global eta_BA = -2
global R_A = 0.1 /*Initial mean return to A*/
global R_B = 0 /*Initial mean return to B*/
global theta_A = 1 /*Initial return to A*/
global theta_B = 0.75 /*Initial return to B*/
global gamma = 0.5772156649 


gen id = _n
gen group = round((id + $group_size /2 - 1)/$group_size)

gen final_use1 = "A"
gen final_use2 = "B"
reshape long final_use, i(id) j(choice)
drop choice

set seed 20036

gen R = 0.2*rnormal()
bysort group final_use: replace R = R[1]
replace R = R + $R_A if final_use == "A"
replace R = R + $R_B if final_use == "B"

gen initial_use1 = "A"
gen initial_use2 = "B"
reshape long initial_use, i(id final_use) j(choice)
drop choice
order initial_use, before(final_use)

// Define parameters
gen theta = .
gen eta = .
replace theta = cond(final_use == "A", $theta_A, $theta_B) 
replace eta = cond(initial_use == "A" & final_use == "B", $eta_AB, ///
					cond(initial_use == "B" & final_use == "A", $eta_BA, 0))

// Create type 1 extreme value error
gen eps = ln(-ln(runiform())) + $gamma					
					
// Calculate utility, which includes error term		
gen vj = theta*R + eta + eps

// Create new variable, defined as the utility of alternative option 
gen vj_A_temp = vj if final_use == "A"
gen vj_B_temp = vj if final_use == "B"
bysort id initial_use: egen vj_A = mean(vj_A_temp)
bysort id initial_use: egen vj_B = mean(vj_B_temp)

gen vk =.
replace vk = vj_A if final_use == "B"
replace vk = vj_B if final_use == "A"

drop vj_A_temp vj_B_temp vj_A vj_B

// Generate land use choice: use j is chosen if vj > vk
sort id initial_use final_use

gen choice = 0
replace choice = 1 if vj > vk				
					
								



// ==================== estimate static shares model ======================== //
collapse (mean) choice R, by(initial_use final_use group)
sort group initial_use final_use

gen choice_outside = 1 - choice if final_use ~= initial_use 

gen R_outside_temp = R if final_use == initial_use
bysort group initial_use: egen R_outside = mean(R_outside_temp)

keep if initial_use ~= final_use

gen lnshare = ln(choice)
gen lnshare_outside = ln(choice_outside)
gen ln_share_diff = lnshare - lnshare_outside

reg ln_share_diff R R_outside if initial_use == "A"
reg ln_share_diff R R_outside if initial_use == "B"