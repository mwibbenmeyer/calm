clear

/* SET UP DATA SET */

gen id = _n

gen final_use1 = "A"
gen final_use2 = "B"
reshape long final_use, i(id) j(choice)
drop choice

gen xi = rnormal()

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
order xi, last
gen eta = 0
gen diff = 0
gen eps = 0 

local diff = 9999

qui while `diff' >= 1e-9 {
    
	replace eps = 0.5*rnormal() /*Epsilon is ijkt specific*/
	replace theta = cond(final_use == "A", `theta_A',`theta_B')
	replace eta = cond(initial_use == "A" & final_use == "B",`eta_AB', ///
					cond(initial_use == "B" & final_use == "A",`eta_BA',0))
	bysort id final_use: egen maxV = max(V)
	replace Vnext = theta + eta + xi + `beta'*maxV
	drop maxV
		
	replace diff = Vnext - V
	summ diff
	local diff = abs(`r(max)')
	
	replace V = Vnext	

	}

bysort id final_use: egen Vmax = max(V) /*Vmax is the expected max value of the next period chosen land use*/


/* CALCULATE CHOICE PROBABILITIES THAT WOULD BE OBSERVED GIVEN V(K_A) and V(K_B) */

drop V Vnext eps
gen vj = theta + eta + xi + `beta'*Vmax 
gen eps =  log(-log(runiform()))

* Keep one initial use for each observation - this is because th
drop if (id <= 0.5*(_N/4) & initial_use == "B") | (id > 0.5*(_N/4) & initial_use == "A") 

bysort id: egen denom = sum(exp(vj + eps))
gen prob = exp(vj + eps) / denom

gen randt = runiform() if final_use == "A"
bysort id: egen rand = max(randt)
drop randt

* Generate land use choice
gen choice = cond(rand <= prob,1,0) if final_use == "A"
bysort id: replace choice = 1 - choice[1] if _n == 2

/* SUMMARIZE CHOICES */

*tab initial_use choice if final_use == "A"

