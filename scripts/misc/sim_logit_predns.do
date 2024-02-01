clear

set obs 1000000

qui { 

gen id = _n
gen x = runiform()
*gen z = ibeta(2,10,1-x)
gen z = ibeta(5,100,1-x)

/* Generate "parcel size" variable */
gen rand = runiform()
gen size = cond(rand > 0.8, 100, 1)
drop rand

/* Uniform group size */
*gen group = round((_n + 49)/100)

/* Random group size */
forvalues i = 1/1000 {
	local random = runiform()
	local rnumbers = "`rnumbers' `random'"
}
local selection : list sort rnumbers
local selection = "0 `selection' 1.1"

gen groupi = _n/_N
egen group = cut(groupi), at(`selection')
bysort group: egen groupsize = sum(1)

/* Individual-level logit */ 
gen rand = runiform()
gen y = (rand < z)
*gen y = z
noi di "Mean realizations of y"
noi summ y
egen total_actual = sum(y*size)
summ total_actual

logit y, cluster(id)
predict prob, pr
noi di "Mean predicted probs from individual-level logit"
noi summ prob

egen total_hat = sum(prob*size)
summ total_hat
drop total_hat

/* Shares regression */
collapse (mean) y x groupsize (sum) size, by(group)
rename y s

noi summ s [aweight = groupsize]

gen y = ln(s) - ln(1-s)
reg y [aweight = groupsize]

gen prob = exp(_b[_cons])/(1 + exp(_b[_cons]))
noi di "Mean predicted probs logistic shares regression"
noi summ prob

egen total = sum(prob*size)
summ total
drop total prob

}


