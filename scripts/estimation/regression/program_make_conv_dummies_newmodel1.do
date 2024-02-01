capture program drop make_conv_dummies
program make_conv_dummies


/******************************************************************************/
/* ADD DUMMY VARIABLES 					   						 			*/

* dummy1: conversion from/to a land use type
levelsof final_use, local(K)
foreach k in `K' {
	gen initial_`k' = (initial_use=="`k'")
}

levelsof final_use, local(J)
foreach j in `J' {
	gen final_`j' = (final_use=="`j'")
}

foreach i_use in `K' {
    gen d`i_use' = .
	replace d`i_use' = final_`i_use' - initial_`i_use' if final_use != "Other" /*Dummy for switches out of initial use*/
	replace d`i_use' = - initial_`i_use' if final_use == "Other"
}

* dummy2: specific conversions
foreach k in `K' {
	foreach j in `J' {
		if "`k'" != "`j'" {
			gen `k'to`j' = (initial_use=="`k'") & (final_use=="`j'") /*Dummy for specific conversion type*/
			label variable `k'to`j' "Conversion from `k' to `j'"
		}
	}
}

end
