* manage variables
	* rename year variables with nonnumerical character
		gen n = _n
		tostring n, replace
		foreach var of varlist * {
			replace `var' = "." if `var' == "--"
			replace `var' = "y" + `var' if n == "1"
			}
		drop n
	* make first row variable names
		foreach var of varlist * {
			qui ren `var' `= `var'[1]'
		}
	drop if ySTATE == "ySTATE"
	drop if ySTATE == ""
	drop if strpos(ySTATE, "1/ Payments based on")
	drop if strpos(ySTATE, "2/ Shannon County, South Dakota")
	drop if strpos(ySTATE, "1/ Fiscal year ends September 30")
	ren ySTATE state
	ren yCOUNTY county
	ren yFIPS fips
	foreach var of varlist y* {
		qui destring `var', replace
		}
	* fix one county with spelling inconsistency
	replace county = "STE. GENEVIEVE" if county == "STE GENEVIEVE"
* reshape
	reshape long y, i(fips) j(year)
	compress
	destring fips, replace
