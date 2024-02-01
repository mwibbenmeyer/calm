use "processing\combined\ddc_data_urbancal_dy", clear

capture drop total_weighted_ccp
egen total_weighted_ccp=sum(weighted_ccp), by(initial_use year fips lcc)

br fips lcc initial_use final_use year weighted_ccp total_weighted_ccp //not always =1
br fips lcc initial_use final_use year weighted_ccp total_weighted_ccp if year == 2007 //makes more sense
br fips lcc initial_use final_use year weighted_ccp total_weighted_ccp if year == 2015 //more problematic


use "processing\combined\full_combined_returns", clear

capture drop total_weighted_ccp
egen total_weighted_ccp=sum(weighted_ccp), by(initial_use year fips lcc)
br fips lcc initial_use final_use year weighted_ccp total_weighted_ccp if year == 2015 //looks okay - it's the adjustment that's making the difference
