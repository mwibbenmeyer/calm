# The RFF CALM model
This repository contains code for the RFF CALM model, a model for simulating land use change and resulting carbon outcomes under alternative land use rent scenarios. The model's carbon module is decribed in detail [here](https://media.rff.org/documents/WP_23-39.pdf).
## System requirements
CALM was built on Windows and uses R (v. 4.2.0) and Stata (v. 17). The model is run from R; however, in each time period, the executes a Stata .do file using the [RStata library](https://cran.r-project.org/web/packages/RStata/index.html). Therefore, the model must be run on a machine with both R and Stata installed.

## Data download
Data required to run the CALM model can be downloaded HERE. Data include:

1) `coefficient_estimates.est` - A Stata .est file that contains coefficient estimates resulting from estimating the econometric land use model underlying CALM's land use module.
2) `sim_df.csv` - A .csv file that containing land use transitions by land capability class and county. This data set defines the structure of the data that is read into the model and updated in each simulation period.
3) `returns.csv` - A .csv file containing estimated 2012 land use returns, by county, for the four primary land use types (cropland, forest, urban, and other)
4) `crop_level_returns.csv` - A .csv containing crop prices, by county, for major crops used in calculating crop returns. This data set is needed in order to adjust individual crop prices when endogenizing crop returns to changes in overall aggregate cropland.
5) `forest_return_inputs/.` - A folder containing several data sets used in endogenizing forest returns to changes in overall aggregate forestland.
6) `tamm_regions.xlsx` - A data set assigning states to Timber Assessment Market Model regions.
7) `carbon_models/.` - A folder containing ecoregion specific .rds carbon model files that describe the evolution of hardwood and softwood timber stocks, by age class, on federal and non-federal lands.

## Running the model
CALM can be run using the script `01_run_calm.R` (included within the `scripts/run_calm/` folder) based on the following steps:

1) **Set up RStata** - For the library RStata to successfully execute Stata commands from R, it needs the path to the Stata executable file (e.g. `StataMP-64.exe`) on your local machine. Locate the path, then modify these lines (within `01_run_calm.R`) to your correct path:
  ```\#May need to modify this based on location of Stata executable on local machine
options("RStata.StataPath" = "\"C:\\Program Files\\Stata17\\StataMP-64\"")
options("RStata.StataVersion" = 17)
```
2) **Change input and output paths** - The following lines set the directories within which the CALM input data (downloaded HERE) live, and to which resulting CALM outputs will be saved. These lines should be modified according to users' needs: 
```
input_path <- "processing/calm_inputs/"
output_path <- "processing/simulation/"
```
3) **Parameterize the model** - The CALM model is run using the following function:
```
sim_output <- run_sim(returns = returns.df, # Returns data frame. This can be modified using function in 01c_fn_modify_returns to explore alternative returns scenarios.
                      ints = 10, # Number of 5-year time intervals over which to run simulation
                      scenario_name = "status_quo" #Scenario name, used for naming results folder, and tagging results
                      )
```
The `ints` parameter controls the number of 5-year time intervals, beginning in 2012, over which the model will run. For example, to run the model over a 50-year period, set `ints = 10`.

The `scenario_name` parameter defines a tag that is used in saving results. This is useful when running the model under several alternative scenarios, as results from each scenario will be saved in the output folder under separate folders, named based on this parameter.

The `returns` parameter specifies a data frame that includes county-specific returns for each of the four primary land use types. By default, the returns parameter takes as input estimated land use returns in 2012. To explore alternative returns scenarios, these returns can be modified using the function `mod_returns()`, which is defined in the script `scripts/run_calm/functions/01c_fn_modify_returns.R`. The `mod_returns` function returns a data set of modified returns, and takes the following as inputs:
```
mod_returns <- function(data, crop_fn = identity,
                        forest_fn = identity,
                        other_fn = identity,
                        urban_fn = identity,
                        crop_counties = "all.fips",
                        forest_counties = "all.fips",
                        other_counties = "all.fips",
                        urban_counties = "all.fips"
)
```
First, the parameter data defines the set of returns to be modified. This should usually be set to `returns.df`. Each "fn" argument defines a function that will be used to modify corresponding returns. For example, you can increase initial crop returns by 10%, or add $10 to crop returns. By default, returns are unmodified, but they can be modified by setting the parameter equal to some desired function. Each "counties" argument defines a list of counties to which the function will be applied. By default, this is set to all counties, and "fn" arguments are applied to all counties. However, users can also apply functions to specific counties, e.g. all counties in some state or region. 

For example, to run CALM under a scenario in which crop returns in Iowa are initially 10% higher than at baseline, run:
```
library(tidycensus)

iowa_counties <- paste0("19", fips_codes[which(fips_codes$state_code == "19"), ]$county_code)

iowa <- run_sim(returns = mod_returns(data = returns.df,
                                      crop_fn = function(x) 1.1*x,
                                      crop_counties = iowa_counties),
                      ints = 10, 
                      scenario_name = "iowa" 
                      )
