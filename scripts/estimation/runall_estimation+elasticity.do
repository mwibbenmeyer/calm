
/******************************************************************************/
/* FOUR REGRESSIONS */
cd L:\Project-Land_Use\scripts\estimation
do revised_ddc_estimation_dy.do

cd L:\Project-Land_Use\scripts\estimation
do revised_ddc_estimation_st.do

cd L:\Project-Land_Use\scripts\estimation
do revised_ddc_estimation_lcc_dy.do

cd L:\Project-Land_Use\scripts\estimation
do revised_ddc_estimation_lcc_st.do

/******************************************************************************/
/* MODEL 1:  NOLCC-DYNAMIC */
cd L:\Project-Land_Use\scripts\estimation\elasticity_calc\nolcc_all\
local flist : dir . files "*.do", respectcase
di `"`flist'"' // show you the filelist
foreach fname of local flist {
  cd L:\Project-Land_Use\scripts\estimation\elasticity_calc\nolcc_all\
  di as red "Executing file: `fname'"
  do "`fname'"
}

** Before the next step, run the R file in this folder manually

/******************************************************************************/
/* MODEL 2:  NOLCC-STATIC */
cd L:\Project-Land_Use\scripts\estimation\elasticity_calc\nolcc_static_all\
local flist : dir . files "*.do", respectcase
di `"`flist'"' // show you the filelist
foreach fname of local flist {
  cd L:\Project-Land_Use\scripts\estimation\elasticity_calc\nolcc_static_all\
  di as red "Executing file: `fname'"
  do "`fname'"
}

/******************************************************************************/
/* MODEL 3:  LCC-DYNAMIC */
cd L:\Project-Land_Use\scripts\estimation\elasticity_calc\lcc_all\
local flist : dir . files "*.do", respectcase
di `"`flist'"' // show you the filelist
foreach fname of local flist {
  cd L:\Project-Land_Use\scripts\estimation\elasticity_calc\lcc_all\
  di as red "Executing file: `fname'"
  do "`fname'"
}

/******************************************************************************/
/* MODEL 4:  LCC-STATIC */
cd L:\Project-Land_Use\scripts\estimation\elasticity_calc\lcc_static_all\
local flist : dir . files "*.do", respectcase
di `"`flist'"' // show you the filelist
foreach fname of local flist {
  cd L:\Project-Land_Use\scripts\estimation\elasticity_calc\lcc_static_all\
  di as red "Executing file: `fname'"
  do "`fname'"
}

** After running this script, 1) run the R visualization scripts in each folder
** 2) edit the elasticities.xlsx in the processing folder
