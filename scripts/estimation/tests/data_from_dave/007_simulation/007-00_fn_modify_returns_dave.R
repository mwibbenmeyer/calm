# Function to modify land use returns
#

identity <- function(x) x

mod_returns <- function(data, crop_fn = identity,
                        forest_fn = identity,
                        urban_fn = identity,
                        crop_counties = "all.fips",
                        forest_counties = "all.fips",
                        urban_counties = "all.fips"
) {
  
  data2 <- copy(data) %>% as.data.table()
  
  if (crop_counties == "all.fips") crop_counties <- data2$fips
  if (forest_counties == "all.fips") forest_counties <- data2$fips
  if (urban_counties == "all.fips") urban_counties <- data2$fips
  
  mod_data <- data2[fips %in% crop_counties , ":=" (ma_nrev_ag = crop_fn(ma_nrev_ag))] %>%
    .[fips %in% forest_counties , ":=" (ma_rent_f = forest_fn(ma_rent_f))] %>% 
    .[fips %in% urban_counties , ":=" (pop_den = urban_fn(pop_den), pipc = urban_fn(pipc))] 
  
  return(mod_data)
  
}
