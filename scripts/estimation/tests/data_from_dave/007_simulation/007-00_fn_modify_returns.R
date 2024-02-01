# Function to modify land use returns
#

identity <- function(x) x

mod_returns <- function(data, crop_fn = identity,
                        forest_fn = identity,
                        other_fn = identity,
                        CRP_fn = identity,
                        urban_fn = identity,
                        crop_counties = "all.fips",
                        forest_counties = "all.fips",
                        other_counties = "all.fips",
                        CRP_counties = "all.fips",
                        urban_counties = "all.fips"
) {
  
  data2 <- copy(data) %>% as.data.table()
  
  if (crop_counties == "all.fips") crop_counties <- data2$fips
  if (forest_counties == "all.fips") forest_counties <- data2$fips
  if (CRP_counties == "all.fips") CRP_counties <- data2$fips
  if (other_counties == "all.fips") other_counties <- data2$fips
  if (urban_counties == "all.fips") urban_counties <- data2$fips
  
  mod_data <- data2[fips %in% crop_counties , ":=" (crop_nr = crop_fn(crop_nr))] %>%
    .[fips %in% forest_counties , ":=" (forest_nr = forest_fn(forest_nr))] %>% 
    .[fips %in% CRP_counties , ":=" (CRP_nr = CRP_fn(CRP_nr))] %>% 
    .[fips %in% other_counties , ":=" (other_nr = other_fn(other_nr))] %>% 
    .[fips %in% urban_counties , ":=" (urban_nr = urban_fn(urban_nr))] 
  
  return(mod_data)
  
}
