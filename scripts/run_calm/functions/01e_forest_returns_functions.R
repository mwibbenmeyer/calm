
# Import functions used to calculate forest returns 
source("scripts/data/net_returns/forest/02a_forest_returns_functions.R")

# Forest demand elasticities, from Lubowski, Plantinga, and Stavins
forest_demand_elasticities <- data.frame(tamm_region = c("Pacific Northwest",
                                                     "Pacific Southwest",
                                                     "Rocky Mountains",
                                                     "North Central",
                                                     "South Central",
                                                     "Southeast",
                                                     "Northeast"),
                                         elasticity = c(-0.3,
                                                          -0.497,
                                                          -0.054,
                                                          -0.141,
                                                          -0.193,
                                                          -0.285,
                                                          -0.029))

# Update forest returns

update_forest_returns <- function(forest_prices, new, orig, elasticities = forest_demand_elasticities) {
  
  # Calc change in total production for species x product
  pct.change <- merge(total_sp_prodn(forest_area = calc_forest_area(old)),
                      total_sp_prodn(forest_area = calc_forest_area(new)),
                      by = c("spcd","product"),
                      suffixes = c(".new",".orig"))   %>% 
                  .[ , pct.change.q := 100*(species.prodn.new - species.prodn.orig)/species.prodn.orig]
  
  # Merge region-specific elasticities
  pct.change.reg <- do.call(rbind, lapply(unique(elasticities$tamm_region), function(x) pct.change %>% mutate(tamm_region = x))) %>% 
                      merge(elasticities, by = "tamm_region") %>% 
                  .[ , pct.change.p := pct.change.q/elasticity] 
  
  # Based on change in production, update commodity prices
  forest_prices_2 <- forest_prices %>% 
                      merge(pct.change.reg %>% 
                              select(spcd, product, tamm_region, pct.change.p), by = c("spcd","product","tamm_region")) %>% 
                      .[ , price := price*((100 + pct.change.p)/100)] %>% 
                      select(fips,spcd,product,price,tamm_region)

  # Calculate new weighted average returns
  new_returns <- calc_forest_returns(calc_forest_area(new), forest_prices_2)
  
  old_returns <- calc_forest_returns(calc_forest_area(old), forest_prices)
  

      
  
}  
  
  

