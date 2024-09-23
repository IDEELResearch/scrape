library(tidyverse)

# MAP Africa map boundaries
isos <- unique(countrycode::codelist$iso3c[countrycode::codelist$continent == "Africa"]) %>% na.omit
map_1 <- malariaAtlas::getShp(ISO = isos, admin_level = c("admin1")) %>% sf::st_as_sf()
map_0 <- malariaAtlas::getShp(ISO = isos, admin_level = c("admin0")) %>% sf::st_as_sf()

# save to file for easier use
saveRDS(map_0, "analysis/data-derived/admin0_sf.rds")
saveRDS(map_1, "analysis/data-derived/admin1_sf.rds")
