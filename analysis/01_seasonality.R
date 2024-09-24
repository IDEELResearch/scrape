library(tidyverse)

# ---------------------------------------------------- #
# 1. Get Climate Seasonality from WorldClimate
# ---------------------------------------------------- #

climate <- raster::getData('worldclim', var='bio', res=2.5, path = "analysis/data-raw")
seasonalprecip <- climate$bio15

# get the admin 1
admin1 <- readRDS("analysis/data-derived/admin1_sf.rds")
seasonalprecip_inter <- raster::extract(seasonalprecip, admin1)
admin1$seasonality <- unlist(lapply(seasonalprecip_inter, mean, na.rm = TRUE))

# Annobon has no climate so take the nearest island
admin1$seasonality[is.na(admin1$seasonality)] <- 59 # average for Sao Tome nearby

## ----------------------------------------------------o
## 2. Make it consistent --------------
## ----------------------------------------------------o

seasonality <- admin1 %>% sf::st_drop_geometry() %>% 
  select(iso3c = iso, name_0, id_0,
         id_1, name_1, seasonality) %>% 
  as.tibble()

# save
saveRDS(seasonality, "analysis/data-derived/seasonality.rds")
