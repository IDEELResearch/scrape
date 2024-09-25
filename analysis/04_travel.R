library(tidyverse)

# ---------------------------------------------------- #
# 1. Get the travel values
# ---------------------------------------------------- #

# save to file
travel_list <- readRDS("analysis/data-derived/travel_list.rds")
all_travel <- do.call(rbind, travel_list)
all_travel <- all_travel %>%  select(id_1, starts_with("Explorer"))
names(all_travel) <- gsub("Explorer__", "MAP_", names(all_travel)) 

## ----------------------------------------------------o
## 2. Read in MAP admin map and make consistent --------------
## ----------------------------------------------------o

# get the admin names etc
admin1 <- readRDS("analysis/data-derived/admin1_sf.rds")

# bind it and complete missingness
df_travel <- admin1 %>% sf::st_drop_geometry() %>% 
  select(iso3c = iso, id_0, name_0,
         id_1, name_1) %>% 
  filter(id_1 != 0) %>%
  left_join(all_travel) %>% 
  as_tibble()

# sort out the columns and rows
prev_all <- prev_all %>% 
  select(iso3c, name_0, id_0, name_1, id_1, year, pfpr210) %>% 
  as_tibble()
rownames(prev_all) <- NULL

# save
saveRDS(df_travel, "analysis/data-derived/MAP_travel.rds")
