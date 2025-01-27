library(tidyverse)

# ---------------------------------------------------- #
# 1. Get the covariates and map
# ---------------------------------------------------- #

# Covariates
prev <- readRDS("analysis/data-derived/pfpr210_2000-2022.rds")
seasonality <- readRDS("analysis/data-derived/seasonality.rds")
travel <- readRDS("analysis/data-derived/MAP_travel.rds")
act <- readRDS("analysis/data-derived/ACT_usage_2000-2022.rds")
ft <- readRDS("analysis/data-derived/subnational_ft_2000-2022.rds")
pop <- readRDS("analysis/data-derived/pop_2000-2022.rds")

# Map to bind to
admin1 <- readRDS("analysis/data-derived/admin1_sf.rds")

# ---------------------------------------------------- #
# 2. Binding Function to Create List
# ---------------------------------------------------- #

# Function to do bind
create_final_df <- function(yr){
  
  left_join(admin1 %>% rename(iso3c = iso), 
            seasonality) %>% 
    left_join(travel) %>% 
    left_join(prev %>% filter(year == yr)) %>% 
    left_join(ft %>% filter(year == yr)) %>% 
    left_join(act %>% filter(year == yr)) %>% 
    left_join(pop %>% filter(year == yr)) %>% 
    sf::st_drop_geometry() %>% 
    select(iso3c, name_0, id_0, name_1, id_1, year, seasonality:pop_total) %>% 
    as.tibble()
  
}

# ---------------------------------------------------- #
# 3. Make the final Results Object
# ---------------------------------------------------- #

# pull for each year
yr_range <- 2000:2022
res <- lapply(yr_range, create_final_df)
names(res) <- paste0("Y", yr_range)

# and get the accompanying map
map <- admin1 %>% rename(iso3c = iso) %>% 
  select(iso3c, name_0, id_0, name_1, id_1)

# save to file
final <- list(
  "covars" = res,
  "map" = map
)

saveRDS(final, "analysis/data-derived/final_covariates.rds")
