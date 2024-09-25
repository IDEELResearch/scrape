library(tidyverse)

# ---------------------------------------------------- #
# 1. Get the WorldPop populations
# ---------------------------------------------------- #

get_pop_map <- function(iso3c, year) {
  
  pop <- cart::get_pop(iso3c = iso3c, year = year)
  pop_extent <- cart:::get_extent(pop)
  data <- terra::rast(c(list(pop)))
  extracted_data <- cart::unpack_cart(admin1 %>% filter(iso == iso3c), data)
  
  summary_data <- extracted_data %>%
    dplyr::select(id_1, pop) %>%
    tidyr::unnest(cols = c(pop)) %>%
    tidyr::drop_na() %>%
    dplyr::group_by(id_1) %>%
    dplyr::summarise(pop_total = sum(pop))
  
  fc <- admin1 %>% sf::st_drop_geometry() %>% 
    select(iso, id_0, name_0, id_1, name_1) %>% 
    filter(iso == iso3c) %>% 
    left_join(summary_data) %>%
    mutate(year = year, .before = pop_total) %>% 
    rename(iso3c = iso)
  
  return(fc)
  
}


# get our iso3c and year grid
pars <- expand.grid("year" = 2000:2022,
                    "iso3c" = unique(admin1$iso))

# safely pull these
safe_get_pop <- purrr::safely(get_pop_map, data.frame())
pop_res <- (pars %>% mutate(uuid = paste0(iso3c, year))) %>% 
  split(.$uuid) %>% 
  purrr::map(function(x){
    safe_get_pop(x$iso3c, x$year)
  }, .progress = TRUE)
pop_all <- do.call(rbind, lapply(pop_res, "[[", "result"))

## ----------------------------------------------------o
## 2. Read in MAP admin map and make consistent --------------
## ----------------------------------------------------o

# get the admin names etc
admin1 <- readRDS("analysis/data-derived/admin1_sf.rds")

# bind it and complete missingness
df_pop <- admin1 %>% sf::st_drop_geometry() %>% 
  select(iso3c = iso, id_0, name_0,
         id_1, name_1) %>%
  mutate(year = 2000) %>% 
  group_by(iso3c, id_0, name_0, id_1, name_1) %>% 
  complete(year = 2000:2022)
df_pop <- df_pop %>% left_join(pop_all)

# sort out the columns and rows
df_pop <- df_pop %>% 
  select(iso3c, name_0, id_0, name_1, id_1, year, pop_total) %>% 
  as_tibble()
rownames(df_pop) <- NULL

# save
saveRDS(df_pop, "analysis/data-derived/pop_2000-2022.rds")
