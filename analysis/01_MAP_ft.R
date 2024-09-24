library(tidyverse)

# ---------------------------------------------------- #
# 1. Get the MAP PfPR Prevalence values
# ---------------------------------------------------- #

# Get MAP ft straight from their website
read_map_ft_json <- function() {
  
  url <- "https://data.malariaatlas.org/global-data/json-stat/2024/v1/interventions/admin0/Antimalarial_EFT-rate_2010-2022_mean.json"
  lci <- jsonlite::read_json(url)
  lci$value[sapply(lci$value, is.null)] <- NA
  
  df <- data.frame("ft" = unlist(lci$value)) %>%
    cbind(expand.grid(
      "year" = unlist(lci$dimension$Year)[-1],
      "iso3c" = as.character(unlist(lci$dimension$ISO))[-1]
    )) 

  return(df)
  
}

# get the prev range for each region here
mean <- read_map_ft_json()

# join together and make a nicer name
ft <- mean %>% select(iso3c, year, ft)

## ----------------------------------------------------o
## 2. Pull ft from rasters via packages for prior to 2010 --------------
## ----------------------------------------------------o

get_iso_year_ft <- function(iso3c, year) {
  
  get_map_ft <- function(iso3c, year) {
    pop <- cart::get_pop(iso3c = iso3c, year = year)
    pop_extent <- cart:::get_extent(pop)
    ftrast <- malariaAtlas::getRaster(year = year, extent = pop_extent, dataset_id = "Interventions__202406_Global_Antimalarial_Effective_Treatment")
    ftrast <- lapply(as.list(ftrast), function(x, y) {
      terra::resample(x, y)
    }, y = pop)
    names(ftrast[[1]]) <- "ft"
    rasters <- terra::rast(c(list(pop), ftrast))
    return(rasters)
  }
  
  data <- get_map_ft(iso3c, year)
  extracted_data <- cart::unpack_cart(admin0 %>% filter(iso == iso3c), data)
  
  summary_data <- extracted_data %>%
    dplyr::select(pop, ft) %>%
    tidyr::unnest(cols = c(pop, ft)) %>%
    tidyr::drop_na() %>%
    dplyr::summarise(ft = round(weighted.mean(ft, pop, na.rm = TRUE), 3))
  
  fc <- admin0 %>% sf::st_drop_geometry() %>% 
    select(iso, id_0, name_0) %>% 
    filter(iso == iso3c) %>% 
    mutate(ft = summary_data$ft) %>%
    mutate(year = year) %>% 
    rename(iso3c = iso)
  
  return(fc)
  
}

# get our iso3c and year grid
admin0 <- readRDS("analysis/data-derived/admin0_sf.rds")
pars <- expand.grid("iso3c" = unique(admin0$iso), 
            "year" = 2000:2022)

# safely pull these
safe_get <- purrr::safely(get_iso_year_ft, data.frame())
ft_res <- (pars %>% mutate(uuid = paste0(iso3c, year))) %>% 
  split(.$uuid) %>% 
  purrr::map(function(x){
    safe_get(x$iso3c, x$year)
  }, .progress = TRUE)

# check that it is the same info - it is
do.call(rbind, lapply(ft_res, "[[", "result")) %>% filter(year == 2010 & iso3c == "AGO")
ft %>% filter(year == 2010 & iso3c == "AGO")

# but the raster don't work for 2021 and 2022
# so bind with the earlier ft object for these years next

## ----------------------------------------------------o
## 3. Read in MAP admin map and make consistent --------------
## ----------------------------------------------------o

# get the admin names etc
admin0 <- readRDS("analysis/data-derived/admin0_sf.rds")

# bind it and complete missingness
ft <- admin0 %>% sf::st_drop_geometry() %>% 
  select(iso3c = iso, id_0, name_0) %>% 
  left_join(ft) %>% 
  arrange(iso3c, year) %>% 
  mutate(year = as.integer(as.character(year))) %>% 
  mutate(year = replace_na(year, 2010)) %>% 
  group_by(iso3c, id_0, name_0) %>% 
  complete(year = 2010:2022) 

ft_all <- rbind(
  do.call(rbind, lapply(ft_res, "[[", "result")) %>% select(names(ft)),
  ft %>% filter(year > 2020)
)
  
# save
saveRDS(ft_all, "analysis/data-derived/ft_2000-2022.rds")


