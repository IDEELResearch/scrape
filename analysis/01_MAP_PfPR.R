library(tidyverse)

# ---------------------------------------------------- #
# 1. Get the MAP PfPR Prevalence values
# ---------------------------------------------------- #

# Get MAP prevalence straight from their website
read_map_json <- function(type, prev = "Micro.2.10_mean") {
  
  url <- paste0(
    "https://data.malariaatlas.org/global-data/json-stat/2024/v1/malaria/admin1/Pf_PR-rate_2010-2022_2-10-years_",
    type,
    ".json")
  lci <- jsonlite::read_json(url)
  lci$value[sapply(lci$value, is.null)] <- NA
  
  df <- data.frame("prev" = unlist(lci$value)) %>%
    cbind(expand.grid(
      "year" = unlist(lci$dimension$Year)[-1],
      "id_1" = as.character(unlist(lci$dimension$ID))[-1]
    )) 
  
  names(df)[1] <- prev
  
  return(df)
  
}

# get the prev range for each region here
mean <- read_map_json("mean", "Micro.2.10_mean")

# join together and make a nicer name
df <- mean %>% mutate(id_1 = as.integer(as.character(id_1)))
df <- df %>% select(id_1, year, starts_with("Micro"))
names(df) <- gsub("Micro.*", "pfpr210", names(df))

## ----------------------------------------------------o
## 2. Pull ft from rasters via packages for prior to 2010 --------------
## ----------------------------------------------------o

admin1 <- readRDS("analysis/data-derived/admin1_sf.rds")
get_iso_year_prev <- function(iso3c, year) {
  
  get_map_prev <- function(iso3c, year) {
    pop <- cart::get_pop(iso3c = iso3c, year = year)
    pop_extent <- cart:::get_extent(pop)
    rast <- malariaAtlas::getRaster(year = year, extent = pop_extent, dataset_id = "Malaria__202406_Global_Pf_Parasite_Rate")
    rast <- lapply(as.list(rast), function(x, y) {
      terra::resample(x, y)
    }, y = pop)
    names(rast[[1]]) <- "pfpr210"
    rasters <- terra::rast(c(list(pop), rast))
    return(rasters)
  }
  
  data <- get_map_prev(iso3c, year)
  extracted_data <- cart::unpack_cart(admin1 %>% filter(iso == iso3c), data)
  
  summary_data <- extracted_data %>%
    dplyr::select(id_1, pop, pfpr210) %>%
    tidyr::unnest(cols = c(pop, pfpr210)) %>%
    tidyr::drop_na() %>%
    dplyr::group_by(id_1) %>%
    dplyr::summarise(pfpr210 = round(weighted.mean(pfpr210, pop, na.rm = TRUE), 3))
  
  fc <- admin1 %>% sf::st_drop_geometry() %>% 
    select(iso, id_0, name_0, id_1, name_1) %>% 
    filter(iso == iso3c) %>% 
    left_join(summary_data) %>%
    mutate(year = year, .before = "pfpr210") %>% 
    rename(iso3c = iso)
  
  return(fc)
  
}

# get our iso3c and year grid
pars <- expand.grid("iso3c" = unique(admin1$iso), 
                    "year" = 2000:2022)

# safely pull these
safe_get_prev <- purrr::safely(get_iso_year_prev, data.frame())
prev_res <- (pars %>% mutate(uuid = paste0(iso3c, year))) %>% 
  split(.$uuid) %>% 
  purrr::map(function(x){
    safe_get_prev(x$iso3c, x$year)
  }, .progress = TRUE)

# check that it is the same info - it is
do.call(rbind, lapply(prev_res, "[[", "result")) %>% filter(year == 2010 & id_1 == 10314456)
df %>% filter(year == 2010 & id_1 == 10314456)

# but the raster don't work for 2021 and 2022
# so bind with the earlier ft object for these years next


## ----------------------------------------------------o
## 3. Read in MAP admin map and make consistent --------------
## ----------------------------------------------------o

# get the admin names etc
admin1 <- readRDS("analysis/data-derived/admin1_sf.rds")

# are all the admin names found in our df?
# yes exampt for bodies of water (fine) and a few island nations (and lesotho which is just one admin with no MAP prevalence)
admin1[which(!(admin1$id_1 %in% df$id_1)),]

# bind it and complete missingness
df <- admin1 %>% sf::st_drop_geometry() %>% 
  select(iso3c = iso, id_0, name_0,
         id_1, name_1) %>% 
  left_join(df) %>% 
  mutate(year = as.numeric(as.character(year))) %>% 
  mutate(year = replace_na(year, 2010)) %>% 
  group_by(iso3c, name_0, id_1, name_1) %>% 
  complete(year = 2010:2022) 

prev_all <- rbind(
  do.call(rbind, lapply(prev_res, "[[", "result")) %>% select(names(df)),
  df %>% filter(year > 2020)
)

# save
saveRDS(prev_all, "analysis/data-derived/pfpr210_2000-2022.rds")
