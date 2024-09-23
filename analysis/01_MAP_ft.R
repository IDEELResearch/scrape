library(tidyverse)

# ---------------------------------------------------- #
# 1. Get the MAP PfPR Prevalence values
# ---------------------------------------------------- #

# Get MAP prevalence straight from their website
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
## 2. Read in MAP admin map and make consistent --------------
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
  
# save
saveRDS(ft, "analysis/data-derived/ft_2010-2022.rds")


