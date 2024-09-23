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
  
  return(df)
  
}

# get the prev range for each region here
lci <- read_map_json("lci", "Micro.2.10_low")
mean <- read_map_json("mean", "Micro.2.10_mean")
uci <- read_map_json("uci", "Micro.2.10_high")

# join together and make a nicer name
df <- left_join(lci, mean) %>% left_join(uci) %>% mutate(id_1 = as.integer(as.character(id_1)))
df <- df %>% select(id_1, year, starts_with("Micro"))
names(df) <- gsub("Micro.2.10", "pfpr210", names(df), fixed = TRUE)

## ----------------------------------------------------o
## 2. Read in MAP admin map and make consistent --------------
## ----------------------------------------------------o

# get the admin names etc
admin1 <- readRDS("analysis/data-derived/admin1_sf.rds")

# are all the admin names dound in our df?
# yes exampt for bodies of water (fine) and a few island nations (and lesotho which is just one admin with no MAP prevalence)
admin1[which(!(admin1$id_1 %in% df$id_1)),]

df <- admin1 %>% sf::st_drop_geometry() %>% 
  select(iso3c = iso, name_0,
         id_1, name_1) %>% 
  left_join(df)

# save
saveRDS(df, "analysis/data-derived/pfpr210_2010-2022.rds")
