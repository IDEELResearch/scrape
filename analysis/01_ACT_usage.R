library(tidyverse)

## ----------------------------------------------------o
## 1. Pull in drug use covariates since 2019 --------------
## ----------------------------------------------------o

# format the average drug use
dg <- readxl::read_xlsx("analysis/data-raw/PMI_GF_product_split_202122.xlsx",
                        sheet = "Combined", skip = 2)

# correctly name columns
dg <- dg %>% 
  setNames(
    c("country", 
      paste0(c("Total_"), paste(2019:2022)),
      paste0(c("AL_"), paste(2019:2022)),
      paste0(c("ASAQ_"), paste(2019:2022)),
      paste0(c("DP_"), paste(2019:2022)),
      paste0(c("ASPY_"), paste(2019:2022)),
      c("AL", "ASAQ", "DP", "ASPY")
    )
  ) %>% 
  select(name_0 = country, AL_2019:ASPY_2022) %>% 
  pivot_longer(AL_2019:ASPY_2022, names_to = c("product", "year"), names_pattern = c("(\\w*)_(\\d*)"))

# format drug proportions being used
dg <- dg %>% group_by(name_0, year) %>% 
  summarise(AL = value[product == "AL"]/sum(value),
            ASAQ = value[product == "ASAQ"]/sum(value),
            DP = value[product == "DP"]/sum(value),
            ASPY = value[product == "ASPY"]/sum(value)) %>% 
  mutate(iso3c = countrycode::countrycode(name_0, "country.name.en", "iso3c"), .before = 1) %>% 
  ungroup() %>% 
  select(-name_0)

## ----------------------------------------------------o
## 2. Read in MAP admin map and make consistent --------------
## ----------------------------------------------------o

# read in admin_0 and make consistent
admin0 <- readRDS("analysis/data-derived/admin0_sf.rds")
dg <- admin0 %>% sf::st_drop_geometry() %>% 
  select(iso3c = iso, id_0, name_0) %>% 
  left_join(dg)  %>% 
  arrange(iso3c, year) %>% 
  mutate(year = as.integer(as.character(year))) %>% 
  mutate(year = replace_na(year, 2010)) %>% 
  group_by(iso3c, id_0, name_0) %>% 
  complete(year = 2010:2022) 

# save
saveRDS(dg, "analysis/data-derived/ACT_usage_2010-2022.rds")
