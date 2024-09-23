library(tidyverse)

## ----------------------------------------------------o
## 1. Pull in drug use covariates since 2019 --------------
## ----------------------------------------------------o
covars <-  readRDS("analysis/data-derived/global_covariate_ranges.rds")

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
  select(country, AL_2019:ASPY_2022) %>% 
  pivot_longer(AL_2019:ASPY_2022, names_to = c("product", "year"), names_pattern = c("(\\w*)_(\\d*)"))

# format drug proportions being used
dg <- dg %>% group_by(country, year) %>% 
  summarise(AL = value[product == "AL"]/sum(value),
            ASAQ = value[product == "ASAQ"]/sum(value),
            DP = value[product == "DP"]/sum(value),
            ASPY = value[product == "ASPY"]/sum(value)) %>% 
  mutate(iso3c = countrycode::countrycode(country, "country.name.en", "iso3c"), .before = 1)

# save
saveRDS(dg, "analysis/data-derived/ACT_usage_2019-2022.rds")
