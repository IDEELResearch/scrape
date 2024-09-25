library(tidyverse)

# ---------------------------------------------------- #
# 1. Function to read in and format GBD ----
# ---------------------------------------------------- #

fls <- list.files("analysis/data-raw/GBD/", full.names = TRUE)

fl <- "analysis/data-raw/GBD/IHME-GBD_2021_DATA-13da5ea5-1.csv"
fl <- "analysis/data-raw/GBD/IHME-GBD_2021_DATA-5dcf9eb3-1.csv"
fl <- "analysis/data-raw/GBD/IHME-GBD_2021_DATA-583362ab-1.csv"

lapply(fls, read.csv) %>% 
  lapply(function(x){c(table(x$rei_name),table(x$metric_name))})

dat <- read.csv(fl)
dat <- dat %>% 
  select(name_0 = location_name, 
         measure = measure_name, 
         metric = metric_name, 
         year = year, 
         value = val) %>% 
  filter(metric == "Rate")

dat <- dat %>% 
  mutate(iso3c = countrycode::countrycode(name_0, "country.name.en", "iso3c"), .before = 1)

# name and select columns
var <- paste0("GBD_", dat$measure[1], "_", dat$metric[1])
names(dat)[ncol(dat)] <- var
dat <- dat %>% select(all_of(c("iso3c", "name_0", "year", var)))
