library(tidyverse)

## ----------------------------------------------------o
## 1. Pull in drug use covariates since 2019 from PMI --------------
## ----------------------------------------------------o

# format the average drug use
dg <- readxl::read_xlsx("analysis/data-raw/PMI_GF_product_split_202122.xlsx",
                        sheet = "PMI_GHSC", skip = 1)
dg <- dg[,-c(6,11,16,21,26)]

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

## ----------------------------------------------------o
## 2. Wrangle the full GF data --------------
## ----------------------------------------------------o

# read in the full data from GF
# https://insights.theglobalfund.org/t/Public/views/PriceQualityReportingTransactionSummary/TransactionSummary?iframeSizedToWindow=true&%3Aembed=y&%3AshowAppBanner=false&%3Adisplay_count=no&%3AshowVizHome=no
gf <- read.csv("analysis/data-raw/full_gfpqr_data.csv")
gf <- janitor::clean_names(gf)
gf <- gf %>% filter(measure_names == "Pack quantity")

# replace product names
gf <- gf %>%
  mutate(product = replace(product, grep("Lumefantrine", product, fixed = TRUE), "AL")) %>%
  mutate(product = replace(product, grep("Amodiaquine+[Sulfadoxine+Pyrimethamine]", product, fixed = TRUE), "AQSP")) %>%
  mutate(product = replace(product, grep("Chloroquine", product, fixed = TRUE), "CQ")) %>%
  mutate(product = replace(product, grep("Artesunate + Mefloquine", product, fixed = TRUE), "ASMQ")) %>%
  mutate(product = replace(product, grep("Artesunate + [Sulfadoxine+Pyrimethamine]", product, fixed = TRUE), "ASSP")) %>%
  mutate(product = replace(product, grep("Artesunate + Amodiaquine", product, fixed = TRUE), "ASAQ")) %>%
  mutate(product = replace(product, grep("Artesunate+Pyronaridine", product, fixed = TRUE), "ASPY")) %>%
  mutate(product = replace(product, grep("Dihydroartemisinin+Piperaquine", product, fixed = TRUE), "DHAPPQ")) %>%
  mutate(product = replace(product, grep("Quinine", product, fixed = TRUE), "QU")) %>%
  mutate(product = replace(product, grep("Sulfadoxine+Pyrimethamine", product, fixed = TRUE), "SP")) %>%
  mutate(product = replace(product, grep("Primaquine", product, fixed = TRUE), "PQ")) %>%
  mutate(product = replace(product, grep("Mefloquine", product, fixed = TRUE), "MQ")) %>%
  mutate(product = replace(product, nchar(product) > 6, "ART"))

# just focus on front line
gf <- gf %>% 
  mutate(treatment = "Frontline") %>% 
  mutate(treatment = replace(treatment, product == "ART", "Severe")) %>% 
  mutate(treatment = replace(treatment, product == "PQ", "Radical Cure")) %>% 
  mutate(treatment = replace(treatment, product %in% c("SP","AQSP"), "SMC")) %>% 
  mutate(treatment = replace(treatment, product %in% c("QU","CQ","MQ"), "Non-ACT"))
  
# sort out dates and work out total doses
gf <- gf %>% filter(treatment == "Frontline") %>% 
  mutate(date = as.Date(actual_delivery_date, "%d/%m/%Y %H:%M:%S")) %>% 
  mutate(year = lubridate::year(date)) %>% 
  group_by(country_teritorry, year, product) %>% 
  summarise(value = sum(measure_values*nb_of_suom_in_pack)) %>% 
  rename(name_0 = country_teritorry) %>% 
  select(name_0, product, year, value)

## ----------------------------------------------------o
## 3. Bring Together and Combine --------------
## ----------------------------------------------------o

# and bind to dg
full <- rbind(dg, gf) %>% 
  group_by(name_0, product,year) %>% 
  summarise(value = sum(value))

# make wide format
full2 <- full %>% 
  mutate(year = as.integer(year)) %>% 
  group_by(name_0) %>% 
  complete(year = 2005:2024) %>% 
  group_by(name_0, year) %>% 
  complete(product = unique(gf$product)) %>% 
  filter(!is.na(product)) %>% 
  group_by(name_0, year) %>% 
  summarise(AL = value[product == "AL"]/sum(value, na.rm = TRUE),
            ASAQ = value[product == "ASAQ"]/sum(value, na.rm = TRUE),
            DHAPPQ = value[product == "DHAPPQ"]/sum(value, na.rm = TRUE),
            ASPY = value[product == "ASPY"]/sum(value, na.rm = TRUE),
            ASSP = value[product == "ASSP"]/sum(value, na.rm = TRUE),
            ASMQ = value[product == "ASMQ"]/sum(value, na.rm = TRUE),
            ASPY = value[product == "ASPY"]/sum(value, na.rm = TRUE)) %>% 
  mutate(iso3c = countrycode::countrycode(name_0, "country.name.en", "iso3c"), .before = 1) %>% 
  ungroup() %>% 
  arrange(iso3c, year)
  
# filter to just Africa
full2 <- full2 %>% 
  mutate(continent = countrycode::countrycode(name_0, "country.name.en", "continent")) %>% 
  filter(continent %in% "Africa") %>%
  select(-continent) %>% 
  ungroup 

# sort out missing vs 0
nms_pos <- which(names(full2) %in% tail(names(full2), 6))
for(i in seq_along(full2$AL)){
  if((full2[i,] %>% select(AL:ASMQ) %>% as.numeric() %>% sum(na.rm = TRUE)) == 1){
    full2[i,nms_pos][is.na(full2[i,nms_pos])] <- 0
  }
}
# save this out for Lucy to look at against drug policies
saveRDS(full2, "analysis/data-derived/ACT_usage_no_interpolation_2000-2022.rds")

# make a summary plot
act_gg <- full2 %>% pivot_longer(AL:ASMQ) %>% 
  mutate(continent = countrycode::countrycode(name_0, "country.name.en", "continent")) %>% 
  filter(continent %in% "Africa") %>%
  ungroup %>%
  complete(name_0, name, year, fill = list(value = 0)) %>%
  ggplot(aes(year, value, fill = name, color = name)) +
  geom_col(na.rm = TRUE) +
  facet_wrap(~name_0, ncol = 5) +
  MetBrewer::scale_fill_met_d(name = "Drug", palette_name = "Lakota") +
  MetBrewer::scale_color_met_d(name = "Drug", palette_name = "Lakota") +
  theme_bw(base_family = "Helvetica", base_size = 14) +
  xlab("Year") +
  ylab("Proportion of Drug Volumes") + 
  theme(panel.spacing = unit(1, "lines"))
save_figs("act_usage", act_gg, width = 14, height = 16)

# it looks like we can probability just interpolate
full3 <- full2 %>% group_by(name_0) %>% fill(AL:ASMQ, .direction = "updown") %>% pivot_longer(AL:ASMQ) %>% 
  mutate(continent = countrycode::countrycode(name_0, "country.name.en", "continent")) %>% 
  filter(continent %in% "Africa") %>%
  select(-continent) %>% 
  ungroup %>%
  complete(name_0, name, year, fill = list(value = 0)) %>%
  group_by(iso3c, name_0, year) %>% 
  mutate(value = value/sum(value)) %>% 
  pivot_wider(names_from = name, values_from = value)

act_interp_gg <- full3 %>% pivot_longer(AL:DHAPPQ) %>% 
  mutate(continent = countrycode::countrycode(name_0, "country.name.en", "continent")) %>% 
  filter(continent %in% "Africa") %>%
  ungroup %>%
  complete(name_0, name, year, fill = list(value = 0)) %>%
  ggplot(aes(year, value, fill = name, color = name)) +
  geom_col(na.rm = TRUE) +
  facet_wrap(~name_0, ncol = 5) +
  MetBrewer::scale_fill_met_d(name = "Drug", palette_name = "Lakota") +
  MetBrewer::scale_color_met_d(name = "Drug", palette_name = "Lakota") +
  theme_bw(base_family = "Helvetica", base_size = 14) +
  xlab("Year") +
  ylab("Proportion of Drug Volumes Interpolated") + 
  theme(panel.spacing = unit(1, "lines"))
save_figs("act_interp_usage", act_interp_gg, width = 14, height = 16)

## ----------------------------------------------------o
## 3. Read in MAP admin map and make consistent --------------
## ----------------------------------------------------o

# read in admin_0 and make consistent
admin0 <- readRDS("analysis/data-derived/admin0_sf.rds")
dg <- admin0 %>% sf::st_drop_geometry() %>% 
  select(iso3c = iso, id_0, name_0) %>% 
  left_join(full3)  %>% 
  arrange(iso3c, year) %>% 
  mutate(year = as.integer(as.character(year))) %>% 
  mutate(year = replace_na(year, 2000)) %>% 
  group_by(iso3c, id_0, name_0) %>% 
  complete(year = 2000:2022) 

# save
saveRDS(dg, "analysis/data-derived/ACT_usage_2000-2022.rds")
