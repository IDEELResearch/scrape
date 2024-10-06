library(tidyverse)
library(ggpubr)
library(MetBrewer)
library(zoo)
# source("R/utils.R") ## don't need when compiled as a package
# library(here)

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
  pivot_longer(AL_2019:ASPY_2022, names_to = c("product", "year"), names_pattern = c("(\\w*)_(\\d*)")) %>%
  mutate(iso3c = countrycode::countrycode(name_0, "country.name.en", "iso3c")) 

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
  rename(country_territory = i_country_teritorry) %>%
  group_by(country_territory, year, product) %>% 
  summarise(value = sum(measure_values*nb_of_suom_in_pack)) %>% 
  rename(name_0 = country_territory) %>% 
  select(name_0, product, year, value) %>%
  mutate(name_0 = ifelse(name_0=="CÃ´te d'Ivoire", "Cote d'Ivoire",name_0)) |>
  mutate(iso3c = countrycode::countrycode(name_0, "country.name.en", "iso3c")) 
### ignore errors

## ----------------------------------------------------o
## 3. Bring Together and Combine --------------
## ----------------------------------------------------o

# and bind to dg
full <- rbind(dg, gf) %>% 
  group_by(iso3c, product,year) %>% 
  summarise(value = sum(value))

# make wide format
full2 <- full %>% 
  mutate(year = as.integer(year)) %>% 
  group_by(iso3c) %>% 
  complete(year = 2005:2024) %>% 
  group_by(iso3c, year) %>% 
  complete(product = unique(gf$product)) %>% 
  filter(!is.na(product)) %>% 
  group_by(iso3c, year) %>% 
  summarise(AL = value[product == "AL"]/sum(value, na.rm = TRUE),
            ASAQ = value[product == "ASAQ"]/sum(value, na.rm = TRUE),
            DHAPPQ = value[product == "DHAPPQ"]/sum(value, na.rm = TRUE),
            ASPY = value[product == "ASPY"]/sum(value, na.rm = TRUE),
            ASSP = value[product == "ASSP"]/sum(value, na.rm = TRUE),
            ASMQ = value[product == "ASMQ"]/sum(value, na.rm = TRUE),
            ASPY = value[product == "ASPY"]/sum(value, na.rm = TRUE)) %>% 
  ungroup() %>% 
  arrange(iso3c, year)
  
# filter to just Africa
full2 <- full2 %>% 
  mutate(continent = countrycode::countrycode(iso3c, "iso3c", "continent")) %>% 
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
full2 <- full2 %>% 
  mutate(name_0 = countrycode::countrycode(iso3c, "iso3c", "country.name"))
  
# save this out for Lucy to look at against drug policies
saveRDS(full2, "analysis/data-derived/ACT_usage_no_interpolation_2000-2022.rds")

# make a summary plot
act_gg <- full2 %>% pivot_longer(AL:ASMQ) %>% 
  mutate(continent = countrycode::countrycode(name_0, "country.name.en", "continent")) %>% 
  filter(continent %in% "Africa") %>%
  ungroup %>%
  complete(name_0, name, year, fill = list(value = 0)) %>%
  rename(drug = name) %>%
  ggplot(aes(year, value, fill = drug, color = drug)) +
  geom_col(na.rm = TRUE) +
  facet_wrap(~iso3c, ncol = 5) +
  #MetBrewer::scale_fill_met_d(name = "Drug", palette_name = "Lakota") +
  #MetBrewer::scale_fill_met_d(name = "Lakota") +
  scale_fill_manual(values = met.brewer("Lakota"), name="Drug") +
  scale_color_manual(values = met.brewer("Lakota"), name="Drug") +
  #MetBrewer::scale_color_met_d(name = "Drug", palette_name = "Lakota") +
  #MetBrewer::scale_color_met_d() +
  theme_bw(base_family = "Helvetica", base_size = 14) +
  xlab("Year") +
  ylab("Proportion of Drug Volumes") + 
  theme(panel.spacing = unit(1, "lines"))
save_figs("act_usage", act_gg, width = 14, height = 16)

# Initial interpolation (to be adjusted later based on drug policies)
# it looks like we can probability just interpolate
## use downup as we assume some time delay before distributing drugs.
full3 <- full2 %>% group_by(iso3c) %>% fill(AL:ASMQ, .direction = "downup") %>% pivot_longer(AL:ASMQ) %>% 
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
saveRDS(dg, "analysis/data-derived/ACT_usage_first_interpolation_2000-2022.rds")


## ----------------------------------------------------o
## 4. Read in and format drug policy data  --------------
## ----------------------------------------------------o

# read in and keep first line policies only
policy <- read.csv('analysis/data-raw/country_drug_policies.csv') %>%
  select(COUNTRY:X1995_1) %>% 
  pivot_longer(X2022_1:X1995_1, names_to = "year", values_to = "drug") %>%
  rename(name_0 = COUNTRY) %>%
  mutate(year = gsub("X", "", year),
         year = as.numeric(gsub("_1", "", year)))


## format and clean up the drug names to just look at main 6 ACTs
policy <- policy  |>
  mutate(drug = gsub("\\+PQ", "", drug),
         drug = gsub("AQ\\+SP", "nonACT", drug),
         drug = gsub("CQ\\+SP", "nonACT", drug),
         drug = gsub("preACT", "nonACT", drug),
         drug = gsub("CQ", "nonACT", drug),
         drug = gsub(", nonACT", "", drug),
         drug = gsub("\\+nonACT", "", drug),
         drug = gsub("AL, AL", "AL", drug),
         drug = gsub("AL, AL\\+AM", "AL", drug),
         drug = ifelse(drug=="AL, AS", "AL", drug),
         drug = ifelse(drug=="SP", "nonACT", drug),
         drug = ifelse(drug=="AS", "AS", drug),
         drug = gsub("nonACT\\+nonACT", "nonACT", drug),
  )

# make a summary plot for a first look
pol_plot1 <- policy %>%
  mutate(dummy =1) %>%
  ggplot(aes(year, dummy, fill = drug, color = drug)) +
  geom_col(na.rm = TRUE) +
  facet_wrap(~name_0, ncol = 5) 

## ----------------------------------------------------o
## 4. Combine drug policy data and drug procurement Global fund data and interpolate --------------
## ----------------------------------------------------o

## add iso3c and merge with raw data
full2 <- full2 %>%
  ungroup() %>%
  complete(year = c(2000:2024),
           iso3c = unique(full2$iso3c)) %>%
  arrange(iso3c, year)


gf_pol <- policy %>% 
  mutate(iso3c = countrycode::countrycode(name_0, "country.name.en", "iso3c")) %>%
  rename(drug_policy = drug) %>%
  right_join(full2, by=c("iso3c", "year"))


###### Compare policy and drug use
gf_pol_plot <- gf_pol %>%
  mutate(drug_policy = replace(drug_policy, drug_policy=='NO DATA', NA)) %>%
  select(drug_policy, AL:ASMQ) %>%
  pivot_longer(cols = AL:ASMQ, names_to = 'drug', values_to = 'proportion_drug') 

gf_pol1 <-  ggplot(gf_pol_plot,aes(x=drug_policy, y=proportion_drug, color=drug)) +
  geom_point() +
  theme_bw() +
  scale_x_discrete(guide = guide_axis(angle = 90)) 

gf_pol2 <-  ggplot(gf_pol_plot,aes(x=drug_policy, y=proportion_drug, fill=drug)) +
  geom_boxplot() +
  geom_point(aes(colour = drug), position = position_dodge(width = .75)) +
  theme_bw() +
  scale_x_discrete(guide = guide_axis(angle = 90))
ggsave(gf_pol2, file="analysis/plots/summary_act_policy_vs_median_gf_purchase.tiff", compression="lzw",
       width=7, height=7)

# merge with OJ's interpolation as well
gf_pol <- full3 %>%
  rename(AL_interp = AL,
         ASAQ_interp = ASAQ,
         ASMQ_interp = ASMQ  ,
         ASPY_interp = ASPY  ,
         ASSP_interp = ASSP,
         DHAPPQ_interp = DHAPPQ) %>% 
  select(-name_0) %>%
  ungroup() %>%
  complete(year = c(2000:2024),
           iso3c = unique(full2$iso3c)) %>%
  arrange(iso3c, year) %>%
  fill(matches("_interp"), .direction = "up") %>%
  right_join(gf_pol, by = c("year", "iso3c"))

## Set pre-ACT policy values to be zero (there is no procurement data at this time anyway.
gf_pol_interp <- gf_pol %>%
  mutate(across(AL_interp:DHAPPQ_interp, ~replace(., drug_policy=="nonACT", 0)))
##### if drug procurement data is missing for a year and drug data before or after
## match policy at 100%, fill in the policy drug
# let's not worry about zanzibar for now.
gf_pol_interp <- gf_pol_interp %>%
  filter(name_0.x!="Tanzania, Zanzibar") %>%
  group_by(iso3c) %>%
  mutate(drug_policy_simple = ifelse(drug_policy %in% c("AL","AS+AQ","nonACT", "AS+MQ","","NO DATA","AS+SP", "AS"),
                                     drug_policy,"MFT"),
         ) %>%
         mutate(drug_policy_simple = replace(drug_policy_simple,drug_policy_simple=="", "NO DATA" ))
  

#### Check drug proportions - remove ASPY and ASMQ as they are zero or tiny
##gf_pol_plot2 %>% group_by(drug) %>% summarise(test = sum(proportion_drug,na.rm=T))

gf_pol_plot2 <- gf_pol_interp %>% 
  select(iso3c, name_0, year, drug_policy_simple, AL_interp:DHAPPQ_interp, AL:ASMQ) %>%
  rename_with(~ paste0(.x, "_raw"), 
              .cols = AL:ASMQ) %>%
  pivot_longer(cols = c(AL_interp:DHAPPQ_interp, AL_raw:ASMQ_raw), 
               names_to = c("drug","data_type"), 
               values_to = c("proportion_drug"),
               names_sep = "_") %>%
  fill(name_0, .direction = "up") %>%
  filter(!(drug %in% c("ASMQ", "ASPY")))
               

p <-ggplot(gf_pol_plot2, 
           aes(x = year, y = proportion_drug, color = drug, group = drug)) +
  geom_rect(aes(xmin = year - 0.5, 
                xmax = year + 0.5, 
                ymin = -Inf, ymax = Inf, 
                fill = drug_policy_simple),  
            alpha = 0.05, inherit.aes = FALSE) +  # Transparency for background
  geom_line(data = subset(gf_pol_plot2, data_type == "interp")) +
  geom_point(data = subset(gf_pol_plot2, data_type == "raw")) +
  theme_bw() +
  facet_wrap(~name_0) +
  scale_color_manual(values = c("nonACT" = "black",
                                "ASAQ" = "orange",
                                "AL" = "blue",
                                "ASSP"  = "purple",  
                                "DHAPPQ"  = "darkgreen")) +
  
  scale_fill_manual(values = c("nonACT" = "gray80",
                               "MFT" = "lightgreen",
                               "AS+AQ" = "yellow1",
                               "AL" = "lightblue",
                               "AS+SP" = "purple1",
                               "AS" = "pink1",
                               "NO DATA" = "white")) +
  
  # Override the alpha in the legend to make the legend fill visible
  guides(fill = guide_legend(override.aes = list(alpha = 1))) +
  
  xlab("year") +
  ylab("proportion of drug purchases, GF") +
  ylim(c(-0.1, 1.1))
#p
ggsave(filename = "analysis/plots/policy_vs_use_original.tiff", plot=p, compression="lzw",
       width = 13, height = 7)  


#### Edit interpolated drug procurement based on country drug policy
# Equatorial Guinea & Gabon – switched to MFT from ASAQ recently and no procurement data. 
      #->Use mean of recent MFT neighbouring countries in the same years (Cameroon, DRC)
gf_pol_interp2 <- gf_pol_interp %>%
  group_by(year) %>%
  mutate(AL_interp2 = replace(AL_interp, 
                              drug_policy_simple == "MFT" & (iso3c == "GNQ" | iso3c == "GAB"),
                              mean(AL_interp[drug_policy_simple == "MFT" & 
                                               (iso3c == "CMR" | iso3c == "COD") & 
                                               year == cur_group()$year], na.rm = TRUE)),
         ASAQ_interp2 = replace(ASAQ_interp, 
                              drug_policy_simple == "MFT" & (iso3c == "GNQ" | iso3c == "GAB"),
                              mean(ASAQ_interp[drug_policy_simple == "MFT" & 
                                               (iso3c == "CMR" | iso3c == "COD") & 
                                               year == cur_group()$year], na.rm = TRUE))) %>%
  ungroup()

#   Congo Brazzaville interpolation is out of sync with policy and missing recent years. 
#  Interpolate using neighbouring countries with the same drug policy: Cameroon, DRC, Gabon.
gf_pol_interp2 <- gf_pol_interp2 %>%
  group_by(year) %>%
  mutate(AL_interp2 = replace(AL_interp2, 
                              (is.na(AL) & iso3c == "COG" & drug_policy_simple!="nonACT"),
                              mean(AL_interp2[iso3c %in% c("CMR", "COD", "GAB") & 
                                               year == cur_group()$year], na.rm = TRUE)),
         ASAQ_interp2 = replace(ASAQ_interp2, 
                              (is.na(ASAQ) & iso3c == "COG" & drug_policy_simple!="nonACT"),
                              mean(ASAQ_interp2[(iso3c == "CMR" | iso3c == "COD" | iso3c == "GAB") & 
                                               year == cur_group()$year], na.rm = TRUE))) %>%
         ungroup()
# Djibouti – missing data after policy switch from ASSP to AL. 
# Assume the switch occurs at similar time as other countries in the region with ASSP policy: Sudan, Somalia.
gf_pol_interp2 <- gf_pol_interp2 %>%
  group_by(year) %>%
  mutate(AL_interp2 = replace(AL_interp2, 
                              (is.na(AL) & iso3c == "DJI" & year>2011),
                              mean(AL_interp2[iso3c %in% c("SDN", "SOM") &
                                                year == cur_group()$year], na.rm = TRUE)),
         ASSP_interp2 = replace(ASSP_interp, 
                                (is.na(ASSP) & iso3c == "DJI"  & year>2011),
                                mean(ASSP_interp[iso3c %in% c("SDN", "SOM") & 
                                                    year == cur_group()$year], na.rm = TRUE))) %>%
  ungroup()

# Mali – gaps in procurement data during MFT policy – uncertain time of change from ASAQ to AL. 
# Implement linear interpolation instead of step change.
gf_pol_interp2 <- gf_pol_interp2 %>%
  mutate(AL_interp2 = replace(AL_interp2, iso3c=="MLI" & year>=2010 & year<=2013, NA),
         ASAQ_interp2 = replace(ASAQ_interp2, iso3c=="MLI" & year>=2010 & year<=2013, NA),
         AL_interp2 = if_else(is.na(AL_interp2 & iso3c=="MLI" & year>=2010 & year<=2013), 
                   zoo::na.approx(AL_interp2, na.rm = FALSE), AL_interp2),
         ASAQ_interp2 = if_else(is.na(ASAQ_interp2 & iso3c=="MLI" & year>=2010 & year<=2013), 
                               zoo::na.approx(ASAQ_interp2, na.rm = FALSE), ASAQ_interp2))

#### Tidy it all up and use the new interp2 values.
# Renormalise all the 'interp' values as some will not sum to 1 when using neighbouring country means.
gf_pol_interp2 <- gf_pol_interp2 %>%
  select(-c(AL_interp, ASAQ_interp, ASSP_interp)) %>%
  rename_with(~ gsub("interp2", "interp", .x), .cols = matches("interp2")) %>%
  select(iso3c, name_0, year, drug_policy_simple, matches("_interp"), AL:ASMQ) %>%
  rename_with(~ paste0(.x, "_raw"), .cols = AL:ASMQ) %>%
  # Add a new column 'total_gf_drugs' which sums across all columns that match "_interp"
  rowwise() %>%
  mutate(total_gf_drugs = sum(across(matches("_interp")), na.rm = TRUE)) %>%
  # Apply the transformation only if total_gf_drugs > 0
  mutate(across(matches("_interp"), ~ ifelse(total_gf_drugs > 0, .x / total_gf_drugs, 0))) %>%
  mutate(name_0 = countrycode::countrycode(iso3c, "iso3c", "country.name")) %>%
  ungroup()

# save for future covariate use
saveRDS(gf_pol_interp2, "analysis/data-derived/ACT_usage_final_interpolation_2000-2022.rds")


gf_pol_plot3 <- gf_pol_interp2 %>% 
  pivot_longer(cols = c(matches("interp"), AL_raw:ASMQ_raw), 
               names_to = c("drug","data_type"), 
               values_to = c("proportion_drug"),
               names_sep = "_") %>%
  fill(name_0, .direction = "up") %>%
  filter(!(drug %in% c("ASMQ", "ASPY")))


p2 <-ggplot(gf_pol_plot3, 
           aes(x = year, y = proportion_drug, color = drug, group = drug)) +
  geom_rect(aes(xmin = year - 0.5, 
                xmax = year + 0.5, 
                ymin = -Inf, ymax = Inf, 
                fill = drug_policy_simple),  
            alpha = 0.05, inherit.aes = FALSE) +  # Transparency for background
  geom_line(data = subset(gf_pol_plot3, data_type == "interp")) +
  geom_point(data = subset(gf_pol_plot3, data_type == "raw")) +
  theme_bw() +
  facet_wrap(~name_0) +
  scale_color_manual(values = c("nonACT" = "black",
                                "ASAQ" = "orange",
                                "AL" = "blue",
                                "ASSP"  = "purple",  
                                "DHAPPQ"  = "darkgreen")) +
  
  scale_fill_manual(values = c("nonACT" = "gray80",
                               "MFT" = "lightgreen",
                               "AS+AQ" = "yellow1",
                               "AL" = "lightblue",
                               "AS+SP" = "purple1",
                               "NO DATA" = "white")) +
  
  # Override the alpha in the legend to make the legend fill visible
  guides(fill = guide_legend(override.aes = list(alpha = 1))) +
  
  xlab("year") +
  ylab("proportion of drug purchases, GF") +
  ylim(c(-0.1, 1.1))
p2
ggsave(filename = "analysis/plots/policy_vs_use_update.tiff", plot=p2, compression="lzw",
       width = 13, height = 7)  
