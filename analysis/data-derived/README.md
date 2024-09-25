## data-derived files

### Final Covariate Object

1. `final_covariates.rds`: Final Covariate Object. List with two elements: 1) the
covariates (`covars`) `list` where each element is a `tibble` of covariates 
for each year and 2) an `sf` admin level 1 object (`map`) that the covariate data
frames correspond to.

### Individual Covariate Objects

1. `ACT_usage_2000-2022.rds`: Proportionate use of ACT types from 2000-2022. N.B. Data only for 2005 onwards
1. `ACT_usage_2000-2022_no_interpolation.rds`: Proportionate use of ACT types from 2000-2022 without interpolation. N.B. Data only for 2005 onwards
1. `pfpr210_2000-2022.rds`: MAP PfPR 2-10 Admin 1 from 2000-2022
1. `ft_2000-2022.rds`: MAP Effective Treatment Admin 0 from 2000-2022
1. `subnational_ft_2000-2022.rds`: MAP Effective Treatment Admin 1 from 2000-2022. N.B. Any missing data is due to no malaria prevalence.
1. `pop_2000-2022.rds`: WorldPop population counts from 2000-2022.
1. `seasonality.rds`: Admin level 1 Seasonality Coefficient of Variation 
1. `MAP_travel.rds`: Admin level 1 MAP travel data 

### Map Objects Being Bound To

1. `admin0_sf.rds`: `sf` object for Admin Level 0 boundaries from MAP 2024
1. `admin1_sf.rds`: `sf` object for Admin Level 1 boundaries from MAP 2024

### Intermediate Data Object

1. `travel_list.rds`: `List` of admin 1 pop weighted travel and friction times from MAP for 2020
1. `dhs_ML_FEVT_C_ADV_shps.rds`: `List` of countries with the DHS shape files for ML_FEVT_C_ADV indicator over time
