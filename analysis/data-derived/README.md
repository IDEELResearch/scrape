## data-derived files

### Final Covariate Object

1. `final_covariates.rds`: Final Covariate Object. List with two elements: 1) the
covariates (`covars`) `list` where each element is a `tibble` of covariates 
for each year and 2) an `sf` admin level 1 object (`map`) that the covariate data
frames correspond to.

### Individual Covariate Objects

1. `ACT_usage_2010-2022.rds`: Proportionate use of ACT types from 2000-2022. N.B. Data only for 2019 onwards
1. `pfpr210_2010-2022.rds`: MAP PfPR 2-10 Admin 1 from 2010-2022
1. `ft_2000-2022.rds`: MAP Effective Treatment Admin 0 from 2000-2022
1. `seasonality.rds`: Admin level 1 Seasonality Coefficient of Variation 

### Map Objects Being Bound To

1. `admin0_sf.rds`: `sf` object for Admin Level 0 boundaries from MAP 2024
1. `admin1_sf.rds`: `sf` object for Admin Level 1 boundaries from MAP 2024
