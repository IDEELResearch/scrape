## Analysis Scripts

1. `00_map_fetch.R`: Fetch Admin 0 and Admin 1 shapes from MAP
1. `01_ACT_usage.R`: Parse ACT usage data from GF and PMI
1. `01_MAP_ft.R`: Fetch MAP effective treatment coverage over time at Admin 0 
1. `01_MAP_PfPR.R`: Fetch MAP PfPR over time at Admin 1
1. `01_pop.R`: Fetch WorldPop over time at Admin 0
1. `01_seasonality.R`: Fetch WorldClim Seasonality at Admin 0
1. `02_dhs_scrape.R`: Scrape DHS treatment seeking shapes over time (N.B. Data generated not used)
1. `03_subnational_ft.R`: Use latest DHS treatment seeking data to infer subnational effective treatment coverage over time at Admin 1
1. `04_travel.R`: Fetch MAP travel/accessibility data at Admin 1
1. `99_create_final_covars.R`: Pull all covariates together into final object at Admin 1
