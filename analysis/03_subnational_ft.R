library(tidyverse)

# ---------------------------------------------------- #
# 1. Get access covariate dataset from MAP rasters ----
# ---------------------------------------------------- #

# rasters
rasters <- c("Walking-only travel time to healthcare map without access to motorized transport",
             "Global friction surface enumerating land-based travel speed with access to motorized transport for a nominal year 2019",
             "Global friction surface enumerating land-based travel walking-only speed without access to motorized transport for a nominal year 2019",
             "Global travel time to healthcare map with access to motorized transport")
rasters <- setNames(as.list(rasters), 
         c("Explorer__2020_walking_only_travel_time_to_healthcare",
         "Explorer__2020_motorized_friction_surface",
         "Explorer__2020_walking_only_friction_surface",
         "Explorer__2020_motorized_travel_time_to_healthcare")
)

travel_ft_map <- function(
    iso3c,
    rasters,
    year = 2020) {
  
  # Get Raster Maps
    get_map_rasters <- function(iso3c, year, rasters) {
      pop <- cart::get_pop(iso3c = iso3c, year = year)
      pop_extent <- cart:::get_extent(pop)
      spatial_limits <- list()
      for (i in seq_along(rasters)) {
        spatial_limits[[i]] <- malariaAtlas::getRaster(surface = rasters[[i]], 
                                                       extent = pop_extent)
        names(spatial_limits[[i]]) <- names(rasters)[i]
      }
      spatial_limits <- lapply(spatial_limits, function(x, y) {
        if (all.equal(terra::res(x), terra::res(y))) {
          x <- terra::resample(x, y, method = "near")
        }
        return(x)
      }, y = pop)
      
      res <- terra::rast(c(list(pop), spatial_limits))
      return(res)
    }
    
    # get it and extract to the spatial map
    data <- get_map_rasters(iso3c, year, rasters)
    extracted_data <- cart::unpack_cart(admin1 %>% filter(iso == iso3c), data)
    
    summary_data <- extracted_data %>%
      dplyr::select(id_1, pop, names(rasters)) %>%
      tidyr::unnest(cols = c(pop, names(rasters))) %>%
      tidyr::drop_na() %>%
      dplyr::group_by(id_1) %>%
      dplyr::summarise(across(names(rasters), function(x){round(weighted.mean(x, pop, na.rm = TRUE), 3)}),
                       pop_total = sum(pop))
    
    fc <- admin1 %>% sf::st_drop_geometry() %>% 
      select(iso, id_0, name_0, id_1, name_1) %>% 
      filter(iso == iso3c) %>% 
      left_join(summary_data) %>%
      mutate(year = year, .before = names(rasters)[1]) %>% 
      rename(iso3c = iso)
    
    return(fc)
    
  }

# which iso3cs is this for
admin1 <- readRDS("analysis/data-derived/admin1_sf.rds")
iso3cs <- unique(admin1$iso)

# create our travel list
travel_list <- vector("list", length(iso3cs))
for(i in seq_along(travel_list)) {
  message(i)
  travel_list[[i]] <- travel_ft_map(iso3cs[i], rasters)
}

# save to file
saveRDS(travel_list, "analysis/data-derived/travel_list.rds")
all_travel <- do.call(rbind, travel_list)

# ---------------------------------------------------- #
# 2. Get the DHS ft values into our MAP map ----
# ---------------------------------------------------- #

# read in dhs data
shp <- sf::read_sf("analysis/data-raw/data_dhs/shps/sdr_subnational_data.shp")
shp$iso3c <- countrycode::countrycode(shp$CNTRYNAMEE, origin = "country.name.en","iso3c")

# Get admin 1 map
admin1 <- readRDS("analysis/data-derived/admin1_sf.rds")

# start matching it to our MAP map
iso3cs <- unique(shp$iso3c)
iso3cs <- iso3cs[iso3cs %in% admin1$iso]
map <- admin1
map$ft <- NA
ids <- map$id_1[map$iso %in% iso3cs]

# loop through and work out ft where we have it
sf::sf_use_s2(FALSE)
for(i in seq_along(ids)){
  
  id_i <- ids[i]
  poly <- map %>% filter(id_1 == id_i)
  centre <- sf::st_centroid(poly)
  shp_polys <- shp %>% filter(iso3c == poly$iso)
  nn <- sf::st_nearest_feature(centre, shp_polys)
  ft_i <- shp_polys[nn,]$MLFEVTCADV
  map$ft[map$id_1 == id_i] <- ft_i
  
}

# set NA to missings or 
map$ft[which(map$ft > 100)] <- NA
map$ft[which(map$ft < 0)] <- NA

# ---------------------------------------------------- #
# 3. Combine these together and model missing ft data ----
# ---------------------------------------------------- #

# bring together
new_df <- map %>% sf::st_drop_geometry() %>% select(iso3c = iso, id_1, ft) %>%
  filter(id_1 != 0) %>% 
  left_join(all_travel %>% sf::st_drop_geometry() %>% select(id_1, pop_total, matches("Explorer")))

# bring in a covariate dataset at the iso3c level from Economist for imputation help
preds <- readRDS("analysis/data-raw/economist_covariates.rds")
preds <- preds %>% filter(date == 18264)
pred_df <- preds %>% 
  select(iso3c, hospital_beds_per_thousand, life_expectancy, 
         vdem_freedom_of_expression_score:polity_democracy_score, 
         wdi_obs_lag:wdi_urban_pop_1m_cities_pct, 
         percent_land_area_in_tropics:total_deaths_latest_per_100k,gdpppc_ppp_imf)

# impute missing predictors
full_df <- new_df %>% left_join(pred_df) %>% ungroup()
mic <- mice::mice(full_df %>% select(-id_1, -iso3c, -ft, -pop_total))
comp <- mice::complete(mic)
full_df[,-which(names(full_df) %in% c("id_1","ft", "iso3c", "pop_total"))] <- comp

# build xgboost for prediction 
library(caret)
library(xgboost)

# split into test train
set.seed(123)
train_indices <- sample(nrow(full_df), nrow(full_df) * 0.75)
train <- full_df[train_indices, ]
test <- full_df[-train_indices, ]

# set up xgboost
xgb_params <- list(objective = "reg:logistic",
                   eta = c(0.05),
                   max_depth = c(12), 
                   subsample = c(0.85), 
                   colsample_bytree = c(0.85))

# Train xgboost model with cross-validation
xgb_cv <- xgb.cv(params = xgb_params,
                 data = as.matrix(train %>% na.omit %>% select(-ft, -iso3c, -id_1)),
                 label = train %>% na.omit %>% mutate(ft = ft/100) %>% pull(ft),
                 nfold = 20, verbose = 0, na.rm = TRUE,
                 nrounds = 200)

# Extract best iteration
best_iter <- which.min(xgb_cv$evaluation_log$test_rmse_mean)

# Train final model using best iteration
xgb_model <- xgboost::xgboost(
  params = xgb_params,
  data = as.matrix(train %>% na.omit %>% select(-ft, -iso3c, -id_1)),
  label = train %>% na.omit %>% mutate(ft = ft/100) %>% pull(ft),
  nrounds = best_iter, verbose = 0)

# Evaluate performance on test set to check looks okay
pred <- predict(xgb_model, as.matrix(test %>% na.omit %>% select(-ft, -iso3c, -id_1)))
rmse <- sqrt(mean(((test %>% na.omit %>% mutate(ft = ft/100) %>% pull(ft)) - pred) ^ 2))
xgboost_gg <- test %>% na.omit %>% mutate(ft = ft/100) %>% mutate(pred = pred) %>% 
  ggplot(aes(pred,ft)) + geom_point() + geom_smooth(method = "lm") + 
  geom_abline(slope = 1, intercept = 0) + 
  coord_equal() + 
  theme_bw(base_size = 14, base_family = "Helvetica") + 
  xlab("Predicted DHS ft") + 
  ylab("Observed DHS ft") +
  coord_flip() +
  ggtitle(paste("RMSE =", format(rmse, digits = 4)))
save_figs("XGBOOST_ft_performance", xgboost_gg, width = 8, height = 6)

# use this to fill the gaps
full_df$ft[which(is.na(full_df$ft))] <- predict(
  xgb_model,
  as.matrix(full_df %>% filter(is.na(ft)) %>% select(-ft, -iso3c, -id_1))
) * 100

# add in the DHS informed fts at national level
ft_dat <- readRDS("analysis/data-derived/ft_2000-2022.rds")
new_ft <- admin1 %>% sf::st_drop_geometry() %>% 
  select(iso3c = iso, id_0, name_0,
         id_1, name_1) %>%
  mutate(year = 2000) %>% 
  group_by(iso3c, id_0, name_0, id_1, name_1) %>% 
  complete(year = 2000:2022) %>% 
  left_join(ft_dat) %>% 
  left_join(full_df %>% select(id_1, ft, pop_total) %>% rename(fs = ft))

# work out the subnational ft
new_ft <- new_ft %>%
  group_by(iso3c, year) %>%
  mutate(ft = ((ft[1]*sum(pop_total))/(sum(fs*pop_total))) * fs) %>%  
  select(-fs) %>%
  select(-pop_total) %>% 
  select(iso3c, name_0, id_0, name_1, id_1, year, ft) %>% 
  as_tibble()

saveRDS(new_ft, "analysis/data-derived/subnational_ft_2000-2022.rds")
