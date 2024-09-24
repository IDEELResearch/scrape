library(tidyverse)

# ---------------------------------------------------- #
# 1. Function to pull from DHS API and bind with DHS shapes ----
# ---------------------------------------------------- #

get_cId_indicators_with_shp <- function(countryIds, indicatorIds = "ML_FEVT_C_ADV") {
  
  api_data <- rdhs::dhs_data(countryIds, indicatorIds = indicatorIds, breakdown = "subnational")
  
  ## safe submission
  try_fail_catch <- function(expr, attempts = 3){
    r <- NULL
    attempt <- 1
    while( is.null(r) && attempt <= 3 ) {
      attempt <- attempt + 1
      try(
        r <- eval(expr)
      )
    }
    if(!is.null(r)){
      r
    } else {
      stop("Error")
    }
    
  }
  
  # get the accompanyin shape files
  sids <- unique(api_data$SurveyId)
  shps <- lapply(sids, function(x){
    try_fail_catch(rdhs::download_boundaries(surveyId = x))
  })
  names(shps) <- sids
  
  # make our shapes with results list
  res_list <- list()
  for(i in seq_along(sids)){
    if(length(shps[[i]]) != 0){
      if(sids[i] == "SN2020MIS") {
        shps[[i]]$sdr_subnational_boundaries <- shps[[i]]$sdr_subnational_boundaries[1:4,]
      }
      shps[[i]]$sdr_subnational_boundaries$CharacteristicId <- as.integer(str_sub(shps[[i]]$sdr_subnational_boundaries$REG_ID, -6))
      
      if(!all(is.na(shps[[i]]$sdr_subnational_boundaries$CharacteristicId))){
        res_list[[i]] <- left_join(
          shps[[i]]$sdr_subnational_boundaries, 
          api_data %>% filter(SurveyId == sids[i]) %>% select(Indicator, IndicatorId, CharacteristicId, Value) 
        ) %>% 
          filter(!is.na(CharacteristicId))
        
      }
    }
    
  }
  
  # return res
  names(res_list) <- sids[which(unlist(lapply(res_list, length))>0)]
  return(res_list)
  
}
save_cid_f <- purrr::safely(.f = get_cId_indicators_with_shp)

# ---------------------------------------------------- #
# 2. Pull from DHS ----
# ---------------------------------------------------- #

# get african ccs
countries <- rdhs::dhs_countries()
ccs <- countries$DHS_CountryCode[grep("Africa", countries$SubregionName)]

# set up our results
res_list <- vector("list", length = length(ccs))

# get our results
for(i in seq_along(res_list)) {
  message(i)
  res_list[[i]] <- save_cid_f(ccs[i])
}

# ---------------------------------------------------- #
# 3. Check and Save ----
# ---------------------------------------------------- #

# which errored
# of these we can check from StatCompiler that these should not have passed anyway
errored <- which(unlist(lapply(res_list, function(x){length(x$result)})) == 0)

# get those that have not errored
names(res_list) <- countries$ISO3_CountryCode[grep("Africa", countries$SubregionName)]
res_list <- res_list[-errored]

saveRDS(res_list, "../../git/scrape/analysis/data-derived/dhs_ML_FEVT_C_ADV_shps.rds")
