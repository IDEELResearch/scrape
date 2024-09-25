# Just explaining how to do this if we ever want to

# So far we fit the predicted treatment seeking rate in the latest DHS at admin 1 
# using MAP accessibility and travel time rasters, which areonly available in 2020. 

# DHS treatment seeking shapes are available over time. 

# For each time point, we could bring in other longitudinal covariates over time
# and then for each DHS shape and year, we could fit a nodel to predict treatment
# seeking rates at admin 1. 

# Then we would have admin 1 effective treatment seeking (by waiting by prevalence
# as we do in 03_subnational_ft) for a subset of years, from which we could interpolate
# to get the over time admin 1 values.

# This approach is so so as it still assumes travel time maps that are only suitable
# for 2020 but perhaps we could also bring in night time lights and vegetation maps
# as well over time to give more realism? Or build a pre model to predict travel times
# based on night time lights and vegetation over time. And then from this just use
# the travel time maps over time. 
