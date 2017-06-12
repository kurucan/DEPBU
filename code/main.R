rm(list = ls())
source("config.R")

## Preprocessing and creating all Rdata files (SLOW)
source("init_1_raw_meters.R")
source("init_2_myHierarchy.R")
source("init_3_bottom_meters.R")
source("init_4_aggregated_meters.R")

## Run the base forecasts (SLOW)
# (option 1) Run for all bottom and aggregate series (takes time)
# system(paste("Rscript base_forecasts.R KD-IC-NML FALSE ", paste(seq(n_bottom), collapse = " "), sep= ""))
# system(paste("Rscript base_forecasts.R DETS      TRUE  ", paste(seq(n_agg), collapse = " "), sep= ""))

# (option 2) In a multi-core environment, you can choose the number of jobs + the number of series per job
system("./run_base_forecasts.sh bottom    32 50") # 32 jobs with 50 series
system("./run_base_forecasts.sh aggregate 32 2")  # 32 jobs with 2 series

## W matrix for MinT (FAST)
source("MinT_ecov.R")

## Empirical copula (FAST)
source("permutations.R")

## Merging the results by half hour (FAST)
source("by_halfhour.R")

## Computing the aggregate forecasts using permutations (SLOW)
source("aggregation.R")

## Merging the results (FAST)
source("aggregation_merge.R")

## Computing the figures (FAST)
source("fig_results.R")


