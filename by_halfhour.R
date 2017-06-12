rm(list = ls())
source("config.R")
source("functions.R")

load(file.path(output.folder, "myinfo.Rdata"))

ntest <- length(test$id)

QF_bottom <- vector("list", length(bottomSeries))
QF_agg <- vector("list", length(aggSeries))

obs_agg    <- revisedmean_agg    <- mean_agg <- matrix(NA, nrow =  length(aggSeries), ncol = ntest)
obs_bottom <- revisedmean_bottom <- mean_bottom <- matrix(NA, nrow =  length(bottomSeries), ncol = ntest)

for(do.agg in c(TRUE, FALSE)){
  
  if(do.agg){
    set_series <- aggSeries
    algo <- algo.agg
  }else{
    set_series <- bottomSeries
    algo <- algo.bottom
  }
  
  for(j in seq_along(set_series)){
   # if(j%%100 == 0)
    print(j)
    
    if(do.agg){
      QF_agg[[j]] <- matrix(NA, nrow = length(taus), ncol = ntest)
    }else{
      QF_bottom[[j]] <- matrix(NA, nrow = length(taus), ncol = ntest)
    }
    
    idseries <- set_series[j]
    
    load(file.path(base_forecasts.folder, algo, paste("results_", idseries, "_", algo, ".Rdata", sep = "")))

    if(do.agg){
      load(file.path(aggregated_meters.folder, paste("series-", idseries, ".Rdata", sep = "")))
      obs_agg[j, ] <- demand[test$id]
      mean_agg[j, ] <- unlist(all_mf)
    }else{
      load(file.path(bottom_meters.folder, paste("mymeter-", idseries, ".Rdata", sep = "")))
      obs_bottom[j, ] <- demand[test$id]
      mean_bottom[j, ] <- unlist(all_mf)
    }
    
    
    for(idtest in seq(ntest)){
      iday <- getInfo(idtest)$iday
      hour <- getInfo(idtest)$hour
      
      if(do.agg){
        QF_agg[[j]][, idtest] <- all_qf[[iday]][, hour]
      }else{
        QF_bottom[[j]][, idtest] <- all_qf[[iday]][, hour]
      }
    }# idtest
  }# series
}# AGG and BOTTOM

for(idtest in seq(ntest)){
  print(idtest)
  by_halfhour_file <- file.path(by_halfhour.folder, paste("results_by_halfhour_", algo.agg, "_", algo.bottom, "_", idtest, ".Rdata", sep = ""))
  
  QF_agg_idtest <- sapply(seq(length(aggSeries)), function(j){
    QF_agg[[j]][, idtest]
  })
  
  QF_bottom_idtest <- sapply(seq(length(bottomSeries)), function(j){
    QF_bottom[[j]][, idtest]
  })
  
  obs_agg_idtest <- obs_agg[, idtest]
  obs_bottom_idtest <- obs_bottom[, idtest]
  
  mean_bottom_idtest <- mean_bottom[, idtest]
  mean_agg_idtest    <- mean_agg[, idtest]

  save(file = by_halfhour_file, list = c("QF_agg_idtest", "QF_bottom_idtest", 
                                          "obs_agg_idtest", "obs_bottom_idtest",
                                          "mean_agg_idtest", "mean_bottom_idtest"))    
}
