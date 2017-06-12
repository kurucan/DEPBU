source("functions_figs.R")
shifter <- function(x, n = 1) {
  #if (n == 0) x else c(tail(x, -n), head(x, n))
  if (n == 0) x else c(tail(x, n), head(x, -n))
}


get_mat <- function(measure, do.skill){
  if(grepl("CRPS", measure)){ 
    if(measure == "CRPS"){
      iweight <- 1
    }else if(measure == "CRPS Tails"){
      iweight <- 2
    }else if(measure == "CRPS Right tail"){
      iweight <- 4
    }else if(measure == "CRPS Left tail"){
      iweight <- 5
    }
    res_agg <- wcrps_agg_byhour[iweight, , , ]
    res_bot <- wcrps_bot_byhour[iweight, , , ]
  }else if(measure == "MSE"){
    res_agg <- mse_agg_byhour
    res_bot <- mse_bot_byhour
  }else if(measure == "QS"){
    res_agg <- total_qscores_agg
    res_bot <- total_qscores_bot
  }else{
    stop("error")
  }
  
  if(do.skill){
    mat_agg_skill <- sapply(seq_along(agg_methods), function(iaggmethod){
      (res_agg[, match("BASE", agg_methods), ] - res_agg[, iaggmethod, ])/res_agg[, match("BASE", agg_methods), ]
    }, simplify = 'array')
    
    mat_bot_skill <- sapply(seq_along(bot_methods), function(ibotgmethod){
      (res_bot[, match("BASE", bot_methods), ] - res_bot[, ibotgmethod, ])/res_bot[, match("BASE", bot_methods), ]
    }, simplify = 'array')
    
    #browser()
    res_agg <- aperm(mat_agg_skill, c(1, 3, 2))
    res_bot <- aperm(mat_bot_skill, c(1, 3, 2))
  }
  
  list(res_agg = res_agg, res_bot = res_bot)
}

to_aggname <- function(algo){
  if(algo %in% c("NAIVEBU", "PERMBU"))
  {
    res <- "BASE"
  }else if(algo %in% c("PERMBU-MINT", "NAIVEBU-MINT")){
    res <- "MINTshrink"
  }else{
    res <- algo
  } 
  res
}

better_names <- function(x){
  if(measure == "MSE"){
    x[which(x == "NAIVEBU")] <- "IndepBU and DepBU"
  }else if(measure == "CRPS" || measure == "CRPS Tails"){
    x[which(x == "NAIVEBU")] <- "IndepBU"
    x[which(x == "PERMBU")] <- "DepBU"
    x[which(x == "NAIVEBU-MINT")] <- "IndepBU-MinTShrink"
    x[which(x == "PERMBU-MINT")]  <- "DepBU-MinTShrink"
  }
  x[which(x == "MINTdiag")] <- "MinTDiag"
  x[which(x == "MINTshrink")] <- "MinTShrink"
  
  return(x)
}

