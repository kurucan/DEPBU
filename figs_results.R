source("fig_results_fct.R")
color.agg[match(c("PERMBU-MINT", "NAIVEBU-MINT"), agg_methods)] <- color.agg[match(c("PERMBU", "NAIVEBU"), agg_methods)]

do.skill <- TRUE
do.colors <- TRUE

#grouping_hours <- c(10, 8, 8, 8, 8, 6) 
#grouping_hours <- c(10, 20, 18) 
#grouping_hours <- c(16, 22 ,10)
#grouping_hours <- rep(2, 24)
#myfactor_hours <- rep(seq(length( grouping_hours )), times = grouping_hours)
myfactor_hours <- numeric(48)
myfactor_hours[which(seq(1, 48) %in% c(seq(45, 48), seq(1, 12)))] <- 3
myfactor_hours[which(seq(48) %in% seq(13, 28))] <- 1
myfactor_hours[which(seq(48) %in% seq(29, 44))] <- 2
myfactor_hours <- as.factor(myfactor_hours)
grouping_hours <- table(myfactor_hours)

grouping_series_agg <- c(rep(1, 6), 8, 5, 7, 10, 9, 5, 5)
myfactor_series_agg <- rep(seq(length( grouping_series_agg )), times = grouping_series_agg)

grouping_series_bot <- 526*3
myfactor_series_bot <- rep(seq(length( grouping_series_bot )), times = grouping_series_bot)

measures <- c("CRPS", "CRPS Tails", "MSE")
for(measure in measures){
  print(measure)
  list_mat <- get_mat(measure, do.skill = FALSE)
  
  for(id in 1:2){
    if(measure == "MSE"){
      if(id == 1){
        #algos <- c("BASE", "NAIVEBU")
        algos <- c("NAIVEBU")
      }else{
        #algos <- c("BASE", "MINTdiag", "MINTshrink")
        algos <- c("NAIVEBU", "MINTdiag", "MINTshrink")
      }	
    }else{
      if(id == 1){
        #algos <- c("BASE", "NAIVEBU", "PERMBU")
        algos <- c("NAIVEBU", "PERMBU")
      }else{
        #algos <- c("BASE", "PERMBU-MINT", "NAIVEBU-MINT", "MINTdiag", "MINTshrink")
        algos <- c("PERMBU-MINT", "NAIVEBU-MINT", "MINTdiag", "MINTshrink")
      }
    }
    id_wanted_agg <- match(algos, agg_methods)
    id_wanted_bot <- match(sapply(algos, to_aggname), bot_methods)  
    if(do.colors){
      mycolors <- color.agg[id_wanted_agg]
    }else{
      mycolors <- "black"
    }   
    
    filename <- paste("RESULTS_JASA_", gsub(" ", "", measure, fixed = TRUE), "_", id, "_", ifelse(do.colors, "COLOR", "BLACK"), sep = "")
    savepdf(file.path(figs.folder, filename), height = 5)
    par(mfrow = c(1, 3))
    
    results_bot <- list_mat$res_bot
    results_bot_avg <- apply(results_bot, c(2, 3), function(x){ tapply(x, myfactor_hours, mean) })
    results_bot_avg <- apply(results_bot_avg, c(1, 2), function(x){ tapply(x, myfactor_series_bot, mean) })
    x_nbbot <- 1
    
    results_agg <- list_mat$res_agg
    results_agg <- results_agg[, , node_order]
    results_agg_avg <- apply(results_agg, c(2, 3), function(x){ tapply(x, myfactor_hours, mean) })
    results_agg_avg <- apply(results_agg_avg, c(1, 2), function(x){ tapply(x, myfactor_series_agg, mean) })
    results_agg_avg <- aperm(results_agg_avg, c(2, 3, 1))
    x_nbagg <- tapply(res_info$info_nodes_agg[node_order], myfactor_series_agg, mean)
    
    mse_ylim <- vector("list", 3)
    mse_ylim[[1]] <- NULL #c(-10, 7)
    mse_ylim[[2]] <-  c(-15, 5) #c(-45, 10)
    mse_ylim[[3]] <-  c(-80, 20) #c(-60, 15)
    
    
    for(k in seq( length(grouping_hours) ) ){
      my_ylim <- NULL
      if(measure == "MSE" && k <= 3){
        my_ylim <- mse_ylim[[k]]
      }

      #maink <- paste(tday[range(which(k == myfactor_hours))], collapse = " - ")
      id <- which(myfactor_hours == k)
      if(k == 3){
        id <- shifter(id, 4)
      }
      maink <- paste(tday[c(head(id, 1), tail(id, 1))], collapse = " - ")
      
      i_base <- match("BASE", bot_methods)
      u_bot <- t( (results_bot_avg[k, i_base] - t(results_bot_avg[k, id_wanted_bot]))/results_bot_avg[k, i_base])
      u_bot <- t(u_bot) * 100
      
      i_base <- match("BASE", agg_methods)
      u_agg <- t( (results_agg_avg[k, i_base, ] - t(results_agg_avg[k, id_wanted_agg, ]))/results_agg_avg[k, i_base, ])
      u_agg <- t(u_agg) * 100
      
      if(length(id_wanted_agg) == 1)
        u_agg <- t(u_agg)
      
      u_all <- rbind(u_agg, u_bot)
      x_all <- c(x_nbagg, x_nbbot)
      
      matplot(log10(x_all), u_all, 
              type = 'o', pch = pch.agg[id_wanted_agg],  lty = 1, col = mycolors,
              xlab = expression(Log[10]("number of aggregated meters")), 
              ylab = paste(measure, " skill (%)", sep = ""), main = maink,
              ylim = my_ylim)
      abline(h = 0)
      if(k == 1){
        legend("bottomleft", better_names(agg_methods[id_wanted_agg]), 
               lty = 1, pch = pch.agg[id_wanted_agg], col = mycolors, cex = 1.1)
      }	
      
    } # k 
    dev.off()
  }# id
} # measures

