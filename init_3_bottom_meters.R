rm(list = ls())

library(lubridate)
library(gdata)
library(dplyr)

source("config_paths.R")
source("config_datasplit.R")
load(file.path(output.folder, "myinfo.Rdata"))

# We replace the following special days by (nearby) non-special days
holidays_toprocess <- c("2009-5-04", "2009-5-25", "2009-8-31", "2009-12-25", 
                        "2009-12-28", "2010-1-01", "2010-4-2", "2010-4-5", "2010-5-3", "2010-5-31")

index_final <- seq(length(seq_complete_interval))

for(i in seq_along(holidays_toprocess)){
  
  target <- ymd(holidays_toprocess[i])
  
  idhours <- which(year(seq_complete_interval) == year(target) & 
                     month(seq_complete_interval) ==  month(target) & 
                     day(seq_complete_interval) == day(target))
  
  stopifnot(length(idhours) == 48)
  
  if(i %in% c(1, 2, 3, 8, 9, 10)){
    id <- 48
  }else if(i %in% c(4, 6, 7)){
    id <- -48
  }else if(i == 5){
    id <- -48*2
  }
  #1. Replace mon by tue \\ +48
  #2. Replace mon by tue \\ +48
  #3. Replace mon by tue \\ +48
  #4. 24/12 replaced by 23nd \\ -48
  #5. 25/12 replaced by 23nd \\ -48 *2
  #6. 1/1   replaced by 31/12 \\ -48
  #7. 24/4  Fri by Thu \\ -48
  #8. 5/4   Mon by Tue \\ +48
  #9. 3/5    same \\ +48
  #10. 31/5  same \\ +48
  index_final[idhours] <- index_final[idhours + id]	
}

for(idseries in bottomSeries){
  print(idseries)
  ###
  infoMeter <- myinfoDT %>% filter(IDMETER == idseries) %>% select(firstAdvance, lastAdvance) 
  firstAdvance <- infoMeter %>% .$firstAdvance
  lastAdvance  <- infoMeter %>% .$lastAdvance
  alldates <- seq(firstAdvance, lastAdvance, by = "30 min")
  ids <- match(seq_complete_interval, alldates) 
  stopifnot(all(!is.na(ids)))
  
  load(file.path(init_meters.folder, paste("meter-", idseries, ".Rdata", sep = "")))
  obs <- dataset[ids,] %>% .$ELECKWH
  npoints <- length(obs)
  ###
  
  # Filling missing observations
  id_na <- which(is.na(obs))
  if(length(id_na) > 0){
    
    for(id in id_na){
      
      idrepl <- id + c(-48 *2, - 48, -2, -1, 1, 2, 48, 48 * 2)
      
      idok <- idrepl[which(idrepl <= npoints & idrepl > 0)]
      stopifnot(length(idok) > 0)
      
      repl <- obs[idok]
      repl <- repl[which(!is.na(repl))]
      stopifnot(length(repl) > 0)
      
      val <- median(repl)
      obs[id] <- val
    }
  }
  
  # Replacing special days
  obs <- obs[index_final]
  demand <- obs
  
  save(file = file.path(bottom_meters.folder, paste("mymeter-", idseries, ".Rdata", sep = "")) , list = c("demand"))
}
