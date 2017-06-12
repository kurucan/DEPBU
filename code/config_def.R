

hour    <- rep(c("00", "01", "02", "03", "04", "05", "06", "07", "08", "09", "10", 
                 "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23"), each = 2)

minutes <- rep(c("00", "30"), 24)

tday <- paste(hour, minutes, sep=":")

abbr.dweek <- c("Mon","Tue","Wed","Thu","Fri", "Sat","Sun")

hours_night <- night_hours <-  c(seq(1, 12), 46, 47, 48)
hours_day   <- day_hours <- setdiff(seq(1, 48), hours_night)
hours_all <- c(hours_night, hours_day)
index_hours <- sort(hours_all, index = T)$ix
