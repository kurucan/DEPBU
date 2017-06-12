main.folder   <- "/home/rstudio/JASA"

data.folder   <- file.path(main.folder, "data")
code.folder   <- file.path(main.folder, "code")
figs.folder   <- file.path(main.folder, "figs")
output.folder <- file.path(main.folder, "output")

init_meters.folder       <- file.path(output.folder, "meter_data", "init_meters")
init_meters.folder       <- "/home/rstudio/PROJ/procdata/initmeters"
bottom_meters.folder     <- file.path(output.folder, "meter_data", "bottom_meters")
aggregated_meters.folder <- file.path(output.folder, "meter_data", "aggregated_meters")

base_forecasts.folder <- file.path(output.folder, "base_forecasts")
error_measures.folder <- file.path(output.folder, "error_measures")
in_sample.folder      <- file.path(output.folder, "in_sample")
permutations.folder   <- file.path(output.folder, "permutations")
by_halfhour.folder   <- file.path(output.folder, "by_halfhour")
temp.folder   <- file.path(output.folder, "temp")

# Create the folders if they do not exist already
x <- c(init_meters.folder, bottom_meters.folder, aggregated_meters.folder,
       base_forecasts.folder,error_measures.folder, in_sample.folder, permutations.folder, by_halfhour.folder, temp.folder)
sapply(x, dir.create, showWarnings = FALSE, recursive = TRUE)

#dir.create(init_meters.folder, showWarnings = FALSE, recursive = TRUE)
#dir.create(bottom_meters.folder, showWarnings = FALSE, recursive = TRUE)
#dir.create(aggregated_meters.folder, showWarnings = FALSE, recursive = TRUE)
#dir.create(base_forecasts.folder, showWarnings = FALSE, recursive = TRUE)
#dir.create(error_measures.folder, showWarnings = FALSE, recursive = TRUE)
#dir.create(in_sample.folder, showWarnings = FALSE, recursive = TRUE)
#dir.create(permutations.folder, showWarnings = FALSE, recursive = TRUE)
#dir.create(temp.folder, showWarnings = FALSE, recursive = TRUE)

