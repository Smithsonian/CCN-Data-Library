## CCRCN Data Library
# contact: klingesd@si.edu

# This script schedules the library growth script to run once every month on 
#   the first day of the month

## Workspace prep ##############
library(taskscheduleR)

plot_script <- system.file("scripts/library_growth", "library_growth.R",
                           package = getwd())

taskscheduler_create(taskname = "generate_plot", 
                       rscript = paste0(getwd(), "/scripts/library_growth.R"),
                     schedule = "MONTHLY",
                     starttime = format(Sys.time() + 10, "%H:%M"))


myscript <- system.file("extdata", "helloworld.R", package = "taskscheduleR")


taskscheduler_delete(taskname = "generate_plot")
