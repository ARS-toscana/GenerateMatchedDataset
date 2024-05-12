# Load packages
list.of.packages <- c("data.table", "qs", "benchmarkme")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
invisible(lapply(list.of.packages, require, character.only = T))

# Clean environment
rm(new.packages, list.of.packages)

# Function to launch step
launch_step <- function(x, print.eval = F) {
  
  if ("folder" %in% ls(envir = .GlobalEnv)) {
    folder <- get("folder", envir = .GlobalEnv)
  } else {
    stop("Folder of the generated parameters is not called 'dirpargen'")
  }
  
  exec_time <- system.time(source(file.path(folder, "p_steps", x), print.eval = print.eval))
  print(exec_time)
  
  rm(list=ls(envir = .GlobalEnv), envir = .GlobalEnv)
  load(file.path(folder, "g_parameters", "parameters.RData"), envir = .GlobalEnv)
  rm(list=ls(), inherits = T)
  
}
