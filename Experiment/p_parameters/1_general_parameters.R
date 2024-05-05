
# set directories

set_and_create_dir <- function(x) {
  x <- paste0(thisdir, x)
  dir.create(file.path(x), showWarnings = F)
  return(x)
}

# set other directories
dirtemp <- set_and_create_dir("/g_intermediate/")
dirresults <- set_and_create_dir("/g_results/")
dirdatasets <- set_and_create_dir("/g_datasets/")

# load packages
read_library <- function(...) {
  x <- c(...)
  invisible(lapply(x, library, character.only = TRUE))
}

list.of.packages <- c("MASS", "haven", "tidyverse", "lubridate",  "stringr", "purrr", "readr", "dplyr", "rmarkdown", "ggplot2", "data.table", "qpdf", "parallel", "readxl", "gtsummary", "labelled", "huxtable", "metafor", "markdown", "R.utils", "RcppAlgos", "qs")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
invisible(lapply(list.of.packages, require, character.only = T))

rm(read_library, new.packages, list.of.packages)
