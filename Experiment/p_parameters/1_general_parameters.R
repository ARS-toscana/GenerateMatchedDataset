# Load packages
list.of.packages <- c("data.table", "qs")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
invisible(lapply(list.of.packages, require, character.only = T))

# Clean environment
rm(new.packages, list.of.packages, set_and_create_dir)
