rm(list=ls(all.names=TRUE))


#set the directory where the script is saved as the working directory
if (!require("rstudioapi")) install.packages("rstudioapi")
thisdir <- setwd(dirname(rstudioapi::getSourceEditorContext()$path))
thisdir <- setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# load packages
if (!require("data.table")) install.packages("data.table")
library(data.table)
if (!require("dplyr")) install.packages("dplyr")
library(dplyr)


# Store in a variable the size of the dataset
df_size <- 1000

# names of the datasets of exposed and candidate_matches, and of the matched
name_dataset_exposed <- "exposed_1000"
name_dataset_candidate_matches <- "candidate_matches_1000"

# load datasets

exposed <- fread(paste0(thisdir,"/",name_dataset_exposed,".csv") )
candidate_matches <- fread(paste0(thisdir,"/",name_dataset_candidate_matches,".csv") )
