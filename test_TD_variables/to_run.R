rm(list=ls(all.names=TRUE))


#load packages
if (!require("data.table")) install.packages("data.table")
library(data.table)


#set the directory where the file is saved as the working directory
if (!require("rstudioapi")) install.packages("rstudioapi")
thisdir<-setwd(dirname(rstudioapi::getSourceEditorContext()$path))
thisdir<-setwd(dirname(rstudioapi::getSourceEditorContext()$path))

#load input
cohort <- fread(paste0(thisdir,"/cohort.csv"), sep = ",")
CONTACTS <- fread(paste0(thisdir,"/CONTACTS.csv"), sep = ",")
DIABDIA <- fread(paste0(thisdir,"/DIABDIA.csv"), sep = ",")
DIABMED <- fread(paste0(thisdir,"/DIABMED.csv"), sep = ",")
TD_dataset <- fread(paste0(thisdir,"/TD_dataset.csv"), sep = ",")
