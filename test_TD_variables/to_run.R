rm(list=ls(all.names=TRUE))


# load packages
if (!require("data.table")) install.packages("data.table")
library(data.table)
if (!require("lubridate")) install.packages("lubridate")
library(lubridate)


# set the directory where the file is saved as the working directory
if (!require("rstudioapi")) install.packages("rstudioapi")
thisdir<-setwd(dirname(rstudioapi::getSourceEditorContext()$path))
thisdir<-setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# load functions

source(paste0(thisdir,"/CreateSpells_v15.R"))

# load input
cohort <- fread(paste0(thisdir,"/cohort.csv"), sep = ",")
CONTACTS <- fread(paste0(thisdir,"/CONTACTS.csv"), sep = ",")
DIABDIA <- fread(paste0(thisdir,"/DIABDIA.csv"), sep = ",")
DIABMED <- fread(paste0(thisdir,"/DIABMED.csv"), sep = ",")

# load output to be verified
TD_dataset <- fread(paste0(thisdir,"/TD_dataset.csv"), sep = ",")

#===========================================
# ARS' procedure

# 1: create the TD variable of CONTACTS7: at least one contact in the past 7 days

# each record lasts 7 days, so its end date is date + 7

CONTACTS$date <- as_date(CONTACTS$date, origin = lubridate::origin)
data.table::setnames(CONTACTS, "date", "start_record")
CONTACTS <- CONTACTS[, end_record := start_record + 7]

# apply CreateSpells to create unique disjoint spells of the variable

TD_single_dataset_to_be_completed <- CreateSpells(
  dataset = CONTACTS,
  id = "person_id" ,
  start_date = "start_record",
  end_date = "end_record",
  quiet = T
)
TD_single_dataset_to_be_completed$entry_spell_category <- as.numeric(TD_single_dataset_to_be_completed$entry_spell_category, origin = lubridate::origin)
data.table::setnames(TD_single_dataset_to_be_completed, "entry_spell_category", "start_record")

TD_single_dataset_to_be_completed$exit_spell_category <- as.numeric(TD_single_dataset_to_be_completed$exit_spell_category, origin = lubridate::origin)
data.table::setnames(TD_single_dataset_to_be_completed, "exit_spell_category", "end_record")


# put the dataset in the standard format of a single-variable TD dataset to be completed: value of variable and default value 
TD_single_dataset_to_be_completed <- TD_single_dataset_to_be_completed[, CONTACTS7 := 1]
value_default <- 0

combined_df <- cohort[TD_single_dataset_to_be_completed, on = "person_id", allow.cartesian = TRUE, nomatch = 0L]


#combined_df <- cohort[TD_single_dataset_to_be_completed, on = "person_id", allow.cartesian = TRUE]
# combined_df <- cohort[TD_single_dataset_to_be_completed, on = "person_id", allow.cartesian = TRUE, nomatch = 0][, .(person_id, CONTACTS7), by = .EACHI]
combined_df <- cohort[, .(person_id)][TD_single_dataset_to_be_completed, on = "person_id", allow.cartesian = TRUE]



# .(key, value, b1, b2, i.a1, i.a2)]
