##%######################################################%##
#                                                          #
####            Create TD Variables datasets            ####
#                                                          #
##%######################################################%##

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
CONTACTS[, date := data.table::as.IDate(date)]
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

# TD_single_dataset_to_be_completed[, entry_spell_category := as.numeric(entry_spell_category)]
data.table::setnames(TD_single_dataset_to_be_completed, "entry_spell_category", "start_record")

# TD_single_dataset_to_be_completed[, exit_spell_category := as.numeric(exit_spell_category)]
data.table::setnames(TD_single_dataset_to_be_completed, "exit_spell_category", "end_record")

# Put the dataset in the standard format of a single-variable TD dataset to be completed: value of variable and default value 
TD_single_dataset_to_be_completed <- TD_single_dataset_to_be_completed[, CONTACTS7 := 1]
value_default <- 0

combined_df <- cohort[TD_single_dataset_to_be_completed, on = "person_id", all.x = T, nomatch = 0L]

# combined_df <- cohort[TD_single_dataset_to_be_completed, on = "person_id", allow.cartesian = TRUE]
# combined_df <- cohort[TD_single_dataset_to_be_completed, on = "person_id", allow.cartesian = TRUE, nomatch = 0][, .(person_id, CONTACTS7), by = .EACHI]
combined_df <- cohort[, .(person_id)][TD_single_dataset_to_be_completed, on = "person_id", all.x = T]

# Test if euqal to final dataset
all.equal(TD_dataset, combined_df)

# .(key, value, b1, b2, i.a1, i.a2)]


## simple example:
# Create data.tables
TD_variable <- data.table::data.table(person_id = c("P01", "P02", "P03", "P03"), start_date = c(1, 100, 1, 500),
                                      end_date = c(675, 760, 300, 675), value_of_in_study_period = c(1, 1, 1, 1))
D_EVENT <- data.table::data.table(person_id = c("P02", "P03"), date_of_event = c(50, 75))
TD_variable[, value_of_in_study_period := NULL]

# Left join of spells and events
combined_df <- TD_variable[D_EVENT, on = "person_id", date_of_event := i.date_of_event]
# rm(TD_variable, D_EVENT)

# Split the data.table in two
combined_df <- split(combined_df, combined_df[, fifelse(date_of_event > start_date & date_of_event <= end_date, 1, 0, 0)])

# First data.table is trivial
combined_df[[1]][, date_of_event := data.table::fifelse(!is.na(date_of_event) & date_of_event <= start_date, 1, 0)]
data.table::setnames(combined_df[[1]], "date_of_event", "value_of_variable")
# rm(combined_df)

# Create a copy of the second one and then censor the exit and entry
combined_df[[3]] <- copy(combined_df[[2]])[, end_date := date_of_event - 1][, date_of_event := NULL][, value_of_variable := 0]
combined_df[[2]][, start_date := date_of_event][, date_of_event := NULL][, value_of_variable := 1]
# rm(df_between)

# Combine and order the data.tables
final_df <- data.table::rbindlist(combined_df)
# rm(df_trivial, df_between_before, df_between_after)
data.table::setorder(final_df, "person_id", "start_date")

