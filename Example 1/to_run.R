#load packages
if (!require("data.table")) install.packages("data.table")
library(data.table)

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
