rm(list=ls(all.names=TRUE))

#set the directory where the script is saved as the working directory
if (!require("rstudioapi")) install.packages("rstudioapi")
thisdir <- setwd(dirname(rstudioapi::getSourceEditorContext()$path))
thisdir <- setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# load packages
if (!require("data.table")) install.packages("data.table")
library(data.table)

# Store in a variable the size of the dataset
df_size <- 1000

# names of the datasets of exposed and candidate_matches, and of the matched
name_dataset_exposed <- paste0("exposed_", as.character(df_size))
name_dataset_candidate_matches <- paste0("candidate_matches_", as.character(df_size))

threshold <- 500  # Set your threshold for batches
name_dataset_matched <- paste0("matched", as.character(df_size), "_in_batches_with_threshold", as.character(df_size))

# assign matching criteria

exact_matching <- c("SES", "REGION")

range_matching <- c("age")

matching_rule_for_range <- list()

matching_rule_for_range[['age']] <- c(1, 1)

batching_variables = c("SES", "REGION", "age")


#-----------------------------
# START MATCHING IN BATCHES
# load datasets

exposed <- fread(paste0(thisdir, "/", name_dataset_exposed, ".csv") )
candidate_matches <- fread(paste0(thisdir, "/", name_dataset_candidate_matches, ".csv") )

# compose match string

preamble <- "person_id != i.person_id & vax1_day >= start & vax1_day < end"

for (matching_variable in range_matching){ 
  matching_strings <- paste0( "between(", matching_variable, ", i.", matching_variable, " - ", matching_rule_for_range[[matching_variable]][2], ", i.", matching_variable, " + ", matching_rule_for_range[[matching_variable]][1], ")")
}

matching_string <- paste(preamble, paste(matching_strings, collapse = " & "), sep = " & ")
string_for_batches <- paste(matching_strings, collapse = " & ")

print(matching_string)

# CREATE BATCHES

# Create new datatables with unique combinations and frequencies

unique_combinations_exposed <- exposed[, .(frequency_exposed = .N), by = batching_variables]
unique_combinations_cm <- candidate_matches[, .(frequency_cm = .N), by = batching_variables]

# merge the two datatables of combinations
parsed_matching_string <- parse(text = string_for_batches)
matched_combinations <- unique_combinations_exposed[unique_combinations_cm, allow.cartesian = TRUE,
                                                    on = exact_matching][eval(parsed_matching_string)]

frequencies_of_matched_combinations <- matched_combinations[, frequency_record := frequency_exposed * frequency_cm]
frequencies_of_matched_combinations <- frequencies_of_matched_combinations[, .(frequency_combination = sum(frequency_record)), by = batching_variables]

# now i want to know what is the best algorithm to execute a task and how to implement it in R. the algorithm should take a integer parameter that has the role of a threshold, and a list of integers all smaller or equal to the threshold. i want to group the integers in such a way that the sum of all the elements of each group is smaller than the threshold, and the number of groups is minimal
# The algorithm implemented right now is First-fit-decreasing (FFD). Worst case instance is FFD(1) = 11/9 * OPT(I) + 6/9

group_integers <- function(values, threshold) {
  names(values) <- 1:length(values)
  values <- sort(values, decreasing = TRUE)  # Sort values in descending order
  
  groups_idx <- list(list("sum" = 0, "indices" = c()))
  
  for (val in names(values)) {
    
    which_less_thr <- which(sapply(groups_idx, `[[`, 1) + values[[val]] <= threshold)
    
    if (identical(which_less_thr, integer(0))) {
      groups_idx <- append(groups_idx, list(list("sum" = values[[val]], "indices" = c(val))))
    } else {
      idx <- min(which_less_thr)
      groups_idx[[idx]][["indices"]] <- c(groups_idx[[idx]][["indices"]], val)
      groups_idx[[idx]][["sum"]] <- groups_idx[[idx]][["sum"]] + values[[val]]
    }
  }
  
  return(groups_idx)
}

# Function to assign groups
assign_groups <- function(values, threshold) {
  groups <- group_integers(values, threshold)
  
  group_assignment <- rep(1:length(groups), sapply(lapply(groups, `[[`, 2), length))
  names(group_assignment) <- unlist(lapply(groups, `[[`, 2))
  group_assignment <- setNames(as.integer(names(group_assignment)), group_assignment)
  group_assignment <- sort(group_assignment)
  group_assignment <- setNames(as.integer(names(group_assignment)), NULL)
  
  return(group_assignment)
}

# Apply the function to create the variable 'batch_number'
batch_numbers_from_unfrequent_combinations <- frequencies_of_matched_combinations[frequency_combination <= threshold, ][, batch_number := assign_groups(frequency_combination, threshold)]

maxbatch_number <- max(batch_numbers_from_unfrequent_combinations[, batch_number])

batch_numbers_from_frequent_combinations <- frequencies_of_matched_combinations[frequency_combination > threshold, ][ , batch_number := maxbatch_number + 1:.N]

batches <- rbind(batch_numbers_from_unfrequent_combinations, batch_numbers_from_frequent_combinations)

list_of_batches <- sort(unique(batches[, batch_number]))

# match in batches

exposed <- batches[exposed, on = batching_variables]

number_of_batches <- length(list_of_batches)

for (batch in list_of_batches){
  print(paste("matching batch", batch, "of", number_of_batches))
  exposedbatch <- exposed[batch_number == batch, ]
  # TODO: select records of candidate_matches whose values of the matching variables correspond in matched_combinations to the values of  -batch- in the exposed dataset
  # values_of_candidate_matched_variables <- matched_combinations[SELECT VALUES OF MATCHING VARIABLES CORRESPONDING TO batch,]
  candidate_matchesbatch <- candidate_matches
  parsed_matching_string <- parse(text = matching_string)
  dataset_matched <- exposedbatch[candidate_matchesbatch, allow.cartesian = TRUE, on = exact_matching][eval(parsed_matching_string)]
  
  # sort
  setkey(dataset_matched, person_id, i.person_id)
  
  #reorder
  column_order <- c("person_id", "i.person_id", "vax1_day", "start", "end" )
  column_order <- union(column_order, names(dataset_matched))
  dataset_matched <- dataset_matched[, ..column_order]
  
  # save the datasets in csv format
  write.csv(dataset_matched, paste0(thisdir, "/", name_dataset_matched, "_", batch, ".csv"), row.names = FALSE)
  
}


#####################################################

