# load library

library(data.table)

# Create dummy datasets of exposed

exposed <- fread("person_id,age,t0,SES,REGION
1,45,340,2,1
2,30,470,2,5
3,45,299,2,1
4,26,341,1,5
5,36,356,2,5
6,21,439,1,9")

# and of candidate matches

candidate_matches <- fread("person_id,start,end,age,SES,REGION
1,1,339,45,2,1
7,1,449,35,2,5
7,450,621,35,3,5
8,1,409,45,2,1
8,410,601,45,3,1
9,1,357,26,1,5
10,1,324,31,2,8
11,1,375,35,2,5
12,1,720,21,1,9
13,1,720,21,1,9
14,1,720,21,1,9
15,98,720,22,1,9
16,1,450,22,1,9
16,451,720,22,1,10")

# assign matching criteria

exact_matching <- c("SES", "REGION")

range_matching <- c("age")

matching_rule_for_range <- list()

matching_rule_for_range[['age']] <- c(1, 1)

batching_variables = c("SES", "REGION", "age")

# compose match string

preamble <- "person_id != i.person_id & vax1_day >= start & vax1_day < end"

for (matching_variable in range_matching){ 
  matching_strings <- paste0( "between(", matching_variable, ", i.", matching_variable, " - ", matching_rule_for_range[[matching_variable]][2], ", i.", matching_variable, " + ", matching_rule_for_range[[matching_variable]][1], ")")
}

matching_string <- paste(preamble, paste(matching_strings, collapse = " & "), sep = " & ")
string_for_batches <- paste(matching_strings, collapse = " & ")

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

# Set threshold

threshold <- 5

# We want to group the integers in such a way that the sum of all the elements of each group is smaller than the threshold, and the number of groups is minimal
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