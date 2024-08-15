clean_parameters <- function() {
  exposed <- checkmate::assert_data_frame(exposed, any.missing = F, min.rows = 1, min.cols = 1)
  candidate_matches <- checkmate::assert_data_frame(candidate_matches, any.missing = F, min.rows = 1, min.cols = 1)
  # TODO add assertion to check unit_of_observation values inside columns names of exposed and candidate_matches
  unit_of_observation <- checkmate::checkAtomicVector(unit_of_observation, any.missing = F, min.len = 1, unique = T)

  type_of_matching
  # TODO add assertion to check time_variable values inside columns names of exposed and candidate_matches
  time_variable_in_exposed <- checkmate::checkAtomicVector(time_variable_in_exposed, any.missing = F, len = 1, unique = T)
  time_variables_in_candidate_matches <- checkmate::checkAtomicVector(time_variables_in_candidate_matches, any.missing = F, len = 1, unique = T)
  variables_with_exact_matching <- checkmate::checkAtomicVector(variables_with_exact_matching, any.missing = F, min.len = 1, unique = T)
  variables_with_range_matching <- checkmate::checkAtomicVector(variables_with_range_matching, any.missing = F, min.len = 1, unique = T)

  range_of_variables_with_range_matching = NULL,
  additional_matching_rules = NULL,
  rule_for_matching_on_dates = NULL,
  output_matching = NULL,
  seeds_for_sampling <- assert_count(seeds_for_sampling, coerce = T, null.ok = T)
  sample_size_per_exposed <- assert_count(sample_size_per_exposed, positive = T, coerce = T)
  methodology_for_bootstrapping <- assert_choice(methodology_for_bootstrapping,
                                                 c("No bootstrapping", "Sample exposed", "Sample units of observations"))
  number_of_bootstrapping_samples <- assert_count(number_of_bootstrapping_samples, coerce = T)
  type_of_sampling <- assert_choice(type_of_sampling, c("with replacement", "without replacement"))
  exclude_sameUoO <- assert_flag(exclude_sameUoO)
  algorithm_for_matching <- assert_choice(type_of_sampling, c("naive", "with threshold"))
  threshold <- assert_count(threshold, coerce = T, null.ok = T)
  technical_details_of_matching = NULL,
  temporary_folder = NULL,
  exclude_columns <- assert_flag(exclude_columns)
}

group_integers <- function(values, threshold) {

  names(values) <- 1:length(values)
  # values <- sort(values, decreasing = TRUE) # Sort values in descending order

  groups_idx <- list(list("sum" = values[[names(values)[[1]]]], "indices" = c(names(values)[[1]])))

  for (val in names(values)[2:length(names(values))]) {

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

AndDiff = function(cond){
  Reduce(
    function(x, y) call("&", x, y),
    lapply(cond, function(var) call("!=", var, paste0("i.", var)))
  )
}
