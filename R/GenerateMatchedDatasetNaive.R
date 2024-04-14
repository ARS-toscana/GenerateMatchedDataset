GenerateMatchedDatasetNaive <- function(exposed,
                                        candidate_matches,
                                        unit_of_observation,
                                        type_of_matching = 'on variables',
                                        time_variable_in_exposed = NULL,
                                        time_variables_in_candidate_matches = NULL,
                                        variables_with_exact_matching = NULL,
                                        variables_with_range_matching = NULL,
                                        range_of_variables_with_range_matching = NULL,
                                        additional_matching_rules = NULL,
                                        rule_for_matching_on_dates = NULL,
                                        output_matching = NULL,
                                        seeds_for_sampling = NULL,
                                        sample_size_per_exposed = 1,
                                        methodology_for_bootstrapping = NULL,
                                        number_of_bootstrapping_samples = 100,
                                        type_of_sampling = NULL,
                                        exclude_sameUoO = NULL,
                                        algorithm_for_matching = NULL,
                                        threshold = NULL,
                                        technical_details_of_matching = NULL,
                                        temporary_folder = NULL) {
  
  # preamble <- "person_id != i.person_id & vax1_day >= start & vax1_day < end"
  # TODO Transform UoO to integer
  # TODO Add a join key parameter
  
  # Get number of threads to use for qs
  data.table_threads <- data.table::getDTthreads()
  
  names(variables_with_range_matching) <- variables_with_range_matching
  exact_strata_col <- if (missing(variables_with_exact_matching)) {
    stop("Please specify a variable for exact matching")} else variables_with_exact_matching
  
  # Remove columns not used during the matching and not primary keys
  # TODO write routine to minimize number of hash tables
  # TODO to prevent different number of rows create if possible single table from entire population
  excl_cols_exp <- setdiff(colnames(exposed), c(variables_with_exact_matching, variables_with_range_matching,
                                                unit_of_observation, time_variable_in_exposed,
                                                time_variables_in_candidate_matches))
  excl_cols_cand <- setdiff(colnames(candidate_matches), c(variables_with_exact_matching, variables_with_range_matching,
                                                           unit_of_observation, time_variables_in_candidate_matches,
                                                           time_variable_in_exposed))
  
  # Define a lower and upper boundaries for variables with ranges
  # In exposed create variables for the boundaries
  # Define the set of rules to be used during matching
  list_simple_ranges_rules <- list()
  if (!is.null(variables_with_range_matching)) {
    # TODO check names usage here
    lower_boundaries <- unlist(range_of_variables_with_range_matching)[c(TRUE, FALSE)]
    names(lower_boundaries) <- paste0("lower_interval_", names(variables_with_range_matching))
    upper_boundaries <- unlist(range_of_variables_with_range_matching)[c(FALSE, TRUE)]
    names(upper_boundaries) <- paste0("upper_interval_", names(variables_with_range_matching))
    data.table::setnames(exposed, variables_with_range_matching, names(lower_boundaries))
    
    list_simple_ranges_rules <- list(paste0(names(lower_boundaries), " <= ", variables_with_range_matching),
                                     paste0(names(upper_boundaries), " >= ", variables_with_range_matching))
    
    exposed[, (names(lower_boundaries)) := Map(`+`, .SD, lower_boundaries), .SDcols = names(lower_boundaries)]
    exposed[, (names(upper_boundaries)) := Map(`+`, .SD, upper_boundaries - lower_boundaries),
            .SDcols = names(lower_boundaries)]
    
  } else {
    lower_boundaries <- character(0)
    upper_boundaries <- character(0)
  }
  
  # Define the set of rules to be used during matching for variables with predefined intervals
  list_simple_ranges_rules <- append(list_simple_ranges_rules,
                                     list(paste0(time_variable_in_exposed, " <= ", time_variables_in_candidate_matches[[2]]),
                                          paste0(time_variable_in_exposed, " >= ", time_variables_in_candidate_matches[[1]])))
  
  # Define join rules and column to be retained after the join
  # TODO review here for naive strategy
  strata_after_join <- c(unit_of_observation, paste0("i.", unit_of_observation))
  join_rules <- c(exact_strata_col, unlist(data.table::transpose(list_simple_ranges_rules)))
  cols_after_join <- c(strata_after_join, exact_strata_col,
                       variables_with_range_matching, time_variable_in_exposed, time_variables_in_candidate_matches,
                       paste0("x.", time_variable_in_exposed), excl_cols_exp, paste0("i.", excl_cols_cand))
  if (!is.null(variables_with_range_matching)) {
    cols_after_join <- c(cols_after_join, paste0("x.", names(lower_boundaries)))
  }
  
  # Get unique UoO
  distinct_UoO <- unique(data.table::rbindlist(list(exposed[, ..unit_of_observation],
                                                    candidate_matches[, ..unit_of_observation])))
  # TODO clean later on
  exposed_filtered <- exposed
  candidate_filtered <- candidate_matches
  
  # Generate samples of UoO and save them
  # TODO change here for sampling
  # TODO set seed for bootstrap sampling
  set.seed(123)
  seeds <- replicate(number_of_bootstrapping_samples, {
    sample(1:100, 1)
  }, simplify = T)
  # TODO remove for release?
  data.table::setorderv(distinct_UoO, unit_of_observation)
  pop_size <- nrow(distinct_UoO)
  for (i in 1:number_of_bootstrapping_samples) {
    set.seed(seeds[[i]])
    file_name <- file.path(temporary_folder, paste0("bootstrap_UoO_naive_", i))
    qs::qsave(data.table::setkeyv(distinct_UoO[sample(.N, pop_size, replace = T)], unit_of_observation),
              file_name, nthreads = data.table_threads)
  }
  rm(distinct_UoO)
  
  # IMPORTANT: necessary for the join in case 2+ variables with ranges
  data.table::setDT(exposed_filtered)
  data.table::setDT(candidate_filtered)
  
  # TODO review here for naive strategy
  # Matching
  matched_df <- exposed_filtered[candidate_filtered, ..cols_after_join, on = join_rules, nomatch = NULL]
  
  # Convert names related to range variables to what we want in the end
  # Lower bound variables are retransformed to inital ones
  if (!is.null(variables_with_range_matching)) {
    data.table::setnames(matched_df, variables_with_range_matching, paste0("i.", variables_with_range_matching))
    data.table::setnames(matched_df, paste0("x.", names(lower_boundaries)), names(variables_with_range_matching))
    matched_df[, (names(variables_with_range_matching)) := lapply(Map(`-`, .SD, lower_boundaries), as.integer), .SDcols = names(variables_with_range_matching)]
  }
  
  # Convert names related to time related variables to what we want in the end
  data.table::setnames(matched_df, time_variable_in_exposed, paste0("i.", time_variable_in_exposed))
  data.table::setnames(matched_df, paste0("x.", time_variable_in_exposed), time_variable_in_exposed)
  
  set.seed(123)
  
  # Create the bootstrap sample and then extract a number of controls for each exposed
  # Save the dataset
  data.table::setkeyv(matched_df, unit_of_observation)
  for (i in 1:number_of_bootstrapping_samples) {
    bootstrap_sample <- qs::qread(file.path(temporary_folder, paste0("bootstrap_UoO_naive_", i)), nthreads = data.table_threads)
    
    # Create bootstrap sample
    # TODO review here
    bootstrap_sample <- matched_df[bootstrap_sample, on = "person_id",
                                   nomatch = NULL, allow.cartesian = T][bootstrap_sample,
                                                                        on = c(i.person_id = "person_id"),
                                                                        nomatch = NULL, allow.cartesian = T]
    
    # Extract a number of controls for each exposed
    # TODO change here for sampling
    bootstrap_sample <- bootstrap_sample[bootstrap_sample[, .I[sample(.N, min(.N, sample_size_per_exposed))], by = "person_id"][[2]]]
    
    # Helps in defining the column order
    common_cols <- intersect(excl_cols_exp, excl_cols_cand)
    if (length(common_cols) > 0) {
      pre_cols <- c(setdiff(excl_cols_cand, excl_cols_exp), common_cols)
      post_cols <- c(setdiff(excl_cols_exp, excl_cols_cand), paste0("i.", common_cols))
    } else {
      pre_cols <- c(setdiff(excl_cols_cand, excl_cols_exp))
      post_cols <- c(setdiff(excl_cols_exp, excl_cols_cand))
    }
    
    # Final column order
    if (!is.null(variables_with_range_matching)) {
      col_order_range_matching <- paste0("i.", variables_with_range_matching)
    } else {
      col_order_range_matching <- character(0)
    }
    col_order <- c(strata_after_join, time_variable_in_exposed, time_variables_in_candidate_matches,
                   variables_with_range_matching, pre_cols, variables_with_exact_matching,
                   col_order_range_matching, post_cols)
    data.table::setcolorder(bootstrap_sample, col_order)
    
    # Final save of bootstrap sample
    file_name <- file.path(output_matching, paste0("bootstrap_naive_", i))
    qs::qsave(bootstrap_sample, file_name, nthreads = data.table_threads)
    rm(bootstrap_sample)
  }
}
