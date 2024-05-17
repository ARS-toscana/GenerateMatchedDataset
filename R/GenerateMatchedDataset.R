#' @param exposed The exposed data table
#' @param candidate_matches The candidate_matches data table
#' @param unit_of_observation
#' @param type_of_matching Decide the type of matching. Default to 'on variables'
#' @param time_variable_in_exposed Variable containing the days UoO are exposed. 
#' @param time_variables_in_candidate_matches Vector with variables containing the start and end when a UoO are candidate matches
#' @param variables_with_exact_matching Vector of variable names to be matched exactly. 
#' @param variables_with_range_matching Vector of variables to be matched in a range.
#' @param range_of_variables_with_range_matching List of vectors. Each vector contains the range around where to match a variable defined in variables_with_range_matching. Follow the same order as variables_with_range_matching
#' @param additional_matching_rules Additional rule to be used during matches
#' @param rule_for_matching_on_dates "exact"
#' @param output_matching Output folder and names. After we will append bootstrap number.
#' @param seeds_for_sampling
#' @param temporary_folder Folder where to store intermediate dataset. Default is NULL, if TRUE then the default folder is tempdir() otherwise it is possible to pass a custom path. If TRUE or custom path the function enter RAM saving mode.
#' @param sample_size_per_exposed Number of controls to be matched for each exposed. Default is 1
#' @param methodology_for_bootstrapping The default for this argument is NULL so no bootstrapping. Other options are "Sample exposed" and "Sample units of observations".
#' @param number_of_bootstrapping_samples Number of bootstrap samples to be generated if methodology_for_bootstrapping is not NULL
#' @param type_of_sampling With or without replacement
#' @param exclude_sameUoO Do not match the same UoO
#' @param algorithm_for_matching Algorithms to test. Possible values are "naive", ...
#' @param threshold Bin capacity to be used for creating bins based on the original dataset
#' @param technical_details_of_matching

GenerateMatchedDataset <- function(exposed,
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
                                   exclude_sameUoO = TRUE,
                                   algorithm_for_matching = NULL,
                                   threshold = NULL,
                                   technical_details_of_matching = NULL,
                                   temporary_folder = NULL) {
  
  # preamble <- "person_id != i.person_id & vax1_day >= start & vax1_day < end"
  # TODO Transform UoO to integer
  # TODO add preamble
  # TODO Add a join key parameter
  # TODO clean temporaary dataset after loading them to save space
  # Get number of threads to use for qs
  data.table_threads <- data.table::getDTthreads()
  
  names(variables_with_range_matching) <- variables_with_range_matching
  
  # Remove columns not used during the matching and not primary keys
  # TODO write routine to minimize number of hash tables
  # TODO to prevent different number of rows create if possible single table from entire population
  excl_cols_exp <- setdiff(colnames(exposed), c(variables_with_exact_matching, variables_with_range_matching,
                                                unit_of_observation, time_variable_in_exposed,
                                                time_variables_in_candidate_matches))
  excl_cols_exp_real <- c(unit_of_observation, excl_cols_exp)
  excl_cols_cand <- setdiff(colnames(candidate_matches), c(variables_with_exact_matching, variables_with_range_matching,
                                                           unit_of_observation, time_variables_in_candidate_matches,
                                                           time_variable_in_exposed))
  excl_cols_cand_real <- c(unit_of_observation, excl_cols_cand)
  
  if (!identical(excl_cols_exp, character(0))) {
    qs::qsave(unique(exposed[, ..excl_cols_exp_real]),
              file.path(temporary_folder, "HT_excl_exposed"), nthreads = data.table_threads)
    exposed[, (excl_cols_exp) := NULL]
  }
  if (!identical(excl_cols_cand, character(0))) {
    qs::qsave(unique(candidate_matches[, ..excl_cols_cand_real]),
              file.path(temporary_folder, "HT_excl_candidates"), nthreads = data.table_threads)
    candidate_matches[, (excl_cols_cand) := NULL]
  }
  
  rm(excl_cols_exp_real, excl_cols_cand_real)
  
  # Recode using an hash table the variables with exact matching
  # Check if exact strata columns are defined
  exact_strata_col <- character()
  if (!is.null(variables_with_exact_matching)) {
    exact_strata_col <- "exact_strata"
    
    hash_table_exact <- unique(data.table::rbindlist(list(unique(exposed[, ..variables_with_exact_matching]),
                                                          unique(candidate_matches[, ..variables_with_exact_matching]))))
    hash_table_exact[, exact_strata := 1:.N]
    qs::qsave(hash_table_exact,
              file.path(temporary_folder, "HT_exact"), nthreads = data.table_threads)
    exposed <- exposed[hash_table_exact, on = variables_with_exact_matching][, (variables_with_exact_matching) := NULL]
    candidate_matches <- candidate_matches[hash_table_exact, on = variables_with_exact_matching][, (variables_with_exact_matching) := NULL]
    rm(hash_table_exact)
  }
  
  # Define a lower and upper boundaries for variables with ranges
  # In exposed create variables for the boundaries
  # Define the set of rules to be used during matching
  list_simple_ranges_rules <- list()
  lower_boundaries <- character()
  upper_boundaries <- character()
  
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
    
  }
  
  # Define the set of rules to be used during matching for variables with predefined intervals
  list_time_ranges_rules <- list(paste0(time_variable_in_exposed, " <= ", time_variables_in_candidate_matches[[2]]),
                                 paste0(time_variable_in_exposed, " >= ", time_variables_in_candidate_matches[[1]]))
  
  # Define join rules and column to be retained after the join
  strata_after_join <- c(unit_of_observation, paste0("i.", unit_of_observation))
  join_rules <- c(exact_strata_col, unlist(data.table::transpose(list_simple_ranges_rules)),
                  unlist(data.table::transpose(list_time_ranges_rules)))
  cols_after_join <- c(strata_after_join, exact_strata_col,
                       variables_with_range_matching, time_variable_in_exposed, time_variables_in_candidate_matches)
  if (!is.null(time_variable_in_exposed)) cols_after_join <- c(cols_after_join, paste0("x.", time_variable_in_exposed))
  if (!is.null(variables_with_range_matching)) cols_after_join <- c(cols_after_join, paste0("x.", names(lower_boundaries)))
  
  # Calculate the theoretical number of combination of each exact strata
  exposed_tr <- exposed[, .N, by = c(exact_strata_col, names(lower_boundaries), names(upper_boundaries))]
  exposed_tr[, N := as.numeric(N)]
  candidate_tr <- candidate_matches[, .N, by = c(exact_strata_col, variables_with_range_matching)]
  exposed_tr[, N := as.numeric(N)]
  cols_to_include <- c(exact_strata_col, variables_with_range_matching)
  if (!is.null(variables_with_range_matching)) {
    cols_to_include <- c(cols_to_include, paste0("x.", c(names(lower_boundaries), names(upper_boundaries))))
  }
  cols_to_include <- c(cols_to_include, "N", "i.N")
  smaller_join_rules <- c(exact_strata_col, unlist(data.table::transpose(list_simple_ranges_rules)))
  complete_tr <- exposed_tr[candidate_tr, ..cols_to_include, on = smaller_join_rules, nomatch = NULL]
  if (!is.null(variables_with_range_matching)) {
    data.table::setnames(complete_tr, paste0("x.", c(names(lower_boundaries), names(upper_boundaries))),
                         c(names(lower_boundaries), names(upper_boundaries)))
  }
  complete_tr[, V2 := N * i.N][, c("N", "i.N") := NULL]
  smaller_HT <- data.table::copy(complete_tr)[, V2 := NULL]
  complete_tr <- complete_tr[, .(V2 = sum(V2)), by = c(exact_strata_col, names(lower_boundaries), names(upper_boundaries))]
  rm(exposed_tr, candidate_tr)
  
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
  
  # Apply the function to create the variable 'batch_number' based on threshold
  data.table::setorderv(complete_tr, "V2")
  complete_tr <- complete_tr[, V2 := assign_groups(V2, threshold)]
  data.table::setnames(complete_tr, "V2", "batch_number")
  N_of_batches <- max(complete_tr[, batch_number])
  
  complete_tr <- smaller_HT[complete_tr, on = intersect(colnames(smaller_HT), colnames(complete_tr))]
  rm(smaller_HT)
  
  # Save each batch in a separate file
  for (batch_n in 1:N_of_batches) {
    cols_exp <- colnames(exposed)
    cols_to_keep <- intersect(colnames(complete_tr), cols_exp)
    qs::qsave(exposed[unique(complete_tr[batch_number == batch_n, ..cols_to_keep]), ..cols_exp,
                      on = cols_to_keep, allow.cartesian = TRUE],
              file.path(temporary_folder, paste0("exposed_strata_", batch_n)), nthreads = data.table_threads)
    cols_cand <- colnames(candidate_matches)
    cols_to_keep <- intersect(colnames(complete_tr), cols_cand)
    qs::qsave(candidate_matches[unique(complete_tr[batch_number == batch_n, ..cols_to_keep]), ..cols_cand,
                                on = cols_to_keep, allow.cartesian = TRUE],
              file.path(temporary_folder, paste0("candidates_strata_", batch_n)), nthreads = data.table_threads)
  }
  rm(complete_tr, cols_exp, cols_cand, cols_to_keep)
  
  if (!is.null(number_of_bootstrapping_samples)) {
    
    # Get unique UoO and then remove exposed and candidate_matches dataset since they are not used anymore
    if (methodology_for_bootstrapping == "SExp") {
      distinct_UoO <- unique(exposed[, ..unit_of_observation])
    } else if (methodology_for_bootstrapping == "SUoO") {
      distinct_UoO <- unique(data.table::rbindlist(list(exposed[, ..unit_of_observation],
                                                        candidate_matches[, ..unit_of_observation])))
    }
    
    # Generate samples of UoO and save them
    # TODO change here for sampling
    # TODO set seed for bootstrap sampling
    set.seed(seeds_for_sampling)
    # seeds <- replicate(number_of_bootstrapping_samples, {
    #   sample(1:100, 1)
    # }, simplify = T)
    # TODO remove for release?
    data.table::setorderv(distinct_UoO, unit_of_observation)
    pop_size <- nrow(distinct_UoO)
    for (i in 1:number_of_bootstrapping_samples) {
      # set.seed(seeds[[i]])
      file_name <- file.path(temporary_folder, paste0("bootstrap_UoO_", i))
      qs::qsave(data.table::setkeyv(distinct_UoO[sample(.N, pop_size, replace = T)], unit_of_observation),
                file_name, nthreads = data.table_threads)
    }
    rm(distinct_UoO)
  }
  rm(exposed, candidate_matches)
  
  
  # For each batch calculate the bootstrap samples
  for (batch_n in 1:N_of_batches) {
    
    # Load batch
    exposed_filtered <- qs::qread(file.path(temporary_folder, paste0("exposed_strata_", batch_n)), nthreads = data.table_threads)
    candidate_filtered <- qs::qread(file.path(temporary_folder, paste0("candidates_strata_", batch_n)), nthreads = data.table_threads)
    
    # IMPORTANT: necessary for the join in case 2+ variables with ranges
    data.table::setDT(exposed_filtered)
    data.table::setDT(candidate_filtered)
    
    # Matching
    matched_df <- exposed_filtered[candidate_filtered, ..cols_after_join, on = join_rules, nomatch = NULL]
    rm(candidate_filtered, exposed_filtered)
    
    # Remove same UoO if necessary
    if (exclude_sameUoO) {
      
      AndDiff = function(cond){
        Reduce(
          function(x, y) call("&", x, y),
          lapply(cond, function(var) call("!=", var, paste0("i.", var)))
        )
      }
      filter_cond <- AndDiff(unit_of_observation)
      matched_df <- matched_df[eval(filter_cond)]
    }
    
    # Convert names related to range variables to what we want in the end
    # Lower bound variables are transformed back to initial ones
    if (!is.null(variables_with_range_matching)) {
      data.table::setnames(matched_df, variables_with_range_matching, paste0("i.", variables_with_range_matching))
      data.table::setnames(matched_df, paste0("x.", names(lower_boundaries)), names(variables_with_range_matching))
      matched_df[, (names(variables_with_range_matching)) := lapply(Map(`-`, .SD, lower_boundaries), as.integer), .SDcols = names(variables_with_range_matching)]
    }
    
    # Convert names related to time related variables to what we want in the end
    if (!is.null(time_variable_in_exposed)) {
      data.table::setnames(matched_df, time_variable_in_exposed, paste0("i.", time_variable_in_exposed))
      data.table::setnames(matched_df, paste0("x.", time_variable_in_exposed), time_variable_in_exposed)
    }
    
    set.seed(seeds_for_sampling)
    
    # Create the bootstrap sample and then extract a number of controls for each exposed
    # Save the dataset
    data.table::setkeyv(matched_df, unit_of_observation)
    if (T) {
      
      bootstrap_sample <- data.table::copy(matched_df)
      
      # Extract a number of controls for each exposed
      if (sample_size_per_exposed != "N") {
        bootstrap_sample <- bootstrap_sample[bootstrap_sample[, .I[sample(.N, min(.N, sample_size_per_exposed))],
                                                              by = unit_of_observation][[2]]]
      }
      
      # Save the dataset
      file_name <- file.path(temporary_folder, paste0("no_bootstrap_batch_", batch_n))
      qs::qsave(bootstrap_sample, file_name, nthreads = data.table_threads)
      rm(bootstrap_sample)
    }
    
    # Create the bootstrap sample and then extract a number of controls for each exposed
    # Save the dataset
    if (!is.null(number_of_bootstrapping_samples)) {
      for (i in 1:number_of_bootstrapping_samples) {
        bootstrap_sample <- qs::qread(file.path(temporary_folder, paste0("bootstrap_UoO_", i)), nthreads = data.table_threads)
        
        # Create bootstrap sample
        # TODO review here
        if (methodology_for_bootstrapping == "SExp") {
          bootstrap_sample <- matched_df[bootstrap_sample, on = unit_of_observation,
                                         nomatch = NULL, allow.cartesian = T]
        } else if (methodology_for_bootstrapping == "SUoO") {
          bootstrap_sample <- matched_df[bootstrap_sample, on = unit_of_observation,
                                         nomatch = NULL, allow.cartesian = T][bootstrap_sample,
                                                                              on = paste(paste0("i.", unit_of_observation), "==",
                                                                                         unit_of_observation, collapse = ", "),
                                                                              nomatch = NULL, allow.cartesian = T]
        }
        
        
        
        # Extract a number of controls for each exposed
        if (sample_size_per_exposed != "N") {
          bootstrap_sample <- bootstrap_sample[bootstrap_sample[, .I[sample(.N, min(.N, sample_size_per_exposed))],
                                                                by = unit_of_observation][[2]]]
        }
        
        # Save the dataset
        file_name <- file.path(temporary_folder, paste0("bootstrap_", i, "_batch_", batch_n))
        qs::qsave(bootstrap_sample, file_name, nthreads = data.table_threads)
        rm(bootstrap_sample)
      }
      rm(matched_df)
    }
  }
  
  # Clean each dataset and combine them to from the complete bootstrap samples
  if (T) {
    
    # Load and combine all batches of a single bootstrap sample
    tmp <- data.table::rbindlist(lapply(1:N_of_batches, function(x) {
      file_name <- file.path(temporary_folder, paste0("no_bootstrap_batch_", x))
      qs::qread(file_name, nthreads = data.table::getDTthreads())
    }))
    
    # Add again excluded columns and single exact variables
    if (!is.null(variables_with_exact_matching)){
      hash_table_exact <- qs::qread(file.path(temporary_folder, "HT_exact"), nthreads = data.table::getDTthreads())
      tmp <- tmp[hash_table_exact, on = c(exact_strata_col), nomatch = NULL][, exact_strata := NULL]
    }
    rm(hash_table_exact)
    
    if (!identical(excl_cols_exp, character(0))) {
      hash_table_excl_exp <- qs::qread(file.path(temporary_folder, "HT_excl_exposed"), nthreads = data.table::getDTthreads())
      tmp <- tmp[hash_table_excl_exp, on = unit_of_observation, nomatch = NULL]
    }
    
    if (!identical(excl_cols_cand, character(0))) {
      hash_table_excl_cand <- qs::qread(file.path(temporary_folder, "HT_excl_candidates"), nthreads = data.table::getDTthreads())
      tmp <- tmp[hash_table_excl_cand, on = paste(paste0("i.", unit_of_observation), "==", unit_of_observation,
                                                  collapse = ", "), nomatch = NULL]
    }
    
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
    data.table::setcolorder(tmp, col_order)
    
    # Final save of bootstrap sample
    file_name <- file.path(output_matching, "no_bootstrap")
    qs::qsave(tmp, file_name, nthreads = data.table_threads)
  }
  
  # Clean each dataset and combine them to from the complete bootstrap samples
  if (!is.null(number_of_bootstrapping_samples)) {
    for (i in 1:number_of_bootstrapping_samples) {
      
      # Load and combine all batches of a single bootstrap sample
      tmp <- data.table::rbindlist(lapply(1:N_of_batches, function(x) {
        file_name <- file.path(temporary_folder, paste0("bootstrap_", i, "_batch_", x))
        qs::qread(file_name, nthreads = data.table::getDTthreads())
      }))
      
      # Add again excluded columns and single exact variables
      if (!is.null(variables_with_exact_matching)){
        hash_table_exact <- qs::qread(file.path(temporary_folder, "HT_exact"), nthreads = data.table::getDTthreads())
        tmp <- tmp[hash_table_exact, on = c(exact_strata_col), nomatch = NULL][, exact_strata := NULL]
      }
      rm(hash_table_exact)
      
      if (!identical(excl_cols_exp, character(0))) {
        hash_table_excl_exp <- qs::qread(file.path(temporary_folder, "HT_excl_exposed"), nthreads = data.table::getDTthreads())
        tmp <- tmp[hash_table_excl_exp, on = unit_of_observation, nomatch = NULL]
      }
      
      if (!identical(excl_cols_cand, character(0))) {
        hash_table_excl_cand <- qs::qread(file.path(temporary_folder, "HT_excl_candidates"), nthreads = data.table::getDTthreads())
        tmp <- tmp[hash_table_excl_cand, on = paste(paste0("i.", unit_of_observation), "==", unit_of_observation,
                                                    collapse = ", "), nomatch = NULL]
      }
      
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
      data.table::setcolorder(tmp, col_order)
      
      # Final save of bootstrap sample
      file_name <- file.path(output_matching, paste0("bootstrap_", i))
      qs::qsave(tmp, file_name, nthreads = data.table_threads)
    }
  }
}
