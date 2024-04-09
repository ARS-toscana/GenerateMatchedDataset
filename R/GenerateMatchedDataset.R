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
                                   exclude_sameUoO = NULL,
                                   algorithm_for_matching = NULL,
                                   threshold = NULL,
                                   technical_details_of_matching = NULL,
                                   temporary_folder = NULL) {
  
  if (is.null(names(variables_with_range_matching))) {
    names(variables_with_range_matching) <- variables_with_range_matching
  }
  
  data.table_threads <- data.table::getDTthreads()
  
  # preamble <- "person_id != i.person_id & vax1_day >= start & vax1_day < end"
  
  exact_strata_col <- if (!missing(variables_with_exact_matching)) "exact_strata" else c()
  
  # TODO write routine to minimize number of hash tables
  # TODO to prevent different number of rows create if possible single table from entire population
  excl_cols_exp <- setdiff(colnames(exposed), c(variables_with_exact_matching, variables_with_range_matching,
                                                unit_of_observation, time_variable_in_exposed, time_variables_in_candidate_matches))
  excl_cols_exp_real <- c(unit_of_observation, excl_cols_exp)
  excl_cols_cand <- setdiff(colnames(candidate_matches), c(variables_with_exact_matching, variables_with_range_matching,
                                                           unit_of_observation, time_variables_in_candidate_matches, time_variable_in_exposed))
  excl_cols_cand_real <- c(unit_of_observation, excl_cols_cand)
  
  qs::qsave(unique(candidate_matches[, ..excl_cols_cand_real]),
            file.path(temporary_folder, "HT_excl_candidates"), nthreads = data.table_threads)
  candidate_matches[, (excl_cols_cand) := NULL]
  qs::qsave(unique(exposed[, ..excl_cols_exp_real]),
            file.path(temporary_folder, "HT_excl_exposed"), nthreads = data.table_threads)
  exposed[, (excl_cols_exp) := NULL]
  rm(excl_cols_exp_real, excl_cols_cand_real)
  
  hash_table_exact <- unique(data.table::rbindlist(list(unique(exposed[, ..variables_with_exact_matching]),
                                                        unique(candidate_matches[, ..variables_with_exact_matching]))))[, exact_strata := 1:.N]
  qs::qsave(hash_table_exact,
            file.path(temporary_folder, "HT_exact"), nthreads = data.table_threads)
  exposed <- exposed[hash_table_exact, on = variables_with_exact_matching][, (variables_with_exact_matching) := NULL]
  candidate_matches <- candidate_matches[hash_table_exact, on = variables_with_exact_matching][, (variables_with_exact_matching) := NULL]
  rm(hash_table_exact)
  
  list_simple_ranges_rules <- list()
  if (!is.null(variables_with_range_matching)) {
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
  
  list_simple_ranges_rules <- append(list_simple_ranges_rules,
                                     list(paste0(time_variable_in_exposed, " <= ", time_variables_in_candidate_matches[[2]]),
                                          paste0(time_variable_in_exposed, " >= ", time_variables_in_candidate_matches[[1]])))
  
  strata_after_join <- c(unit_of_observation, paste0("i.", unit_of_observation))
  join_rules <- c(exact_strata_col, unlist(data.table::transpose(list_simple_ranges_rules)))
  cols_after_join <- c(strata_after_join, exact_strata_col,
                       variables_with_range_matching, time_variable_in_exposed, time_variables_in_candidate_matches,
                       paste0("x.", time_variable_in_exposed))
  if (!is.null(variables_with_range_matching)) {
    cols_after_join <- c(cols_after_join, paste0("x.", names(lower_boundaries)))
  }
  
  
  # Threshold calculation
  exposed_tr <- exposed[, .N, by = exact_strata]
  candidate_tr <- candidate_matches[, .N, by = exact_strata]
  complete_tr <- exposed_tr[candidate_tr, .(exact_strata, N * i.N), on = "exact_strata", nomatch = NULL]
  
  group_integers <- function(values, threshold) {
    names(values) <- 1:length(values)
    values <- sort(values, decreasing = TRUE) # Sort values in descending order
    
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
  complete_tr <- complete_tr[, .(exact_strata, batch_number = assign_groups(V2, threshold))]
  N_of_batches <- max(complete_tr[, batch_number])
  
  for (batch_n in 1:N_of_batches) {
    filtered_exact_strata <- complete_tr[batch_number == batch_n, exact_strata]
    qs::qsave(exposed[exact_strata %in% filtered_exact_strata],
              file.path(temporary_folder, paste0("exposed_strata_", batch_n)), nthreads = data.table_threads)
    qs::qsave(candidate_matches[exact_strata %in% filtered_exact_strata],
              file.path(temporary_folder, paste0("candidates_strata_", batch_n)), nthreads = data.table_threads)
  }
  rm(filtered_exact_strata)
  unique_strata <- unique(data.table::rbindlist(list(exposed[, ..unit_of_observation],
                                                     candidate_matches[, ..unit_of_observation])))
  # TODO add for release
  rm(exposed, candidate_matches)
  
  pop_size <- nrow(unique_strata)
  set.seed(123)
  
  for (i in 1:number_of_bootstrapping_samples) {
    bootstrap_sample <- unique_strata[sample(.N, pop_size, replace = T)]
    data.table::setkeyv(bootstrap_sample, unit_of_observation)
    file_name <- file.path(temporary_folder, paste0("bootstrap_UoO_", i))
    qs::qsave(bootstrap_sample, file_name, nthreads = data.table_threads)
  }
  
  for (batch_n in 1:N_of_batches) {
    exposed_filtered <- qs::qread(file.path(temporary_folder, paste0("exposed_strata_", batch_n)), nthreads = data.table_threads)
    candidate_filtered <- qs::qread(file.path(temporary_folder, paste0("candidates_strata_", batch_n)), nthreads = data.table_threads)
    
    # TODO remove for release
    # exposed_filtered <- data.table::copy(exposed)
    # candidate_filtered <- data.table::copy(candidate_matches)
    data.table::setDT(exposed_filtered)
    data.table::setDT(candidate_filtered)
    
    matched_df <- exposed_filtered[candidate_filtered, ..cols_after_join, on = join_rules, nomatch = NULL]
    
    # join_2 <- function() {
    # exposed_keyed <- data.table::copy(exposed)
    # candidate_keyed <- data.table::copy(candidate_matches)[, age_1 := age]
    # data.table::data.table::setkeyv(exposed_keyed, c("exact_strata", "lower_interval", "upper_interval"))
    # data.table::data.table::setkeyv(candidate_keyed, c("exact_strata", "age_1", "age"))
    # test_1 <- data.table::foverlaps(candidate_keyed, exposed_keyed)[, ..cols_after_join_1]
    # data.table::setnames(test_1, c("lower_interval"), c("x.lower_interval"))
    # # data.table::setorder(test_1, exact_strata, i.person_id, person_id)
    # }
    
    # test_1 <- matched_df[matched[, ..strata_after_join], on = strata_after_join, nomatch = NULL]
    
    if (!is.null(variables_with_range_matching)) {
      data.table::setnames(matched_df, variables_with_range_matching, paste0("i.", variables_with_range_matching))
      data.table::setnames(matched_df, paste0("x.", names(lower_boundaries)), names(variables_with_range_matching))
      matched_df[, (names(variables_with_range_matching)) := lapply(Map(`-`, .SD, lower_boundaries), as.integer), .SDcols = names(variables_with_range_matching)]
    }
    
    data.table::setnames(matched_df, time_variable_in_exposed, paste0("i.", time_variable_in_exposed))
    data.table::setnames(matched_df, paste0("x.", time_variable_in_exposed), time_variable_in_exposed)
    
    data.table::setkeyv(matched_df, unit_of_observation)
    
    set.seed(123)
    for (i in 1:number_of_bootstrapping_samples) {
      bootstrap_sample <- qs::qread(file.path(temporary_folder, paste0("bootstrap_UoO_", i)), nthreads = data.table_threads)
      
      # TODO revised here
      bootstrap_sample <- matched_df[bootstrap_sample, on = "person_id",
                                     nomatch = NULL][bootstrap_sample, on = c(i.person_id = "person_id"),
                                                     nomatch = NULL, allow.cartesian = T]
      bootstrap_sample <- bootstrap_sample[bootstrap_sample[, .I[sample(.N, min(.N, sample_size_per_exposed))], by = "person_id"][[2]]]
      file_name <- file.path(temporary_folder, paste0("bootstrap_", i, "_batch_", batch_n))
      qs::qsave(bootstrap_sample, file_name, nthreads = data.table_threads)
    }
    
    
    
    # number_of_bootstrapping_samples <- replicate(number_of_bootstrapping_samples,
    #                                data.table::setorderv(unique_strata[sample(.N, pop_size, replace = T)], cols = unit_of_observation),
    #                                simplify = FALSE)
    
    # folder <- file.path(getwd(), "test_with_simulated_data", "output_datasets")
    # dir.create(folder, showWarnings = F, recursive = T)
    
    # data.table_threads <- data.table::getDTthreads()
    # for (i in 1:length(number_of_bootstrapping_samples)) {
    #   test_1 <- matched_df[number_of_bootstrapping_samples[[i]], on = "person_id",
    #                        nomatch = NULL][number_of_bootstrapping_samples[[i]], on = c(i.person_id = "person_id"),
    #                                        nomatch = NULL, allow.cartesian = T]
    #   test_2 <- test_1[test_1[, .I[sample(.N, min(.N, sample_size_per_exposed))], by = "person_id"][[2]]]
    #   file_name <- file.path(folder, paste0("bootstrap_", i, "_strata_", batch_n))
    #   qs::qsave(test_2, file_name, nthreads = data.table_threads)
    # }
    
    
    
    
    # matched_df <- matched_df[hash_table_exact, on = c("exact_strata")][, exact_strata := NULL]
    # matched_df <- matched_df[hash_table_excl_exp, on = c("person_id")]
    # matched_df <- matched_df[hash_table_excl_cand, on = c("i.person_id == person_id")]
    # data.table::setcolorder(matched_df, c(strata_after_join, "vax1_day", "start", "end", paste0("x.", names(lower_boundaries)),
    #                                       "COMORBIDITY", variables_with_exact_matching, variables_with_range_matching, "i.COMORBIDITY", "i.vax1_day"))
  }
  
  for (i in 1:number_of_bootstrapping_samples) {
    tmp <- data.table::rbindlist(lapply(1:N_of_batches, function(x) {
      file_name <- file.path(temporary_folder, paste0("bootstrap_", i, "_batch_", batch_n))
      qs::qread(file_name, nthreads = data.table::getDTthreads())
    }))
    
    hash_table_exact <- qs::qread(file.path(temporary_folder, "HT_exact"), nthreads = data.table::getDTthreads())
    hash_table_excl_exp <- qs::qread(file.path(temporary_folder, "HT_excl_exposed"), nthreads = data.table::getDTthreads())
    hash_table_excl_cand <- qs::qread(file.path(temporary_folder, "HT_excl_candidates"), nthreads = data.table::getDTthreads())
    
    tmp <- tmp[hash_table_exact, on = c("exact_strata"), nomatch = NULL][, exact_strata := NULL]
    tmp <- tmp[hash_table_excl_exp, on = c("person_id"), nomatch = NULL]
    tmp <- tmp[hash_table_excl_cand, on = c("i.person_id == person_id"), nomatch = NULL]
    
    common_cols <- intersect(excl_cols_exp, excl_cols_cand)
    if (length(common_cols) > 0) {
      pre_cols <- c(setdiff(excl_cols_cand, excl_cols_exp), common_cols)
      post_cols <- c(setdiff(excl_cols_exp, excl_cols_cand), paste0("i.", common_cols))
    } else {
      pre_cols <- c(setdiff(excl_cols_cand, excl_cols_exp))
      post_cols <- c(setdiff(excl_cols_exp, excl_cols_cand))
    }
    
    if (!is.null(variables_with_range_matching)) {
      col_order_range_matching <- paste0("i.", variables_with_range_matching)
    } else {
      col_order_range_matching <- character(0)
    }
    col_order <- c(strata_after_join, time_variable_in_exposed, time_variables_in_candidate_matches,
                   variables_with_range_matching, pre_cols, variables_with_exact_matching,
                   col_order_range_matching, post_cols)
    
    data.table::setcolorder(tmp, col_order)
    
    file_name <- file.path(temporary_folder, paste0("bootstrap_", i))
    qs::qsave(tmp, file_name, nthreads = data.table_threads)
  }
}

