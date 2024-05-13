
for (i in 1:nrow(combination_experiment)){
  
  save(i, file = file.path(folder, "g_parameters", "i.RData"))
  
  rm(list=ls(all.names=TRUE))
  folder <- "Experiment"
  load(file.path(folder, "g_parameters", "i.RData"), envir = .GlobalEnv)
  load(file.path(folder, "g_parameters", "parameters.RData"), envir = .GlobalEnv)
  
  single_row <- combination_experiment[i, ]
  
  print(paste0(round((i - 1)/nrow(combination_experiment) * 100), "% label: ", single_row[, complete_label]))
  
  # Load simulated datasets
  exposed <- qs::qread(file.path(folder, "g_datasets", paste0("exposed_", single_row[, label_exp], ".qs")))
  candidate_matches = qs::qread(file.path(folder, "g_datasets", paste0("candidate_matches_", single_row[, label_exp],
                                                                       "_", single_row[, label_cm], ".qs")))
  
  exposed_pre <- data.table::copy(exposed)
  candidate_matches_pre <- data.table::copy(candidate_matches)
  
  variables_with_exact_matching <- single_row[, cat_var]
  if (single_row[, age] == "exact") {
    variables_with_exact_matching <- c(variables_with_exact_matching, "age")
    variables_with_range_matching <- NULL
    range_of_variables_with_range_matching <- NULL
  } else {
    variables_with_range_matching <- c("age")
    range_of_variables_with_range_matching <- list(c(-1, 1))
  }
  
  number_of_bootstrapping_samples <- if (!is.na(single_row[, boot_n])) single_row[, boot_n] else NULL
  
  sample_size_per_exposed <- if (single_row[, samp_schema] != "N") as.integer(single_row[, samp_schema]) else single_row[, samp_schema]
  
  methodology_for_bootstrapping <- if (!is.na(single_row[, boot_schema])) single_row[, boot_schema] else NULL
  
  if (single_row[, cores] == 1) data.table::setDTthreads(1) else data.table::setDTthreads(original_data.table_threads)
  
  if (single_row[, threshold_to_use] %in% c("max", "half", "double")) {
    
    list_simple_ranges_rules <- list()
    lower_boundaries <- character()
    upper_boundaries <- character()
    
    if (!is.null(variables_with_range_matching)) {
      # TODO check names usage here
      lower_boundaries <- unlist(range_of_variables_with_range_matching)[c(TRUE, FALSE)]
      names(lower_boundaries) <- paste0("lower_interval_", names(variables_with_range_matching))
      upper_boundaries <- unlist(range_of_variables_with_range_matching)[c(FALSE, TRUE)]
      names(upper_boundaries) <- paste0("upper_interval_", names(variables_with_range_matching))
      data.table::setnames(exposed_pre, variables_with_range_matching, names(lower_boundaries))
      
      list_simple_ranges_rules <- list(paste0(names(lower_boundaries), " <= ", variables_with_range_matching),
                                       paste0(names(upper_boundaries), " >= ", variables_with_range_matching))
      
      exposed_pre[, (names(lower_boundaries)) := Map(`+`, .SD, lower_boundaries), .SDcols = names(lower_boundaries)]
      exposed_pre[, (names(upper_boundaries)) := Map(`+`, .SD, upper_boundaries - lower_boundaries),
                  .SDcols = names(lower_boundaries)]
      
    }
    
    exposed_tr <- exposed_pre[, .N, by = c(variables_with_exact_matching, names(lower_boundaries), names(upper_boundaries))]
    exposed_tr[, N := as.numeric(N)]
    candidate_tr <- candidate_matches_pre[, .N, by = c(variables_with_exact_matching, variables_with_range_matching)]
    exposed_tr[, N := as.numeric(N)]
    cols_to_include <- c(variables_with_exact_matching, variables_with_range_matching)
    if (!is.null(variables_with_range_matching)) {
      cols_to_include <- c(cols_to_include, paste0("x.", c(names(lower_boundaries), names(upper_boundaries))))
    }
    cols_to_include <- c(cols_to_include, "N", "i.N")
    smaller_join_rules <- c(variables_with_exact_matching, unlist(data.table::transpose(list_simple_ranges_rules)))
    complete_tr <- exposed_tr[candidate_tr, ..cols_to_include, on = smaller_join_rules, nomatch = NULL]
    if (!is.null(variables_with_range_matching)) {
      data.table::setnames(complete_tr, paste0("x.", c(names(lower_boundaries), names(upper_boundaries))),
                           c(names(lower_boundaries), names(upper_boundaries)))
    }
    complete_tr[, V2 := N * i.N][, c("N", "i.N") := NULL]
    complete_tr <- complete_tr[, .(V2 = sum(V2)), by = c(variables_with_exact_matching, names(lower_boundaries), names(upper_boundaries))]
    rm(exposed_tr, candidate_tr)
    
    threshold <- data.table::fcase(single_row[, threshold_to_use] == "max", complete_tr[, max(V2)],
                                   single_row[, threshold_to_use] == "double", complete_tr[, max(V2)] * 2,
                                   single_row[, threshold_to_use] == "half", round(complete_tr[, max(V2)] / 2))
    
    
    rm(complete_tr, list_simple_ranges_rules, cols_to_include, lower_boundaries, upper_boundaries, smaller_join_rules)
    
  } else {
    threshold <- as.integer(single_row[, threshold_to_use])
  }
  
  dir.create(file.path(folder, "g_intermediate", single_row[, complete_label]), showWarnings = F)
  dir.create(file.path(folder, "g_output", single_row[, complete_label]), showWarnings = F)
  
  # 
  bnch <- bench::mark(
    do.call(
      what = single_row[, function_to_use],
      args = list(exposed = data.table::copy(exposed),
                  candidate_matches = data.table::copy(candidate_matches),
                  unit_of_observation = c("person_id"),
                  time_variable_in_exposed = c("time0"),
                  time_variables_in_candidate_matches = c("start", "end"),
                  variables_with_exact_matching = variables_with_exact_matching,
                  variables_with_range_matching = variables_with_range_matching,
                  range_of_variables_with_range_matching = range_of_variables_with_range_matching,
                  sample_size_per_exposed = sample_size_per_exposed,
                  number_of_bootstrapping_samples = number_of_bootstrapping_samples,
                  methodology_for_bootstrapping = methodology_for_bootstrapping,
                  threshold = threshold,
                  temporary_folder = file.path(folder, "g_intermediate", single_row[, complete_label]),
                  output_matching = file.path(folder, "g_output", single_row[, complete_label]))
    ), iterations = 10)
  
  # TODO add Secondary memory utilization
  bnch <- data.table::as.data.table(bnch)
  bnch[, threshold_used := threshold]
  bnch[, timestamp := Sys.time()]
  installed_pkgs <- data.table::as.data.table(benchmarkme::get_sys_details(ram = F)$installed_packages)
  bnch[, version_data.table := installed_pkgs[Package == "data.table", ]$Version]
  bnch[, version_qs := installed_pkgs[Package == "qs", ]$Version]
  bnch[, OS_type := data.table::as.data.table(benchmarkme::get_sys_details(ram = F)$sys_info$sysname)]
  bnch[, size_intermediate := sum(file.info(list.files(file.path(folder, "g_intermediate", combination_experiment[1, complete_label]), all.files = TRUE, recursive = TRUE, full.names = TRUE))$size)]
  bnch[, size_output := sum(file.info(list.files(file.path(folder, "g_output", combination_experiment[1, complete_label]), all.files = TRUE, recursive = TRUE, full.names = TRUE))$size)]
  
  
  bnch <- bnch |> dplyr::bind_cols(single_row) |>
    dplyr::bind_cols(data.table::as.data.table(benchmarkme::get_cpu())) |>
    dplyr::bind_cols(data.table::data.table(ram = benchmarkme:::clean_ram(suppressWarnings(try(
      benchmarkme:::system_ram(R.version$os),silent = TRUE)), R.version$os))) |>
    dplyr::bind_cols(data.table::as.data.table(benchmarkme::get_r_version()))
  
  # saveRDS(bnch, file.path(folder, "g_results", paste0(single_row[, complete_label], ".rds")))
  
  rm(bnch, single_row)
}