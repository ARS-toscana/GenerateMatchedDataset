
bnch <- readRDS(file.path(folder, "g_results", paste0(single_row[, complete_label], ".rds")))

bnch <- data.table::rbindlist(lapply(1:nrow(combination_experiment), function(x) {
  bnch <- readRDS(file.path(folder, "g_results", paste0(combination_experiment[x, complete_label], ".rds")))
  return(bnch[, .(complete_label, algo, time = bnch[, time][[1]], mem_alloc, n_itr, exp, cm, algo, threshold_to_use, threshold_used, match_vars_label, samp_schema_label, cores_label, model_name, no_of_cores, ram, OS_type, major, minor)])
}))
