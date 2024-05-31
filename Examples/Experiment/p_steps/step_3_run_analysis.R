
bnch <- data.table::rbindlist(lapply(intersect(list.files(file.path(folder, "g_output", "Laptop 1")),
                                               paste0(combination_experiment[, complete_label], ".rds")), function(x) {
  bnch <- readRDS(file.path(folder, "g_output", "Laptop 1", x))
  # return(bnch[, .(complete_label, algo, time = bnch[, time][[1]], mem_alloc, size_intermediate, size_output, n_itr, exp, cm, threshold_to_use, threshold_used, match_vars_label, samp_schema_label, cores_label, model_name, no_of_cores, ram, OS_type, major, minor, version_data.table, version_qs, type_exp)])
  return(bnch[, .(complete_label, algo, time = bnch[, time][[1]], mem_alloc, size_intermediate, size_output, n_itr, exp, cm, threshold_to_use, threshold_used, match_vars_label, samp_schema_label, cores_label, model_name, no_of_cores, ram, OS_type, major, minor, version_data.table, version_qs)])
}))

qs::qsave(bnch, file.path(folder, "g_results", "Laptop 1", paste0("combined_benchmarks", ".qs")),
          nthreads = original_data.table_threads)
