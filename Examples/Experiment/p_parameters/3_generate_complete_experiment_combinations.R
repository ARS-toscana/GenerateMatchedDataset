# Algorithm to use
algo_df <- data.table::data.table(algo = c("Naive", "Hash", "Threshold", "Threshold",
                                           "Threshold", "Threshold"),
                                  algo_label = c("N", "H", "T1", "Tm", "Th", "Td"),
                                  function_to_use = c("GenerateMatchedDatasetNaive", "GenerateMatchedDatasetHT",
                                                      rep("GenerateMatchedDataset", 4)),
                                  threshold_to_use = c(NA, NA, 1, "max", "half", "double"))

combination_experiment <- merge(combination_experiment[, placeholder := T], algo_df[, placeholder := T],
                                by = "placeholder", allow.cartesian = T)[, placeholder := NULL]

match_vars_df <- data.table::data.table(age = c("exact", "range +-1", "exact"),
                                        cat_var = c("EXACT_UNIF", "EXACT_UNIF", "EXACT_NORM"),
                                        match_vars_label = c("Mat1", "Mat2", "Mat3"))

combination_experiment <- merge(combination_experiment[, placeholder := T], match_vars_df[, placeholder := T],
                                by = "placeholder", allow.cartesian = T)[, placeholder := NULL]

sampling_schemas <- data.table::data.table(samp_schema = c(1, "N", 1, 1),
                                           boot_n = c(NA, NA, 10, 10),
                                           boot_schema = c(NA, NA, "SExp", "SUoO"),
                                           samp_schema_label = c("S1", "SN", "S1B10SExp", "S1B10SUoO"))

combination_experiment <- merge(combination_experiment[, placeholder := T], sampling_schemas[, placeholder := T],
                                by = "placeholder", allow.cartesian = T)[, placeholder := NULL]

tech_restriction <- data.table::data.table(cores = c(T, 1),
                                           cores_label = c("Cdef", "C1"))

combination_experiment <- merge(combination_experiment[, placeholder := T], tech_restriction[, placeholder := T],
                                by = "placeholder", allow.cartesian = T)[, placeholder := NULL]

combination_experiment[, complete_label := paste(label_exp, label_cm, algo_label, match_vars_label,
                                                 samp_schema_label, cores_label, sep = "_")]

rm(algo_df, match_vars_df, sampling_schemas, tech_restriction)

original_data.table_threads <- data.table::getDTthreads()

data.table::setorder(combination_experiment, cores_label, match_vars_label, samp_schema_label, algo_label, label_cm, label_exp)
