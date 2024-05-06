for (i in nrow(pairs_df)){
  
  # Load simulated datasets
  exposed <- data.table::fread("test_with_simulated_data/exposed_1000.csv")
  candidate_matches = data.table::fread("test_with_simulated_data/candidate_matches_1000.csv")
  
  single_row <- pairs_df[1, ]
  single_row <- single_row[, complete_label := paste0(label_exp, label_cm)]
  
  # 
  bnch <- bench::mark(
    do.call(
      what = "GenerateMatchedDataset",
      args = list(exposed = data.table::copy(exposed),
                  candidate_matches = data.table::copy(candidate_matches),
                  unit_of_observation = c("person_id"),
                  time_variable_in_exposed = c("vax1_day"),
                  time_variables_in_candidate_matches = c("start", "end"),
                  variables_with_exact_matching = c("SES", "REGION"),
                  variables_with_range_matching = c("age"),
                  range_of_variables_with_range_matching = list(c(-1, 1)),
                  sample_size_per_exposed = 1,
                  number_of_bootstrapping_samples = 10,
                  threshold = 8000,
                  temporary_folder = c("Example 3/g_intermediate"),
                  output_matching = c("Example 3/g_output"))
    ), min_iterations = 10)
  
  bnch <- bnch |> dplyr::bind_cols(single_row)
  
  saveRDS(bnch, file.path(folder, "g_results", paste0(single_row[, complete_label], ".rds")))
  
  rm(bnch, single_row)
}