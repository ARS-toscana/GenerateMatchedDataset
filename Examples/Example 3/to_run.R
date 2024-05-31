rm(list=ls(all.names=TRUE))
source(file.path("R", "GenerateMatchedDataset.R"))
source(file.path("R", "GenerateMatchedDatasetNaive.R"))
source(file.path("R", "GenerateMatchedDatasetHT.R"))
unlink("Example 3/g_intermediate", recursive = T)
unlink("Example 3/g_output", recursive = T)
dir.create("Example 3/g_intermediate")
dir.create("Example 3/g_output")
exposed <- data.table::fread("test_with_simulated_data/exposed_1000.csv")
candidate_matches = data.table::fread("test_with_simulated_data/candidate_matches_1000.csv")

bench::mark({
  GenerateMatchedDataset(exposed = data.table::copy(exposed),
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
}, {
  GenerateMatchedDatasetNaive(exposed = data.table::copy(exposed),
                              candidate_matches = data.table::copy(candidate_matches),
                              unit_of_observation = c("person_id"),
                              time_variable_in_exposed = c("vax1_day"),
                              time_variables_in_candidate_matches = c("start", "end"),
                              variables_with_exact_matching = c("SES", "REGION"),
                              variables_with_range_matching = c("age"),
                              range_of_variables_with_range_matching = list(c(-1, 1)),
                              sample_size_per_exposed = 1,
                              number_of_bootstrapping_samples = 10,
                              threshold = 2000000000,
                              temporary_folder = c("Example 3/g_intermediate"),
                              output_matching = c("Example 3/g_output"))
}, {
  GenerateMatchedDatasetHT(exposed = data.table::copy(exposed),
                           candidate_matches = data.table::copy(candidate_matches),
                           unit_of_observation = c("person_id"),
                           time_variable_in_exposed = c("vax1_day"),
                           time_variables_in_candidate_matches = c("start", "end"),
                           variables_with_exact_matching = c("SES", "REGION"),
                           variables_with_range_matching = c("age"),
                           range_of_variables_with_range_matching = list(c(-1, 1)),
                           sample_size_per_exposed = 1,
                           number_of_bootstrapping_samples = 10,
                           threshold = 2000000000,
                           temporary_folder = c("Example 3/g_intermediate"),
                           output_matching = c("Example 3/g_output"))
}, min_iterations = 1)

bootstrap_sample_original <- qs::qread(file.path("Example 3/g_output", paste0("bootstrap_", 1)))
bootstrap_sample_naive <- qs::qread(file.path("Example 3/g_output", paste0("bootstrap_naive_", 1)))
bootstrap_sample_HT <- qs::qread(file.path("Example 3/g_output", paste0("bootstrap_HT_", 1)))
data.table::setorder(bootstrap_sample_original, person_id, i.person_id)
data.table::setorder(bootstrap_sample_naive, person_id, i.person_id)
data.table::setorder(bootstrap_sample_HT, person_id, i.person_id)
identical(bootstrap_sample_original, bootstrap_sample_naive)
all.equal(bootstrap_sample_original, bootstrap_sample_naive)
identical(bootstrap_sample_original, bootstrap_sample_HT)
all.equal(bootstrap_sample_original, bootstrap_sample_HT)
