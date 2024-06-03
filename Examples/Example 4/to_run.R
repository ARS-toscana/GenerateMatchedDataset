rm(list=ls(all.names=TRUE))
source(file.path("R", "GenerateMatchedDataset.R"))
source(file.path("R", "GenerateMatchedDatasetNaive.R"))
source(file.path("R", "GenerateMatchedDatasetHT.R"))
unlink("Example 4/g_intermediate", recursive = T)
unlink("Example 4/g_output", recursive = T)
dir.create("Example 4/g_intermediate")
dir.create("Example 4/g_output/single_seed", recursive = T)
dir.create("Example 4/g_output/single_seed_retry", recursive = T)
dir.create("Example 4/g_output/different_seed", recursive = T)
exposed <- data.table::fread("test_with_simulated_data/exposed_1000.csv")
candidate_matches = data.table::fread("test_with_simulated_data/candidate_matches_1000.csv")

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
                       seeds_for_sampling = 123,
                       threshold = 2000,
                       temporary_folder = c("Example 4/g_intermediate"),
                       output_matching = c("Example 4/g_output/single_seed"))

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
                       seeds_for_sampling = 123,
                       threshold = 2000,
                       temporary_folder = c("Example 4/g_intermediate"),
                       output_matching = c("Example 4/g_output/single_seed_retry"))

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
                       seeds_for_sampling = 124,
                       threshold = 2000,
                       temporary_folder = c("Example 4/g_intermediate"),
                       output_matching = c("Example 4/g_output/different_seed"))

bootstrap_sample_seed <- qs::qread(file.path("Example 4/g_output/single_seed", paste0("bootstrap_", 1)))
bootstrap_sample_retry <- qs::qread(file.path("Example 4/g_output/single_seed_retry", paste0("bootstrap_", 1)))
bootstrap_sample_different <- qs::qread(file.path("Example 4/g_output/different_seed", paste0("bootstrap_", 1)))
data.table::setorder(bootstrap_sample_seed, person_id, i.person_id)
data.table::setorder(bootstrap_sample_retry, person_id, i.person_id)
data.table::setorder(bootstrap_sample_different, person_id, i.person_id)
identical(bootstrap_sample_seed, bootstrap_sample_retry)
all.equal(bootstrap_sample_seed, bootstrap_sample_retry)
identical(bootstrap_sample_seed, bootstrap_sample_different)
all.equal(bootstrap_sample_seed, bootstrap_sample_different)
