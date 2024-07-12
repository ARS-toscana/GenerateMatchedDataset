rm(list=ls(all.names=TRUE))
source(file.path("R", "GenerateMatchedDataset.R"))
unlink("Examples/Example 5/g_intermediate", recursive = T)
unlink("Examples/Example 5/g_output", recursive = T)
dir.create("Examples/Example 5/g_intermediate")
dir.create("Examples/Example 5/g_output", recursive = T)
exposed <- data.table::fread("Examples/test_with_simulated_data/exposed_1000.csv")
candidate_matches = data.table::fread("Examples/test_with_simulated_data/candidate_matches_1000.csv")

GenerateMatchedDataset(exposed = data.table::copy(exposed),
                       candidate_matches = data.table::copy(candidate_matches),
                       unit_of_observation = c("person_id"),
                       time_variable_in_exposed = c("vax1_day"),
                       time_variables_in_candidate_matches = c("start", "start"),
                       variables_with_exact_matching = c("SES", "REGION"),
                       variables_with_range_matching = c("age"),
                       range_of_variables_with_range_matching = list(c(-1, 1)),
                       sample_size_per_exposed = 1,
                       seeds_for_sampling = 123,
                       threshold = 2000,
                       temporary_folder = c("Examples/Example 5/g_intermediate"),
                       output_matching = c("Examples/Example 5/g_output"))

bootstrap_sample <- qs::qread(file.path("Examples/Example 5/g_output", paste0("bootstrap_", 1)))
data.table::setorder(bootstrap_sample, person_id, i.person_id)
