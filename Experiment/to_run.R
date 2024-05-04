# TODO Comment on what file do
# TODO add us 4 and programming taskforce

# Clean environment and load functions
rm(list=ls(all.names=TRUE))
source(file.path("R", "GenerateMatchedDataset.R"))
source(file.path("R", "GenerateMatchedDatasetNaive.R"))
source(file.path("R", "GenerateMatchedDatasetHT.R"))

# TODO parametrize folder
# Delete old results
unlink("Experiment/g_intermediate", recursive = T)
unlink("Experiment/g_output", recursive = T)

# Delete old results
dir.create("Experiment/g_intermediate")
dir.create("Experiment/g_output")

# Set the experiment parameters

list_of_experiments <- c("threshold2_pop1000_cm0.1_matching2_tech0_samplingcomplete","threshold3_pop1000_cm0.1_matching2_tech0_samplingcomplete")

threshold[["threshold2_pop1000_cm0.1_matching2_tech0_samplingcomplete"]] <- "max among frequency of bin capacity"
pop[["threshold2_pop1000_cm0.1_matching2_tech0_samplingcomplete"]] <- 1000
cm[["threshold2_pop1000_cm0.1_matching2_tech0_samplingcomplete"]] <- 0.1
matching[["threshold2_pop1000_cm0.1_matching2_tech0_samplingcomplete"]] <- 2
tech[["threshold2_pop1000_cm0.1_matching2_tech0_samplingcomplete"]] <- 0
sampling[["threshold2_pop1000_cm0.1_matching2_tech0_samplingcomplete"]] <- "complete"

for (experiment in list_of_experiments){
  # Load simulated datasets
  exposed <- data.table::fread("test_with_simulated_data/exposed_1000.csv")
  candidate_matches = data.table::fread("test_with_simulated_data/candidate_matches_1000.csv")
  
  # 
  
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
}