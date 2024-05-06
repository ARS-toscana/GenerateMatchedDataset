# TODO Comment on what file do
# TODO add us 4 and programming taskforce

# Clean environment and load functions
rm(list=ls(all.names=TRUE))
source(file.path("R", "GenerateMatchedDataset.R"))
source(file.path("R", "GenerateMatchedDatasetNaive.R"))
source(file.path("R", "GenerateMatchedDatasetHT.R"))

# Set folder
folder <- "Experiment"

# Delete old results
unlink(file.path(folder, "g_intermediate"), recursive = T)
unlink(file.path(folder, "g_output"), recursive = T)
unlink(file.path(folder, "g_results"), recursive = T)
unlink(file.path(folder, "g_datasets"), recursive = T)
# Create again folders
dir.create(file.path(folder, "g_intermediate"))
dir.create(file.path(folder, "g_output"))
dir.create(file.path(folder, "g_results"))
dir.create(file.path(folder, "g_datasets"))

# Set the experiment parameters
source(file.path(folder, "p_parameters", "1_general_parameters.R"))
source(file.path(folder, "p_parameters", "2_parameters_datasets.R"))
source(file.path(folder, "p_parameters", "3_generate_complete_experiment_combinations.R"))

# Generate datasets
source(file.path(folder, "p_steps", "step_1_generate_datasets.R"))
source(file.path(folder, "p_steps", "step_2_run_simulations.R"))
source(file.path(folder, "p_steps", "step_3_run_analysis.R"))
source(file.path(folder, "p_steps", "step_4_tables_and_figures.R"))

rm(folder)
