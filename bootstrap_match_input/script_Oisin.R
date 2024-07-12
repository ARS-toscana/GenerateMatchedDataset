# Install package
remotes::install_github("ARS-toscana/GenerateMatchedDataset")
library(matchboots)

# Parameter: main folder name
if (!require("rstudioapi")) install.packages("rstudioapi")
directory_name <- setwd(dirname(rstudioapi::getSourceEditorContext()$path))
directory_name <- setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Create results folders
dir.create(file.path(directory_name, "g_intermediate"), recursive = T, showWarnings = F)
dir.create(file.path(directory_name, "g_output"), recursive = T, showWarnings = F)

# Load controls and exposed
controls <- data.table::data.table(readRDS(file.path(directory_name, "Controls.RDS")))
exposed <- data.table::data.table(readRDS(file.path(directory_name, "Exposed.RDS")))

# Define end of eligibility for controls as min of end of spell and date of first vaccination
controls[, end_of_eligibility := pmin(VAC_DATE1, EN2, na.rm = T)]

# Remove ineligible spells: vaccination before ST2
controls <- controls[end_of_eligibility >= ST2, ]

# Main function
GenerateMatchedDataset(exposed = exposed,
                       candidate_matches = controls,
                       unit_of_observation = c("person_id"),
                       time_variable_in_exposed = c("FIRST_PFIZER"),
                       time_variables_in_candidate_matches = c("ST2", "end_of_eligibility"),
                       variables_with_exact_matching = c("I_COVID_COV", "IM_IMC_COV", "L_GEOREGION_COV",
                                                         "L_SOCIOECO_COV", "V_CDC_COV", "INF"),
                       variables_with_range_matching = c("YEAR_BIRTH"),
                       range_of_variables_with_range_matching = list(c(-1, 1)),
                       sample_size_per_exposed = 1,
                       methodology_for_bootstrapping = "SUoO",
                       number_of_bootstrapping_samples = 500,
                       threshold = 2000000000,
                       temporary_folder = file.path(directory_name, "g_intermediate"),
                       output_matching = file.path(directory_name, "g_output"))

# As an example here it is how the results can be loaded
matched <- qs::qread(file.path(directory_name, "g_output", "no_bootstrap"))
