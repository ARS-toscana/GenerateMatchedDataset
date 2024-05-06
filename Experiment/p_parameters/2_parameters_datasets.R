
#----------------------------------------------------------------
# this parameter file defines the list of dataset names list_of_all_datasets that will be generated and used, and their properties. each name is used for a pair of dataset: exposed and candidate_matches (cm). 

# each dataset name has is composed by a label indicating how many exposed are generated in the exposed (list of such numbers is stored in the list df_size_exposed), and a label indicating how many candidate matches, as a factor (list of such factor stored in the list cm_factor). each df_size and each cm_factor has a label, and this is what is used to create the dataset name, for example 1000 in df_size has label '1k', and 10 in cm_factor has label '10t'. 

# to each dataset name there are four columns of the dataset pairs_df associated: 
# exp         - df size of the exposed in the ds
# label_exp   - label of df_size_exposed_ds 
# cm          - factor of candidate matches
# label_cm    - label of cm_factor_ds

# Restrict datasets creation in case bigger ones are not necessary (values ranges from 0 to 4)
level_restriction <- 3

# list of all possible sizes of exposed
df_size_exposed <- c(100L, 200L, 1000L, 2000L, 10000L, 20000L, 100000L, 200000L, 1000000L)
df_size_exposed_lab <- c("1h", "2h", "1k", "2k", "10k", "20k", "100k", "200k", "1M")
names(df_size_exposed) <- df_size_exposed_lab
rm(df_size_exposed_lab)
df_size_exposed[1:(length(df_size_exposed) - data.table::fcase(level_restriction == 0, 0,
                                                               level_restriction == 1, 1,
                                                               level_restriction == 2, 3,
                                                               level_restriction == 3, 5,
                                                               level_restriction == 4, 7))]

# list of all possible factors to create candidate matches
cm_factor <- c(0.1, 2, 10, 100, 1000, 10000, 100000, 1000000)
cm_factor <- as.numeric(format(cm_factor, scientific = FALSE))
cm_factor_lab <- c("1dt","2t","10t","100t","1kt","10kt","100kt","1Mt")
names(cm_factor) <- cm_factor_lab
cm_factor[1:(length(cm_factor) - level_restriction)]

#-------------------------------------
# all combinations

# all df_size are paired with small cm_factors
combination_experiment <- data.table::data.table(label_exp = character(), label_cm = character(),
                                                 exp = numeric(), cm = numeric())

for (i in 1:length(df_size_exposed)) {
  combination_experiment <- data.table::rbindlist(list(combination_experiment,
                                                       data.table::data.table(label_exp = names(df_size_exposed)[[i]],
                                                                              label_cm = cm_factor_lab,
                                                                              exp = df_size_exposed[[i]],
                                                                              cm = cm_factor[cm_factor_lab])))
  if (i %% 2 == 0) {
    cm_factor_lab <- cm_factor_lab[-length(cm_factor_lab)]
  }
}

rm(df_size_exposed, cm_factor, cm_factor_lab, i)

