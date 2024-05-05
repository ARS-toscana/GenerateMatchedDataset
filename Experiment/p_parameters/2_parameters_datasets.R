
#----------------------------------------------------------------
# this parameter file defines the list of dataset names list_of_all_datasets that will be generated and used, and their properties. each name is used for a pair of dataset: exposed and candidate_matches (cm). 

# each dataset name has is composed by a label indicating how many exposed are generated in the exposed (list of such numbers is stored in the list df_size_exposed), and a label indicating how many candidate matches, as a factor (list of such factor stored in the list cm_factor). each df_size and each cm_factor has a label, and this is what is used to create the dataset name, for eaxample 1000 in df_size has label '1K', and 10 in cm_factor has label '10t'. 

# to each dataset name there are four lists associated: 
# df_size_exposed_ds        - df size of the exposed in the ds
# df_size_exposed_lab_ds    - label of df_size_exposed_ds 
# cm_factor_ds              - factor of candidate matches
# cm_factor_lab_ds          - label of cm_factor_ds




# {}

# list of all possible sizes of exposed
df_size_exposed <- c(100,200,1000,2000,10000,20000,100000,200000,1000000)
df_size_exposed_lab <- c("1h","2h","1K","2K","10K","20K","100K","200K","1KK")
df_size_exposed_name <- setNames(df_size_exposed,df_size_exposed_lab)
df_size_exposed_num <- list()
for (j in seq_along(df_size_exposed_lab) ){
  df_size_exposed_num[[df_size_exposed_lab[j]]] <- df_size_exposed[j]
}

# list of all possible factors to create candidate matches

cm_factor <- c(0.1,2,10,100,1000,10000,100000,1000000)
cm_factor_lab <- c("point1t","double","10t","100t","1Kt","10Kt","100Kt","1KKt")
cm_factor_name <- setNames(cm_factor,cm_factor_lab)
cm_factor_num <- list()
for (j in seq_along(cm_factor_lab) ){
  cm_factor_num[[cm_factor_lab[j]]] <- cm_factor[j]
}

#-------------------------------------
# all combinations

# all df_size are paired with small cm_factors
pairs <- list()
for (df_size in df_size_exposed_lab){
  for (cm in c("point1t","double","10t") ){
    pairs[[df_size]] <- c(pairs[[df_size]],cm)
  }
}
# small df_size are paired with large cm_factors
for (df_size in c("1h","2h","1K") ){
  for (cm_fac in cm_factor_lab ){
    pairs[[df_size]] <- unique(c(pairs[[df_size]],cm_fac))
  }
}


#-------------------------------------
# list of all datasets

list_of_all_datasets <- c()
df_size_exposed_ds <- list()
df_size_exposed_lab_ds <- list()
cm_factor_ds <- list()
cm_factor_lab_ds <- list()
for (df_size in df_size_exposed_lab){ 
  for (cm_fac in pairs[[df_size]]){ 
    ds <- paste0(df_size,"_",cm_fac)
    list_of_all_datasets <- c(list_of_all_datasets,ds)
    df_size_exposed_ds[[ds]] <- df_size_exposed_num[[cm_fac]]
    df_size_exposed_lab_ds[[ds]] <- df_size_exposed_lab[[cm_fac]]
    cm_factor_ds[[ds]] <- cm_factor_num[[cm_fac]]
    cm_factor_lab_ds[[ds]] <- cm_fac
  }
}
  

