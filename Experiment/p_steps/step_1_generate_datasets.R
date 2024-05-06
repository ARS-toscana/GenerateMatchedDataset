# generate datasets to be used in the experiments

list_of_exp_datasets <- unique(unlist(combination_experiment[,.(label_exp)]))
# list_of_exp_datasets <- c("1h")

for (ds in list_of_exp_datasets){
  
  # Set seed for reproducibility
  set.seed(1234)
  
  
  # retrieve the size of the dataset of exposed
  df_size <- unique(unlist(combination_experiment[label_exp == ds,.(exp)]))
  # Define parameters for age bands (left limits included)
  age_band_limits <- c(18, 25, 35, 45, 65, 85)
  # if you want to include an additional ageband from the last limit onwards, set the parameter add_older_ages to TRUE, otherwise to FALSE
  add_older_ages <- T
  # Define probabilities for age bands (the length of this vector must be )
  age_band_probabilities <- c(0.1, 0.18, 0.24, 0.23, 0.15, 0.1)
  
  # Define parameters for time0
  time0_min <- 1
  time0_max <- 720
  time0_age_band_means <- c(400, 390, 380, 200, 50, 50)
  time0_age_band_sd <- c(50,50, 50, 100, 20, 20)
  time0_comorbidity_cluster_prob <- 0.05
  time0_comorbidity_cluster_mean <- 50
  time0_comorbidity_cluster_sd <- 10
  
  # Define probabilities for COMORBIDITY based on ageband
  frequency_COM <- c(0.01, 0.02, 0.1, 0.15, 0.2, 0.3)
  
  # Define num of values for categorical variables
  categorical_variables <- c("EXACT_UNIF","EXACT_NORM")
  numvalues <- vector(mode="list")
  numvalues[["EXACT_UNIF"]] <- 9
  numvalues[["EXACT_NORM"]] <- 9
  
  # sets the complete list of intervals for age bands, excluding possibly the last
  if (add_older_ages) {
    age_band_limits_complete <- c(age_band_limits,100)
  } else {
    age_band_limits_complete <- age_band_limits 
  }
  
  number_age_bands <- length(age_band_limits)
  age_band_values <- age_band_limits
  
  # check that the probabilities for age are correct
  if (length(age_band_probabilities) != number_age_bands){
    stop(paste0("The number of probabilities you assigned in -age_band_probabilities- is ", length(age_band_probabilities),
                ", while the number of age bands you have set is ", number_age_bands,
                ". In -age_band_probabilities- you must set as many probabilities as the number of age bands"))
  }
  sum_probabilities <- sum(age_band_probabilities)
  if (sum_probabilities != 1){ 
    stop(paste0("The probabilities you assigned in -age_band_probabilities- add up to ", sum_probabilities,
                ", while they should add up to 1"))
  }
  
  # store the name of the datasets of exposed 
  name_dataset_exposed <- paste0("exposed_",ds)

  # retrieve list of datasets of cm_matches associated with ds
  list_of_cm_fac_labs <- unique(unlist(combination_experiment[label_exp == ds,.(label_cm)]))
  
  occurrence_unexposed <- list()
  name_dataset_candidate_matches <- list()
  cm_fac <- list()
  for (cm_fac_lab in list_of_cm_fac_labs){ 
  # Define occurrence of cases of unexposed, per ageband: the higher the number, the larger the amount of unexposed persons in the candidate_matches dataset for that ageband
  cm_fac[[cm_fac_lab]] <- combination_experiment[label_exp == ds & label_cm == cm_fac_lab,.(cm)][[1]]
  cm <- cm_fac[[cm_fac_lab]]
  occurrence_unexposed[[cm_fac_lab]] <- c(cm, cm, cm, cm, cm, cm)
  
  # check that the occurrence of unexposed are correct
  if (length(occurrence_unexposed[[cm_fac_lab]]) != number_age_bands){
    stop(paste0("The number of occurrence of unexposed you assigned in -occurrence_unexposed- for factor ",cm_fac_lab," is " ,
                length(occurrence_unexposed[[cm_fac_lab]]), ", while the number of age bands you have set is ", number_age_bands,
                ". In -age_band_probabilities- you must set occurrence of unexposed as many times as the number of age bands"))
  }
  
  # store the name of the datasets candidate_matches
  name_dataset_candidate_matches[[cm_fac_lab]] <- paste0("candidate_matches_",ds,"_",cm_fac_lab)
  }
  
  
  
  ###################################################################
  
  
  
  ##%######################################################%##
  #                                                          #
  ####           CREATE THE DATASET OF EXPOSED            ####
  #                                                          #
  ##%######################################################%##
  
  # Create an empty data frame
  df_data <- data.table::data.table(person_id = 1:df_size, 
                                    age = 0, 
                                    # COMORBIDITY = 0, 
                                    time0 = NA, 
                                    EXACT_UNIF = NA,
                                    EXACT_NORM = NA
  )
  
  # set ageband labels
  age_band_labels <- c()
  for (i in 1:(length(age_band_limits)-1))  {
    age_band_labels <- c(age_band_labels, paste0(age_band_limits[i], "-", age_band_limits[i+1] - 1))
  }
  if (add_older_ages){
    age_band_labels <- c(age_band_labels, paste0(age_band_limits[length(age_band_limits)], "+"))
  }
  
  # Generate agebands based on the distribution
  df_data[, Nageband := sample(1:number_age_bands, df_size, replace = TRUE, prob = age_band_probabilities)]
  
  # Assign Ages Within Bands
  df_data[, age := sapply(Nageband, function(x) {
    lower_limit <- age_band_limits_complete[x]
    upper_limit <- age_band_limits_complete[x + 1] 
    sample(seq(lower_limit, upper_limit), 1)
  })]
  
  # Create a categorical variable based on age_band_labels
  df_data[, ageband := age_band_labels[Nageband]]
  
  # # Print distribution of age
  # print(prop.table(table(df_data$age)))
  # print(prop.table(table(df_data$ageband)))
  # print(age_band_probabilities)
  
  # Generate time0 variable based on age bands, means, and standard deviations
  df_data$time0 <- mapply(function(mean, sd) { 
      round(rnorm(1, mean, sd))}, 
      time0_age_band_means[df_data$Nageband], time0_age_band_sd[df_data$Nageband])

  
  # Clip the values to be within the specified range [time0_min, time0_max]
  df_data$time0 <- pmin(pmax(df_data$time0, time0_min), time0_max)
  
  
  # Generate random values for the categorical variable uniformly distributed
  random_values <- sample(1:numvalues[["EXACT_UNIF"]], nrow(df_data), replace = TRUE)
  df_data[,EXACT_UNIF := as.factor(random_values)]
  
  # Generate random values for the categorical variable fìnormally distributed
  df_data[,EXACT_NORM := round(rnorm(nrow(df_data),5,1))]
  df_data[EXACT_NORM < 1,EXACT_NORM := 1]
  df_data[EXACT_NORM > 9,EXACT_NORM := 9]
  
  
  # this is the dataset of exposed
  exposed <- df_data
  
  for (cm_fac_lab in list_of_cm_fac_labs){ 
    # DATASET OF CANDIDATE MATCHES
    
    df_data <- copy(exposed)
    
    # consider first the exposed before exposure (only those whose time0 > 1)
    candidate_matches <- df_data[df_data$time0 > 1,]
    
    # Add new variables start and end to the data dataframe
    candidate_matches$start <- 1
    candidate_matches$end <- candidate_matches$time0 - 1
    
    # Generate random real numbers in a new variable in candidate_matches, as a base for duplication occurrence_unexposed times
    candidate_matches[, additional_unexposed := runif(nrow(candidate_matches), min = 0, max = occurrence_unexposed[[cm_fac_lab]][candidate_matches$Nageband])]
    candidate_matches[, probability := additional_unexposed - floor(additional_unexposed)]
    candidate_matches[, add:= rbinom(1, 1 , prob = probability), by = seq_len(nrow(candidate_matches)) ]
    candidate_matches[,additional_unexposed := floor(additional_unexposed) + add ]
    candidate_matches <- subset(candidate_matches, select = c(-add,-probability))
    
    # Filter records where additional_unexposed > 0
    filtered_candidate_matches <- candidate_matches[candidate_matches$additional_unexposed > 0, ]
    # modify person_id
    maxgroup <- max(ceiling(occurrence_unexposed[[cm_fac_lab]]))
    filtered_candidate_matches$person_id_new <- (filtered_candidate_matches$person_id + df_size) * maxgroup
    
    # Create candidate_matches_add with a counter across copies
    candidate_matches_add <- filtered_candidate_matches[rep(1:.N, additional_unexposed)][, counter := 1:.N, by = person_id]
    rm(filtered_candidate_matches)
    # modify person_id and verify it's a unique identifier
    
    candidate_matches_add$person_id_new <- candidate_matches_add$person_id_new + candidate_matches_add$counter
    has_duplicates <- any(duplicated(candidate_matches_add$person_id_new))
    # Print the result
    if (has_duplicates) {
      print("person_id contains duplicates.")
    } else {
      print("person_id is a unique identifier.")
    }
    candidate_matches_add$person_id <- candidate_matches_add$person_id_new
    
    # remove vaccination date and move end to the end of study period
    candidate_matches_add$time0 <- NA
    candidate_matches_add$end <- 720
    #clean up
    candidate_matches_add <- subset(candidate_matches_add, select = c(-counter,-person_id_new))
    
    
    # bind the two dataframes
    
    candidate_matches <- rbind(candidate_matches,candidate_matches_add)
    rm(candidate_matches_add)
    candidate_matches <- subset(candidate_matches,select = -additional_unexposed)
    candidate_matches <-  subset(candidate_matches, select = c(-ageband,-Nageband))
    write.csv(candidate_matches, paste0(dirdatasets,"/",name_dataset_candidate_matches[[cm_fac_lab]],".csv"), row.names = FALSE)
  }
  
  #remove data
  
  rm(df_data)
  
  # clean exposed and candidate_matches
  
  exposed <-  subset(exposed, select = c(-ageband,-Nageband))
  
  
  
  # save the datasets in csv format
  
  
  write.csv(exposed, paste0(dirdatasets,"/",name_dataset_exposed,".csv"), row.names = FALSE)
 
  #####################################################
}
