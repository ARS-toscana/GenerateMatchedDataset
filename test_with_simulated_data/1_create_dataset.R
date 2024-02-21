rm(list=ls(all.names=TRUE))

if (!require("dplyr")) install.packages("dplyr")
library(dplyr)

#-------------------------------
# Create dataset 

# Store in a variable the size of the dataset
df_size <- 1000

# Define parameters for age bands (left limits included)
age_band_limits <- c(18, 25, 35, 45, 65, 85)
# if you want to include an additional ageband from the last limit onwards, set the parameter add_older_ages to TRUE, otherwise to FALSE
# TODO what to do in case FALSE and there are persons over 85?
add_older_ages <- T
# Define probabilities for age bands (the length of this vector must be )
age_band_probabilities <- c(0.1, 0.18, 0.24, 0.23, 0.15, 0.1)

# Define parameters for vax1_day
vax1_day_min <- 1
vax1_day_max <- 720
vax1_day_age_band_means <- c(400, 390, 380, 200, 50, 50)
vax1_day_age_band_sd <- c(50,50, 50, 100, 20, 20)
vax1_day_comorbidity_cluster_prob <- 0.05
vax1_day_comorbidity_cluster_mean <- 50
vax1_day_comorbidity_cluster_sd <- 10

# Define probabilities for COMORBIDITY based on ageband
frequency_COM <- c(0.01, 0.02, 0.1, 0.15, 0.2, 0.3)

# Define num of values for categorical variables
categorical_variables <- c("SES","REGION")
numvalues <- vector(mode="list")
numvalues[["SES"]] <- 4
numvalues[["REGION"]] <- 10

# Define occurrence of cases of unexposed, per ageband: the higher the number, the larger the amount of unexposed persons in the candidate_matches dataset for that ageband
occurrence_unexposed <- c(4.0, 0.4, 0.2, 0.01, 0.01, 0.001)

# store the name of the datasets of exposed and candidate_matches
name_dataset_exposed <- paste0("exposed_",as.character(df_size))
name_dataset_candidate_matches <- paste0("candidate_matches_",as.character(df_size))

# Set seed for reproducibility
set.seed(1234)


###################################################################

#set the directory where the script is saved as the working directory
if (!require("rstudioapi")) install.packages("rstudioapi")
thisdir <- setwd(dirname(rstudioapi::getSourceEditorContext()$path))
thisdir <- setwd(dirname(rstudioapi::getSourceEditorContext()$path))

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

# check that the frequencies for comorbidity are correct
if (length(frequency_COM) != number_age_bands){
  stop(paste0("The number of frequencies you assigned in -frequency_COMs- is ", length(frequency_COM),
              ", while the number of age bands you have set is ", number_age_bands,
              ". In -age_band_probabilities- you must set as many frequencies as the number of age bands"))
}
sum_freq <- sum(frequency_COM)
if (sum_freq > 1){
  stop(paste0("The sum of the frequencies in -frequency_COMs- is larger than 1, please reset"))
}
# check that the occurrence of unexposed are correct
if (length(occurrence_unexposed) != number_age_bands){
  stop(paste0("The number of occurrence of unexposed you assigned in -occurrence_unexposed- is ",length(occurrence_unexposed),", while the number of age bands you have set is ",number_age_bands,". In -age_band_probabilities- you must set occurrence of unexposed as many times as the number of age bands"))
}
for (freq in frequency_COM){
  if (freq > 1){
    stop(paste0("One of the frequencies in -frequency_COMs- is larger than 1, please reset"))
  }
}


########################################
# CREATE THE DATASET OF EXPOSED

# Create an empty data frame
data <- data.frame(person_id = 1:df_size, 
                   age = rep(0, df_size), 
                   COMORBIDITY = rep(0, df_size), 
                   vax1_day = rep(NA, df_size), 
                   SES = rep(NA, df_size),
                   REGION = rep(NA, df_size)
)


# set ageband labels
age_band_labels <- c()
for (i in 1:(length(age_band_limits) - 1))  {
  age_band_labels <- c(age_band_labels,paste0(as.character(age_band_limits[i]),"-",as.character(age_band_limits[i+1] - 1)) )
}
if (add_older_ages){
  age_band_labels <- c(age_band_labels,paste0(as.character(age_band_limits[length(age_band_limits)]),"+"))
}

# Generate agebands based on the distribution
data$Nageband <- sample(1:number_age_bands, df_size, replace = TRUE, prob = age_band_probabilities)

# Assign Ages Within Bands
data$age <- sapply(data$Nageband, function(x) {
  lower_limit <- age_band_limits_complete[x]
  upper_limit <- age_band_limits_complete[x + 1] 
  sample(seq(lower_limit, upper_limit), 1)
})

# Create a categorical variable based on age_band_labels
data$ageband <- age_band_labels[data$Nageband]


# Print distribution of age
print(prop.table(table(data$age)))
print(prop.table(table(data$ageband)))
print(age_band_probabilities)


# Generate binary variable COMORBIDITY based on probabilities
data$COMORBIDITY <- sapply(data$Nageband, function(x) {
  probability <- frequency_COM[x]
  sample(c(0, 1), 1, prob = c(1 - probability, probability))
})

# Calculate prevalence rate of COMORBIDITY for each ageband
prevalence_by_ageband <- tapply(data$COMORBIDITY, data$ageband, function(x) sum(x) / length(x))
print(prevalence_by_ageband)


# Generate vax1_day variable based on age bands, means, and standard deviations
data$vax1_day <- mapply(function(mean, sd, comorbidity) {
  if (comorbidity == 1) {
    # If COMORBIDITY == 1, introduce additional parameters
    cluster_prob <- vax1_day_comorbidity_cluster_prob
    cluster_mean <- vax1_day_comorbidity_cluster_mean
    cluster_sd <- vax1_day_comorbidity_cluster_sd
    
    # Determine if the individual is part of the comorbidity cluster
    in_cluster <- sample(c(0, 1), 1, prob = c(1 - cluster_prob, cluster_prob))
    
    # Generate vax1_day value based on the comorbidity cluster
    if (in_cluster == 1) {
      round(rnorm(1, cluster_mean, cluster_sd))
    } else {
      round(rnorm(1, mean, sd))
    }
  } else {
    # If COMORBIDITY == 0, use the regular parameters
    round(rnorm(1, mean, sd))
  }
}, vax1_day_age_band_means[data$Nageband], vax1_day_age_band_sd[data$Nageband], data$COMORBIDITY)


# Identify a small sample of records with Nageband == 2 and lower vax1_day
sample_size <- round(0.05 * sum(data$Nageband == 2))
lower_vax1_day_indices <- sample(which(data$Nageband == 2), size = sample_size)

# Update the vax1_day values for the selected records
data$vax1_day[lower_vax1_day_indices] <- round(rnorm(sample_size, 80, 30))


# Clip the values to be within the specified range [vax1_day_min, vax1_day_max]
data$vax1_day <- pmin(pmax(data$vax1_day, vax1_day_min), vax1_day_max)


# assign now the categorical variables
for (cate in categorical_variables) {
  # Generate random values for the categorical variable
  random_values <- sample(1:numvalues[[cate]], nrow(data), replace = TRUE)
  
  # Assign the values to the dataframe
  data[[cate]] <- as.factor(random_values)
}

# this is the dataset of exposed
exposed <- data

# DATASET OF CANDIDATE MATCHES


# consider first the exposed before exposure (only those whose vax1_day > 1)

candidate_matches <- data[data$vax1_day > 1,]

# Add new variables start and end to the data dataframe
candidate_matches$start <- 1
candidate_matches$end <- candidate_matches$vax1_day - 1

# Generate random real numbers for a new variable in candidate_matches
candidate_matches$additional_unexposed <- round(runif(nrow(candidate_matches), min = 0, max = occurrence_unexposed[candidate_matches$Nageband]))

# Filter records where additional_unexposed > 0
filtered_candidate_matches <- candidate_matches[candidate_matches$additional_unexposed > 0, ]
# modify person_id
maxgroup <- max(ceiling(occurrence_unexposed))
filtered_candidate_matches$person_id_new <- (filtered_candidate_matches$person_id + df_size) * maxgroup

# Create candidate_matches_add with a counter across copies
candidate_matches_add <- filtered_candidate_matches %>%
  group_by(person_id) %>%
  slice(rep(1:n(), additional_unexposed)) %>%
  mutate(counter = row_number())
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
candidate_matches_add$vax1_day <- NA
candidate_matches_add$end <- 720
#clean up
candidate_matches_add <- subset(candidate_matches_add, select = c(-counter,-person_id_new))


# bind the two dataframes

candidate_matches <- rbind(candidate_matches,candidate_matches_add)
rm(candidate_matches_add)
candidate_matches <- subset(candidate_matches,select = -additional_unexposed)

#remove data

rm(data)

# clean exposed and candidate_matches

exposed <-  subset(exposed, select = c(-ageband,-Nageband))
candidate_matches <-  subset(candidate_matches, select = c(-ageband,-Nageband))


# save the datasets in csv format


write.csv(exposed, paste0(thisdir,"/",name_dataset_exposed,".csv"), row.names = FALSE)
write.csv(candidate_matches, paste0(thisdir,"/",name_dataset_candidate_matches,".csv"), row.names = FALSE)
#####################################################

