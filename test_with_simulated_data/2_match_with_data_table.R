rm(list=ls(all.names=TRUE))


#set the directory where the script is saved as the working directory
if (!require("rstudioapi")) install.packages("rstudioapi")
thisdir <- setwd(dirname(rstudioapi::getSourceEditorContext()$path))
thisdir <- setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# load packages
if (!require("data.table")) install.packages("data.table")
library(data.table)
if (!require("dplyr")) install.packages("dplyr")
library(dplyr)


# Store in a variable the size of the dataset
df_size <- 1000

# names of the datasets of exposed and candidate_matches, and of the matched
name_dataset_exposed <- paste0("exposed_",as.character(df_size))
name_dataset_candidate_matches <- paste0("candidate_matches_",as.character(df_size))
name_dataset_matched <- paste0("matched_",as.character(df_size))

# assign matching criteria

exact_matching <- "SES, REGION"

range_matching <- c("age")

matching_rule_for_range <- vector(mode="list")

matching_rule_for_range[['age']] <- c(1,1)


#-----------------------------
# START MATCHING
# load datasets

exposed <- fread(paste0(thisdir,"/",name_dataset_exposed,".csv") )
candidate_matches <- fread(paste0(thisdir,"/",name_dataset_candidate_matches,".csv") )


# compose match string

preamble <- "person_id != i.person_id & vax1_day >= start & vax1_day < end"

matching_strings <- c("")

for (matching_variable in range_matching){ 
    matching_strings <- c(matching_strings, paste0( "between(",matching_variable,",",matching_variable," - ",matching_rule_for_range[[matching_variable]][2], " ,",matching_variable," + ",matching_rule_for_range[[matching_variable]][1],")") )
  }

matching_string <- paste(preamble,paste(matching_strings, collapse = " & "))

print(matching_string)

# match

dataset_matched  <- eval(parse(text = sprintf("exposed[candidate_matches, allow.cartesian = TRUE, on = .(%s)][%s]", exact_matching, matching_string)))

# save the datasets in csv format

write.csv(dataset_matched, paste0(thisdir,"/",name_dataset_matched,".csv"), row.names = FALSE)

#####################################################

