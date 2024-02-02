# parameters_by_stage <- function(stage){
#   time = "time_since_first_screening"
#   outcome = "T1D.Indicator"
#   response <-  "Surv(time_since_first_screening, T1D.Indicator)"
#   variables <- c("GRS2","IA2", "AB_group", "Sex", "age_at_first_drawing", "BMI",
#                  "lnT","Index60", "AUC_glucose", "AUC_ceptide")
#   if(stage == "stage0"){
#     conditions <- c("!(lnT == TRUE & age_at_first_drawing == TRUE)", #to not have age in two ways
#                     "!(Index60 == TRUE & AUC_glucose == TRUE  & AUC_ceptide == TRUE)", #to not have strongly correlated variables simultaniously
#                     "!(AB_group == TRUE & IA2 == TRUE)", #to not have strongly correlated variables simultaniously
#                     "!(GRS2 == FALSE & age_at_first_drawing == FALSE & BMI  == FALSE & lnT  == FALSE & Index60  == FALSE & AUC_glucose  == FALSE & AUC_ceptide  == FALSE)") #to remove all 
#   }
#   if(stage == "stage1"){
#     conditions <- c("!(lnT == TRUE & age_at_first_drawing == TRUE)", #to not have age in two ways
#                     "!(Index60 == TRUE & AUC_glucose == TRUE  & AUC_ceptide == TRUE)", #to not have strongly correlated variables simultaniously
#                     "!(AB_group == TRUE & IA2 == TRUE)",
#                     "!(GRS2 == FALSE & age_at_first_drawing == FALSE & BMI  == FALSE & lnT  == FALSE & Index60  == FALSE & AUC_glucose  == FALSE & AUC_ceptide  == FALSE)") #to remove all 
#   }
#   if(stage == "stage2"){
#     conditions <- c("!(lnT == TRUE & age_at_first_drawing == TRUE)", #to not have age in two ways
#                     "!(Index60 == TRUE & AUC_glucose == TRUE  & AUC_ceptide == TRUE)", #to not have strongly correlated variables simultaniously
#                     "!(AB_group == TRUE & IA2 == TRUE)",
#                     "!(GRS2 == FALSE & age_at_first_drawing == FALSE & BMI  == FALSE & lnT  == FALSE & Index60  == FALSE & AUC_glucose  == FALSE & AUC_ceptide  == FALSE)") #to remove all 
#   }
#   parameters = list(variables = variables, conditions = conditions, response = response, time = time, outcome = outcome)
#   return(parameters)
# }

parameters_by_stage <- function(stage){
  time = "time_since_first_screening"
  outcome = "T1D.Indicator"
  response <-  "Surv(time_since_first_screening, T1D.Indicator)"
  variables <- c("GRS2","IA2", "AB_group", "Sex", "age_at_first_drawing", "BMI",
                 "lnT","Index60", "AUC_glucose", "AUC_ceptide","Z_BMI","C_peptide_index_30","beta2_score","Hba1c")
  string1 <- "Index60 == FALSE"
  string2 <- "C_peptide_index_30 == FALSE"
  string3 <- "AUC_ceptide == FALSE"
  string4 <- "beta2_score == FALSE"
  
  # Generate combinations
  combinations <- combinations(4, 2, c(string1, string2, string3, string4), set = TRUE)
  # combinations
  # Concatenate the strings for each permutation
  concatenated_strings <- apply(combinations, 1, paste, collapse = " | ")
  avoid_metabolic_repetition <- paste0("(",paste(concatenated_strings, collapse = ") & ("),")")
  at_least_one_numeric <- "!(GRS2 == FALSE & age_at_first_drawing == FALSE &  Z_BMI == FALSE & BMI  == FALSE & lnT  == FALSE & Index60  == FALSE & AUC_glucose  == FALSE & AUC_ceptide == FALSE & C_peptide_index_30 == FALSE & beta2_score == FALSE & Hba1c == FALSE)" #to remove all 
  
  if(stage == "stage0"){
    conditions <- c("!(lnT == TRUE & age_at_first_drawing == TRUE)", #to not have age in two ways
                    avoid_metabolic_repetition,
                    "!(AB_group == TRUE & IA2 == TRUE)", #to not have strongly correlated variables simultaniously
                    "!(beta2_score == TRUE & Hba1c == TRUE)", # because Hba1C is in beta2 score formula
                    "!(Z_BMI == TRUE & BMI  == TRUE)",
                    at_least_one_numeric) # to avoid problem of calibration
  }
  if(stage == "stage1"){
    conditions <- c("!(lnT == TRUE & age_at_first_drawing == TRUE)", #to not have age in two ways
                    avoid_metabolic_repetition,
                    "!(AB_group == TRUE & IA2 == TRUE)",
                    "!(Z_BMI == TRUE & BMI  == TRUE)",
                    "!(beta2_score == TRUE & Hba1c == TRUE)", # because Hba1C is in beta2 score formula
                    at_least_one_numeric) # to avoid problem of calibration
  }
  if(stage == "stage2"){
    conditions <- c("!(lnT == TRUE & age_at_first_drawing == TRUE)", #to not have age in two ways
                    avoid_metabolic_repetition,
                    "!(beta2_score == TRUE & Hba1c == TRUE)", # because Hba1C is in beta2 score formula
                    "!(AB_group == TRUE & IA2 == TRUE)",
                    "!(Z_BMI == TRUE & BMI  == TRUE)",
                    at_least_one_numeric) # to avoid problem of calibration
  }

  parameters = list(variables = variables, conditions = conditions, response = response, time = time, outcome = outcome)
  return(parameters)
}



