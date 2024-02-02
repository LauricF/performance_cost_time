variables <- c("AUC_glucose","AUC_ceptide","Hba1c","Index60","C_peptide_index_30","beta2_score","DPTRS(?!\\d)","DPTRS60","M120", "CPH","LR","GRS2", "Sex", "age_at_first_drawing", "BMI",
               "lnT","Z_BMI" )
patient_time <- c( 120 + 45, 120+ 45, 10, 60+ 45, 30+ 45, 10, 120+ 45, 60+ 45, 120+ 45, 90+ 45, 90+ 45, 10, 10, 10, 10, 10, 10)

# variables <- c("AUC_glucose","AUC_ceptide","Hba1c","Index60","C_peptide_index_30","beta2_score", "GRS2" )
# total_time <- c( 130, 130, 10, 70,40,10, 10)/60
OGTT_needed <- c(TRUE,TRUE,FALSE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE, FALSE,FALSE,FALSE,FALSE,FALSE,FALSE)

data_time <- data.frame(variables, patient_time, OGTT_needed)

time_needed_f <- function(formula, data_time){
  presence_in_formula <- data_time %>% rowwise() %>%  filter(grepl(variables,formula, perl = TRUE)) %>% select(patient_time)
  if(dim(presence_in_formula)[1] != 0){ # presence of at least one metabolic variables
    res <- max(presence_in_formula$patient_time)
  }else{ # absence of any metabolic variables
    res <-  0
  }
  return(res)
}

OGTT_needed_f <- function(formula, data_time){
  presence_in_formula <- data_time %>% rowwise() %>%  filter(grepl(variables,formula, perl = TRUE)) %>% select(OGTT_needed)
  if(dim(presence_in_formula)[1] != 0){ # presence of at least one metabolic variables
    res <- max(presence_in_formula$OGTT_needed)
  }else{ # absence of any metabolic variables
    res <-  FALSE
  }
  return(res)
}

# cost_metabolic_f(formula = "", data_metabolic_cost, cost = c(16.79/4,16.79/4,16.79/4,16.79/4,20.81,20.81,20.81,20.81,9.71,14.88))

time_all_formula <- function(formula_v, data_time){
 
 time_v <- sapply(formula_v, function(x) time_needed_f(x, data_time = data_time))
 visit_v <- sapply(formula_v, function(x) OGTT_needed_f(x, data_time = data_time))
  res_dt <- data.frame(formula = formula_v, time = time_v, visit = visit_v)
  return(res_dt)
}


# cost_estimate_all_formula2(formula_v = c("GRS2", "GRS2+Index60+Hba1c"), cost_l)
# time_all_formula(formula_v = c("GRS2", "GRS2+Hba1c", "AUC_glucose","AUC_ceptide","Hba1c + AUC_ceptide","Index60","C_peptide_index_30","beta2_score", "GRS2", "DPTRS","DPTRS60"), data_time)
# 
 time_all_formula(formula_v = c("DPTRS"," DPTRS + GRS2" ), data_time)
#   