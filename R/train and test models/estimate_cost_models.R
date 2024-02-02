cost_estimate_one_formula <- function(formula, cost_dt) {
  cost <- 0
  for(i in 1:dim(cost_dt)[1]){
    if(grepl(cost_dt[i,"variable"], formula, perl = TRUE)){
      cost <- cost + cost_dt[i,"cost"]
    }
  }
  return(cost)
}

cost_estimate_all_formula <- function(formula_v, cost_dt){
  cost_v <- sapply(formula_v, function(x) cost_estimate_one_formula(x, cost_dt))
  res_dt <- data.frame(formula = formula_v, cost = cost_v)
  return(res_dt)
}

# https://diabetesjournals.org/care/article/26/9/2536/22506/Costs-of-Screening-for-Pre-diabetes-Among-U-S
# AUC_glucose and AUC_ceptide cost
ogtt_cost <- 3/4*51+15.96*1/12+17.22+1+8*3.25+7
index60_cost <- 3/4*51/2+15.96*1/12+17.22/2+1+8*3.25/2+7
variable <-  c("GRS2", "GAD", "IAA", "IA2", "Participant.Sex", "age_at_first_drawing", "BMI",
               "lnT","^(?!.*AUC_glucose)(?!.*AUC_ceptide).*Index60*$","AUC_glucose|AUC_ceptide")
cost <- c(20, 1, 1, 1, 1, 0, 3,1,index60_cost, ogtt_cost)


cost_dt <- data.frame(variable, cost)

# res <- cost_estimate_all_formula(formula_v, cost_dt)

# variables <- c("AUC_glucose","AUC_ceptide","Hba1c","Index60","C_peptide_index_30","beta2_score" )
# # variables <- c("AUC glucose","AUC Cpeptide","Hba1c", "Index60", "Cpep30","Beta2 score")
# Gt0 <- c(1,0,0,0,1,1)
# Gt1 <- c(1,0,0,0,1,0)
# Gt2 <- c(1,0,0,1,0,0)
# Gt3 <- c(1,0,0,0,0,0)
# Cpept0 <-c(0,1,0,1,1,1) 
# Cpept1 <- c(0,1,0,0,1,0)
# Cpept2 <- c(0,1,0,1,0,0)
# Cpept3 <- c(0,1,0,0,0,0)
# Hba1c <- c(0,0,1,0,0,1)
# patient_time <- c( 2.25, 2.25, 0.15,1.25,0.6,0.4)
# 
# data_metabolic_cost <- data.frame(variables, Gt0,Gt1,Gt2,Gt3,Cpept0,Cpept1,Cpept2,Cpept3,Hba1c,patient_time)

variables <- c("AUC_glucose","AUC_ceptide","Hba1c","Index60","C_peptide_index_30","beta2_score","DPTRS(?!\\d)","DPTRS60","M120", "CPH","LR" )

G0 <- c(1,0,0,0,1,1,0,0,0,0,0)
G30 <- c(1,0,0,0,1,0,1,0,0,0,0)
G60 <- c(1,0,0,1,0,0,1,1,0,0,0)
G90 <- c(1,0,0,0,0,0,1,0,0,1,1)
G120 <- c(1,0,0,0,0,0,1,0,1,0,0)
Cpep0 <-c(0,1,0,1,1,1,1,1,0,0,0) 
Cpep30 <- c(0,1,0,0,1,0,1,0,0,0,0)
Cpep60 <- c(0,1,0,1,0,0,1,1,0,0,0)
Cpep90 <- c(0,1,0,0,0,0,1,0,0,0,0)
Cpep120 <- c(0,1,0,0,0,0,1,0,1,0,0)
Hba1c <- c(0,0,1,0,0,1,0,0,1,1,1)
# patient_time <- c( 130, 130, 10, 70, 40, 10, 130, 70, 130, 100, 100)
patient_time <- c( 120 + 45, 120+ 45, 10, 60+ 45, 30+ 45, 10, 120+ 45, 60+ 45, 120+ 45, 90+ 45, 90+ 45)
physician_time <- c( 3/4, 3/4, 1/5, 1/2, 2/5, 1/5, 3/4, 1/2, 3/4, 2/5, 2/5)

data_metabolic_cost <- data.frame(variables, G0, G30, G60, G90, G120, Cpep0, Cpep30, Cpep60, Cpep90, Cpep120,Hba1c,patient_time, physician_time)

compute_cost <- function(row_selected, cost_lab = c(20.71/5,20.71/5,20.71/5,20.71/5,20.71/5,20.81,20.81,20.81,20.81,20.81,9.71)) {
  res <- sum(row_selected[1:11]*cost_lab) + row_selected[12]*14.88/60 + row_selected[13]*66.08 + 1/12*19.84 + 12.4 # the last bit is uncompresible cost of  admin + trip
  return(res)
}
 # compute_cost(data_metabolic_cost[2,-1], cost = c(16.79/4,16.79/4,16.79/4,16.79/4,20.81,20.81,20.81,20.81,9.71,14.88))

cost_multiple_metabolic <- function(selected_variables,cost = c(20.71/5,20.71/5,20.71/5,20.71/5,20.71/5,20.81,20.81,20.81,20.81,20.81,9.71)) {
  row_summarised <- do.call(pmax,data.frame(t(selected_variables)))
  res <- compute_cost(row_summarised,cost)
  return(res)
}
# cost_multiple_metabolic(data_metabolic_cost[1:6,-1], cost = c(16.79/4,16.79/4,16.79/4,16.79/4,20.81,20.81,20.81,20.81,9.71,14.88))


cost_metabolic_f <- function(formula, data_metabolic_cost, cost = c(20.71/5,20.71/5,20.71/5,20.71/5,20.71/5,20.81,20.81,20.81,20.81,20.81,9.71)){
  presence_in_formula <- data_metabolic_cost %>% rowwise() %>%  filter(grepl(variables,formula, perl = TRUE))
  if(dim(presence_in_formula)[1] != 0){ # presence of at least one metabolic variables
  res <- cost_multiple_metabolic(presence_in_formula[,-1], cost = cost)
  }else{ # absence of any metabolic variables
  res <-  0
  }
  return(res)
}

# cost_metabolic_f(formula = "", data_metabolic_cost, cost = c(16.79/4,16.79/4,16.79/4,16.79/4,20.81,20.81,20.81,20.81,9.71,14.88))

cost_estimate_one_formula2 <- function(formula, cost_dt, data_metabolic_cost, cost_metabolic = c(20.71/5,20.71/5,20.71/5,20.71/5,20.71/5,20.81,20.81,20.81,20.81,20.81,9.71)) {
  cost <- 0
  # simple cost ( without metabolic)
  
  for(i in 1:dim(cost_dt)[1]){
    if(grepl(cost_dt[i,"variable"], formula, perl = TRUE)){
      cost <- cost + cost_dt[i,"cost"]
    }
  }
  # more advanced cost ( with metabolic)
  cost_metabolic <- cost_metabolic_f(formula = formula, data_metabolic_cost = data_metabolic_cost, cost = cost_metabolic)
  cost <- cost + cost_metabolic
  return(cost)
}
# variable <-  c("GRS2")
# cost <- c(20)
# cost_dt <- data.frame(variable, cost)
# cost_estimate_one_formula2(formula = "AUC_ceptide+GRS2", cost_dt = cost_dt, data_metabolic_cost = data_metabolic_cost, cost_metabolic = c(16.79/4,16.79/4,16.79/4,16.79/4,20.81,20.81,20.81,20.81,9.71,14.88))


cost_estimate_all_formula2 <- function(formula_v, cost_l){
  cost_dt <- cost_l$cost_dt
  data_metabolic_cost <- cost_l$data_metabolic_cost
  cost_metabolic <- cost_l$cost_metabolic
  cost_v <- sapply(formula_v, function(x) cost_estimate_one_formula2(x, cost_dt = cost_dt, data_metabolic_cost = data_metabolic_cost, cost_metabolic = cost_metabolic))
  res_dt <- data.frame(formula = formula_v, cost = cost_v)
  return(res_dt)
}

variable <-  c("GRS2","age_at_first_drawing|BMI|lnT|Z_BMI")
cost <- c(1/5*66.08 + 1/12*19.84 + 20  + 10/60*14.88,  1/5*66.08 + 1/12*19.84 + 10/60*14.88)
cost_dt <- data.frame(variable, cost)
# cost_l <- list(cost_dt = cost_dt, data_metabolic_cost = data_metabolic_cost, cost_metabolic = c(16.79/4,16.79/4,16.79/4,16.79/4,20.81,20.81,20.81,20.81,9.71,14.88))


cost_l <- list(cost_dt = cost_dt, data_metabolic_cost = data_metabolic_cost, cost_metabolic = c(20.71/5,20.71/5,20.71/5,20.71/5,20.71/5,20.81,20.81,20.81,20.81,20.81,9.71))
# 
 cost_estimate_all_formula2(formula_v = c("GRS2", "GRS2+Index60+Hba1c", "AUC_glucose","C_peptide_index_30","beta2_score","DPTRS", "DPTRS + GRS2","DPTRS60","M120", "CPH","LR"), cost_l)

 