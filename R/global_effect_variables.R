find_variable_effect <- function(formula_v,affix,path_to_export_dt =  here(),variable_of_interest, horizon = 3){
  res_dt <- extract_performance_measure(path_to_export_dt =  path_to_export_dt,formula_v = formula_v, affix = affix)
  tAUC <- res_dt[[1]]
  tAUC <- tAUC %>% filter(horizon_time == horizon)
  C_dt <-res_dt[[2]]
  B_dt <-res_dt[[3]]
  
  plot_dt <- C_dt %>% left_join(tAUC) %>% left_join(B_dt)
  
  pattern <- paste0(pattern = " \\+ |",variable_of_interest)
  plot_dt <- presence_in_formula(plot_dt, variable_of_interest)
  plot_dt$group_variable <- gsub(pattern = pattern, replacement = "", x =  plot_dt$formula) # remove formula operation
  
  # remove formula which do not have a twin
  dups <- duplicated(plot_dt[,"group_variable"])
  group_variables_dulpicated <-  plot_dt[ which(dups),"group_variable"] %>% pull()
  plot_dt <- plot_dt[plot_dt$group_variable %in% group_variables_dulpicated,]
  

  x = plot_dt[plot_dt[,variable_of_interest] == TRUE ,"AUC"]
  y = plot_dt[plot_dt[,variable_of_interest] == FALSE ,"AUC"]
  
  test <- t.test(x = x, y = y)
  return(test)
}

res <- find_variable_effect(formula_v, affix= "Cox_Stage0", path_to_export_dt =  path_to_export_dt,variable_of_interest = "GRS2", horizon = 3)
res

# # Cox_Stage0
# names_variables <- c("GRS2","AB_group", "IA2", "Sex","lnT","age_at_first_drawing","Z_BMI","BMI", "Index60","AUC_glucose", "AUC_ceptide", "Z_BMI", "C_peptide_index_30", "beta2_score", "Hba1c")
# for(i in 1:length(names_variables)){
#   res <- find_variable_effect(formula_v, affix= "Cox_Stage0", path_to_export_dt =  path_to_export_dt,variable_of_interest = names_variables[i], horizon = 3)
#   print(names_variables[i])
#   print(res)
# }
# 
# # Cox_Stage1
# names_variables <- c("GRS2","AB_group", "IA2", "Sex","lnT","age_at_first_drawing","Z_BMI","BMI", "Index60","AUC_glucose", "AUC_ceptide", "Z_BMI", "C_peptide_index_30", "beta2_score", "Hba1c")
# for(i in 1:length(names_variables)){
#   res <- find_variable_effect(formula_v, affix= "Cox_Stage1", path_to_export_dt =  path_to_export_dt,variable_of_interest = names_variables[i], horizon = 3)
#   print(names_variables[i])
#   print(res)
# }
# 
# # Cox_Stage2
# names_variables <- c("GRS2","AB_group", "IA2", "Sex","lnT","age_at_first_drawing","Z_BMI","BMI", "Index60","AUC_glucose", "AUC_ceptide", "Z_BMI", "C_peptide_index_30", "beta2_score", "Hba1c")
# for(i in 1:length(names_variables)){
#   res <- find_variable_effect(formula_v, affix= "Cox_Stage2", path_to_export_dt =  path_to_export_dt,variable_of_interest = names_variables[i], horizon = 3)
#   print(names_variables[i])
#   print(res)
# }


for(i in c(0,1,2)){
  res <- find_variable_effect(formula_v, affix= paste0("Cox_Stage",i), path_to_export_dt =  path_to_export_dt,variable_of_interest = "GRS2", horizon = 7)
  print( paste0("Cox_Stage",i))
  print(res)
  print(res$estimate[1]-res$estimate[2])
}

for(i in c(0,1,2)){
  res <- find_variable_effect(formula_v, affix= paste0("Cox_Stage",i), path_to_export_dt =  path_to_export_dt,variable_of_interest = "Z_BMI", horizon = 7)
  print( paste0("Cox_Stage",i))
  print(res)
  print(res$estimate[1]-res$estimate[2])
}
table(res$difference)


table(res_dt$difference, res_dt$C_peptide_index_30)

table(res_dt$difference, res_dt$GRS2)


table(res_dt$difference, res_dt$AB_group)

table(res_dt$difference, res_dt$Sex)

table(res_dt$difference, res_dt$lnT)

table(res_dt$difference, res_dt$AUC_glucose)

table(res_dt$difference, res_dt$Z_BMI)

table(res_dt$difference, res_dt$Hba1c)

table(res_dt$difference, res_dt$beta2_score)


res_dt %>% filter(C_peptide_index_30 == TRUE) %>% arrange(diff_RF_minus_COX) %>% tail() %>% View()

data(veteran, package = "randomForestSRC")
v.obj <- rfsrc(Surv(time_since_first_screening, T1D.Indicator) ~ age_at_first_drawing + BMI + C_peptide_index_30 , trainData, nsplit = 10, ntree = 100)

## partial effect of age on mortality
partial.obj <- partial(v.obj,
                       partial.type = "mort",
                       partial.xvar = "C_peptide_index_30",
                       partial.values = v.obj$xvar$C_peptide_index_30,
                       partial.time = v.obj$time.interest)
pdta <- get.partial.plot.data(partial.obj)

plot(lowess(pdta$x, pdta$yhat, f = 1/3))


## partial effects of karnofsky score on survival
C_peptide_index_30 <- quantile(v.obj$xvar$C_peptide_index_30)
partial.obj <- partial(v.obj,
                       partial.type = "surv",
                       partial.xvar = "C_peptide_index_30",
                       partial.values = C_peptide_index_30,
                       partial.time = v.obj$time.interest)
pdta <- get.partial.plot.data(partial.obj)

matplot(pdta$partial.time, t(pdta$yhat), type = "l", lty = 1,
        xlab = "time", ylab = "karnofsky adjusted survival")
legend("topright", legend = paste0("C_peptide_index_30 = ", C_peptide_index_30), fill = 1:5)
