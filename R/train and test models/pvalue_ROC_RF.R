# path_to_export_dt <- paste0(here(),"/analysis_28_06/")
path_to_export_dt <-  here()
# stage 0 -----------------------------------------------------------------

# load parameters for stage 0
parameters <- parameters_by_stage(stage = "stage0")
formula_v <- create_formula(variables = parameters$variables, response = parameters$response, conditions = parameters$conditions)

outcome <- parameters$outcome
time <-parameters$time
variables <-parameters$variables
trainData <-trainData
testData <-testData
conditions <-parameters$condition
formula_to_compare <- paste0(parameters$response,"~",extract_best_formula(formula_v,affix ="RF_Stage0",path_to_export_dt =  path_to_export_dt, n = 1, horizon = 3))
res_0 <- compare_TimeROC_l(formula_to_compare = formula_to_compare,formula_v = formula_v,model_2_fit = "rfsrc",trainData = trainData,testData = testData,prediction_f = prediction_LF,outcome = outcome, time = time,horizon_time = c(3,5), idd = TRUE)
write.csv(x = res_0, file = paste0(path_to_export_dt,"tables/ROC_pvalue_RF_Stage0.csv"))



# stage 1 -----------------------------------------------------------------
# load parameters for stage 1
parameters <- parameters_by_stage(stage = "stage1")
formula_v <- create_formula(variables = parameters$variables, response = parameters$response, conditions = parameters$conditions)
# formula_v_best <- extract_best_formula(formula_v,affix ="RF_Stage0",path_to_export_dt =  here())
# formula_to_compare <- paste0(parameters$response,"~",formula_v_best[1])
# formula_v_to_compare  <-paste0(parameters$response,"~",formula_v_best[2:3])

outcome <- parameters$outcome
time <-parameters$time
variables <-parameters$variables
trainData <-trainData
testData <-testData
conditions <-parameters$condition
formula_to_compare <- paste0(parameters$response,"~",extract_best_formula(formula_v,affix ="RF_Stage1",path_to_export_dt =  path_to_export_dt, n = 1, horizon = 3))
res_1 <- compare_TimeROC_l(formula_to_compare = formula_to_compare,formula_v = formula_v,model_2_fit = "rfsrc",trainData = trainData,testData = testData,prediction_f = prediction_LF,outcome = outcome, time = time,horizon_time = c(3,5), idd = TRUE)
write.csv(x = res_1, file = paste0(path_to_export_dt,"tables/ROC_pvalue_RF_Stage1.csv"))

# stage 2 -----------------------------------------------------------------
# load parameters for stage 2
parameters <- parameters_by_stage(stage = "stage2")
formula_v <- create_formula(variables = parameters$variables, response = parameters$response, conditions = parameters$conditions)
# formula_v_best <- extract_best_formula(formula_v,affix ="RF_Stage0",path_to_export_dt =  here())
# formula_to_compare <- paste0(parameters$response,"~",formula_v_best[1])
# formula_v_to_compare  <-paste0(parameters$response,"~",formula_v_best[2:3])

outcome <- parameters$outcome
time <-parameters$time
variables <-parameters$variables
trainData <-trainData
testData <-testData
conditions <-parameters$condition
formula_to_compare <- paste0(parameters$response,"~",extract_best_formula(formula_v,affix ="RF_Stage2",path_to_export_dt =  path_to_export_dt, n = 1, horizon = 3))
res_2 <- compare_TimeROC_l(formula_to_compare = formula_to_compare,formula_v = formula_v,model_2_fit = "rfsrc",trainData = trainData,testData = testData,prediction_f = prediction_LF,outcome = outcome, time = time,horizon_time = c(3,5), idd = TRUE)
write.csv(x = res_1, file = paste0(path_to_export_dt,"tables/ROC_pvalue_RF_Stage1.csv"))