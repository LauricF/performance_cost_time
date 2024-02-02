
# stage 0 -----------------------------------------------------------------

# load parameters for stage 0
parameters <- parameters_by_stage(stage = "stage0")
formula_v <- create_formula(variables = parameters$variables, response = parameters$response, conditions = parameters$conditions)

outcome <- parameters$outcome
time <-parameters$time
variables <-parameters$variables
horizon_time_v <-c(2, 3, 5, 7, 10)
trainData <-trainData
testData <-testData
conditions <-parameters$condition
res <- compare_TimeROC_l(formula_to_compare = formula_to_compare,formula_v = formula_v,model_2_fit = "coxph",trainData = trainData,testData = testData,prediction_f = prediction_LF,outcome = outcome, time = time,horizon_time = c(3,5), idd = TRUE)
write.csv(x = res, file = "ROC_pvalue_Cox_Stage0.csv")



# stage 1 -----------------------------------------------------------------
# load parameters for stage 1
parameters <- parameters_by_stage(stage = "stage1")
formula_v <- create_formula(variables = parameters$variables, response = parameters$response, conditions = parameters$conditions)
# formula_v_best <- extract_best_formula(formula_v,affix ="Cox_Stage0",path_to_export_dt =  here())
# formula_to_compare <- paste0(parameters$response,"~",formula_v_best[1])
# formula_v_to_compare  <-paste0(parameters$response,"~",formula_v_best[2:3])

outcome <- parameters$outcome
time <-parameters$time
variables <-parameters$variables
horizon_time_v <-c(2, 3, 5, 7, 10)
trainData <-trainData
testData <-testData
conditions <-parameters$condition
res <- compare_TimeROC_l(formula_to_compare = formula_to_compare,formula_v = formula_v,model_2_fit = "coxph",trainData = trainData,testData = testData,prediction_f = prediction_LF,outcome = outcome, time = time,horizon_time = c(3,5), idd = TRUE)
write.csv(x = res, file = "ROC_pvalue_Cox_Stage1.csv")