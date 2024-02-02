compute_TimeROC <- function(formula_to_compare,model_2_fit,trainData,testData,prediction_f,outcome, time,horizon_time = 3, idd = TRUE, path_to_export_dt = "/Users/laf225/Desktop/T1Dprediction_trialnet/R/") {
  model1 <- create_models(model_2_fit = model_2_fit, formula_v = formula_to_compare, arg_l = list(x = TRUE, data = trainData))[[1]]
  prediction_at_horizon_time <- do.call(prediction_f,arg = list(object = model1, testdata = testData, horizon_time = horizon_time[[1]]))
  ROC.1 <- timeROC(
    T = testData[[time]],
    delta = testData[[outcome]],
    marker = prediction_at_horizon_time[],
    cause = 1,
    times = horizon_time,
    ROC = TRUE,
    iid = idd
  )
  return(ROC.1)
}
