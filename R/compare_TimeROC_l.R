compare_TimeROC_l <- function(formula_to_compare,formula_v,model_2_fit,trainData,testData,prediction_f,outcome, time,horizon_time = 3, idd = TRUE, path_to_export_dt = "/Users/laf225/Desktop/T1Dprediction_trialnet/R/") {
  n_comparison <- length(formula_v)
  res_l <- vector("list", n_comparison)
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
  n1 <- Sys.time()

  if(n_comparison < 10){
    for(i in 1:n_comparison){
      model2 <- create_models(model_2_fit = model_2_fit, formula_v = formula_v[i], arg_l = list(x = TRUE, data = trainData))[[1]]
      prediction_at_horizon_time <- do.call(prediction_f,arg = list(object = model2, testdata = testData, horizon_time = horizon_time[1]))
      ROC.2 <- timeROC(
        T = testData[[time]],
        delta = testData[[outcome]],
        marker = prediction_at_horizon_time,
        cause = 1,
        times = horizon_time,
        ROC = TRUE,
        iid = idd)
      formula_i <- str_split(formula_v[i],"~")[[1]][2]
      compare_i <-c(compare(ROC.1, ROC.2)$p_values_AUC) #compute p-values of comparison tests
        dt <- data.frame(pvalue = compare_i, year = horizon_time, formula = formula_i)
      res_l[[i]] <- dt 
    }
  } else{
    no_cores <- detectCores()-1
    cl <- makeCluster(no_cores)
    registerDoParallel(cl)
    res_l <- foreach(i = 1:n_comparison, .packages = c('randomForestSRC', 'pec','timeROC','survival','SurvMetrics','rms','riskRegression','stringr','dplyr','naniar')) %dopar% {
      source(paste0(path_to_export_dt,"prediction_for_each_class.R"))
      source(paste0(path_to_export_dt,"fit_models.R"))
      source(paste0(path_to_export_dt,"f_model_performance.R"))
      model2 <- create_models(model_2_fit = model_2_fit, formula_v = formula_v[i], arg_l = list(x = TRUE, data = trainData))[[1]]
      prediction_at_horizon_time <- do.call(prediction_f,arg = list(object = model2, testdata = testData, horizon_time = horizon_time[1]))
      ROC.2 <- timeROC(
        T = testData[[time]],
        delta = testData[[outcome]],
        marker = prediction_at_horizon_time,
        cause = 1,
        times = horizon_time,
        ROC = TRUE,
        iid = idd)
      formula_i <- str_split(formula_v[i],"~ ")[[1]][2]
      compare_i <-c(compare(ROC.1, ROC.2)$p_values_AUC) #compute p-values of comparison tests
      dt <- data.frame(pvalue = compare_i, year = horizon_time, formula = formula_i)
      # res_l[[i]] <- dt 
    }
    stopCluster(cl)
  }
  print(Sys.time()- n1)  
  res <- do.call("rbind",res_l)
  return(res)
}


# # load parameters for stage 0
# parameters <- parameters_by_stage(stage = "stage0")
# formula_v_best <- extract_best_formula(formula_v,affix ="Cox_Stage0",path_to_export_dt =  here())
# formula_to_compare <- paste0(parameters$response,"~",formula_v_best[1])
# formula_v_to_compare  <-paste0(parameters$response,"~",formula_v_best[2:3])
# 
# outcome <- parameters$outcome
# time <-parameters$time
# variables <-parameters$variables
# horizon_time_v <-c(2, 3, 5, 7, 10)
# trainData <-trainData
# testData <-testData
# conditions <-parameters$condition
# res <- compare_TimeROC_l(formula_to_compare = formula_to_compare,formula_v = formula_v_to_compare,model_2_fit = "coxph",trainData = trainData,testData = testData,prediction_f = prediction_LF,outcome = outcome, time = time,horizon_time = c(3,5), idd = TRUE)

