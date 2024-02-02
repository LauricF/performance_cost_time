determine_failed_group <- function(path_to_export_dt,affix,formula_v) {
  file_missing <- c()
  for(i in 1:length(formula_v)){
    formula_i <- str_split(formula_v[i],"~ ")[[1]][2]
    name_to_load <- paste0(affix,"_",formula_i)
    if(!file.exists(paste0(path_to_export_dt,"/tables/T_ROC",name_to_load,".csv"))){
    file_missing <- c(file_missing,formula_v[i])
    }
  }
  return(file_missing)
}

fail_model_formula <- determine_failed_group(path_to_export_dt =  path_to_export_dt,formula_v = formula_v, affix = "rfsrc_all")


n1 <- Sys.time()
fail_model_formula
subs= 1:length(fail_model_formula)
# for(i in subs){
foreach(i = subs, .packages = c('randomForestSRC', 'pec','timeROC','survival','SurvMetrics','rms','riskRegression','stringr','dplyr')) %dopar% {
  model <- create_models(model_2_fit = "rfsrc", formula_v = fail_model_formula[i], arg_l = list(x = TRUE, data = trainData))
  res <- measure_models_LF(models =  model,test_data = testData,prediction_f = prediction_LF,outcome = outcome, time = time,horizon_time_v = horizon_time_v,path_to_export_dt = path_to_export_dt, formula_v = fail_model_formula[i], band = FALSE, affix = "rfsrc_all")
}
Sys.time()- n1
