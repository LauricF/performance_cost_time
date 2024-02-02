# load parameters for stage 2
parameters <- parameters_by_stage(stage = "stage2")
dt <- data_demographics_ogtt %>% 
  select(all_of(c(parameters$variables,parameters$time,parameters$outcome,"Draw.Date.x","AB_group","ADA_dysglycemia","DPTRS","DPTRS60","M120","CPH","LR"))) %>% 
  drop_na()
# select mAB normoglycemia stage 2

dt <- dt %>% filter(ADA_dysglycemia == "Dysglycemia" & !(AB_group %in% c("GAD","IAA","IA2A")))
trainData <-
  dt[dt$Draw.Date.x < "2012-02-20", ] %>% mutate_if(is.character, as.factor) %>%  select(all_of(c(parameters$variables,parameters$time,parameters$outcome,"DPTRS","DPTRS60","M120","CPH","LR"))) %>% stats::na.omit(all_of(parameters$variables)) %>% filter(!!rlang::sym(parameters$time) != 0)
testData <-
  dt[dt$Draw.Date.x >= "2012-02-20", ]  %>% mutate_if(is.character, as.factor) %>%  select(all_of(c(parameters$variables,parameters$time,parameters$outcome, "DPTRS","DPTRS60","M120","CPH","LR"))) %>% stats::na.omit(all_of(parameters$variables)) %>% filter(!!rlang::sym(parameters$time) != 0)
path_to_export_dt <-  paste0(here(),"/analysis_28_06")
# path_to_export_dt <-  here()
## coxph model
## 
parameters$variables <-  "GRS2"
# bonus_variables <- c("DPTRS","DPTRS60","M120","CPH","LR","DPTRS + GRS2","DPTRS60 + GRS2","M120 + GRS2","CPH + GRS2","LR + GRS2")
 bonus_variables <- c("DPTRS","DPTRS60","M120","CPH","LR",
"DPTRS + GRS2","DPTRS60 + GRS2","M120 + GRS2","CPH + GRS2","LR + GRS2",
"DPTRS + Hba1c","DPTRS60 + Hba1c",
"DPTRS + GRS2 + Hba1c","DPTRS60 + GRS2 + Hba1c")

fit_save_performancebis <- function(outcome  =  "T1D.Indicator",
                                 time = "time_since_first_screening",
                                 horizon_time_v = c(2, 3, 5, 7, 10),
                                 trainData,
                                 testData,
                                 bonus_variables = NULL,
                                 model_2_fit,
                                 path_to_export_dt, affix){
  
  response <-  paste0("Surv(",time,",", outcome,")")
  formula_v <-  paste(response,bonus_variables, sep = " ~ ")
  n1 <- Sys.time()
  if(model_2_fit == "coxph"){
    models <- create_models(model_2_fit = model_2_fit, formula_v = formula_v, arg_l = list(x = TRUE, data = trainData))
    res <- measure_models_LF(models =  models,test_data = testData,prediction_f = prediction_LF,outcome = outcome, time = time,horizon_time_v = horizon_time_v,path_to_export_dt = path_to_export_dt, formula_v = formula_v, band = FALSE, affix = affix)
    
  } else{
    no_cores <- detectCores()-1
    cl <- makeCluster(no_cores)
    registerDoParallel(cl)
    # foreach(i = 1:length(formula_v)) %dofuture% {
    foreach(i = 1:length(formula_v), .packages = c('randomForestSRC', 'pec','timeROC','survival','SurvMetrics','rms','riskRegression','stringr','dplyr','naniar','here')) %dopar% {
      source(paste0(here(),"/R/prediction_for_each_class.R"))
      source(paste0(here(),"/R/fit_models.R"))
      source(paste0(here(),"/R/f_model_performance.R"))
      source(paste0(here(),"/R/presence_in_formula.R"))
      model <- create_models(model_2_fit = "rfsrc", formula_v = formula_v[i], arg_l = list(x = TRUE, data = trainData))
      res <- measure_models_LF(models =  model,test_data = testData,prediction_f = prediction_LF,outcome = outcome, time = time,horizon_time_v = horizon_time_v,path_to_export_dt = path_to_export_dt, formula_v = formula_v[i], band = FALSE, affix = affix)
    }
    stopCluster(cl)
  }
  print(Sys.time()- n1)  
}

fit_save_performancebis(outcome  =  parameters$outcome,
                     time = parameters$time,
                     horizon_time_v = c(2, 3, 5, 7, 10),
                     trainData = trainData,
                     testData = testData,
                     bonus_variables = bonus_variables,
                     model_2_fit = "coxph",
                     path_to_export_dt = path_to_export_dt,
                     affix = "Cox_Stage2")                     

# rfsrc model
fit_save_performancebis(outcome  =  parameters$outcome,
                     time = parameters$time,
                     horizon_time_v = c(2, 3, 5, 7, 10),
                     trainData = trainData,
                     testData = testData,
                     bonus_variables = bonus_variables,
                     model_2_fit = "rfsrc",
                     path_to_export_dt = path_to_export_dt,
                     affix = "RF_Stage2")
