# model performance assessment
source("~/Desktop/T1Dprediction_trialnet/R/prediction_for_each_class.R")
source("~/Desktop/T1Dprediction_trialnet/R/f_model_performance.R")


outcome <- "T1D.Indicator"
time <- "time_since_first_screening"
variables <- c("GRS2", "GAD", "IAA", "IA2", "AB_group", "Participant.Sex", "age_at_first_drawing", "BMI",
               "lnT","Index60", "AUC_glucose", "AUC_ceptide")
formula <-
  as.formula(paste0(
    "Surv(",
    time,
    ", ",
    outcome,
    ") ~",
    paste(variables, collapse = "+")
  ))
horizon_time_v <- c(2, 3, 5, 7, 10)
# load data
data_demographics_ogtt <- data_demographics_ogtt %>% select(all_of(c(variables,time,outcome)))
trainData <-
  data_demographics_ogtt[data_demographics_ogtt$Draw.Date.x < "2012-02-20", ] %>% mutate_if(is.character, as.factor) %>%  select(all_of(c(variables, outcome, time))) %>% stats::na.omit(all_of(variables)) %>% filter(!!rlang::sym(time) != 0)
testData <-
  data_demographics_ogtt[data_demographics_ogtt$Draw.Date.x >= "2012-02-20", ]  %>% mutate_if(is.character, as.factor) %>%  select(all_of(c(variables, outcome, time))) %>% stats::na.omit(all_of(variables)) %>% filter(!!rlang::sym(time) != 0)




res.cox <- coxph(formula, x = TRUE, data = trainData)

# TROC_LF(model = res.cox,
#         test_data = data.frame(trainData),
#         prediction_f = predict,
#         outcome = outcome,
#         time = time,
#         horizon_time_v = horizon_time_v,
#         idd = FALSE)

# TROC_LF(model = res.cox,
#            test_data = trainData,
#            prediction_f = predict,
#            outcome = outcome,
#            time = time,
#            horizon_time_v = horizon_time_v,
#         idd = TRUE)


# rfsrc.model <- rfsrc(
#   formula,
#   ntree = 1000,
#   nodesize = 100,
#   forest = T,
#   ntime = horizon_time_v,
#   seed = -17072019,
#   data = data.frame(trainData)
# )
# 
# predict_rsfc <- function(object, newdata, time) {
#   res <-
#     predict(object = object,
#             newdata = data.frame(newdata),
#             type = "prob")
#   index.time.interest <- which.min(abs(res$time.interest - time))
#   marker_score <- 1 - res$survival[, index.time.interest]
#   return(marker_score)
# }

# TROC_LF(model = rfsrc.model,
#         test_data = trainData,
#         prediction_f = predict_rsfc,
#         outcome = outcome,
#         time = time,
#         horizon_time_v = horizon_time_v,
#         idd = FALSE)


# performance_measure_LF(measure_fun_l = list(TROC_LF), arg_l = list(
#   list(
#     model = rfsrc.model,
#     test_data = trainData,
#     prediction_f = predict_rsfc,
#     outcome = outcome,
#     time = time,
#     horizon_time_v = horizon_time_v,
#     idd = FALSE
#   )
# ))


# prediction_LF(rfsrc.model,
#               testdata = data.frame(testData),
#               horizon_time = 3)
# falsemodel <- rfsrc.model
# class(falsemodel) <-  "haha"
# prediction_LF(falsemodel,
#               testdata = data.frame(testData),
#               horizon_time = 3)


# prediction_LF(res.cox, testdata = testData, horizon_time = 3)
# 
# 
# CIndex_LF(
#   res.cox,
#   test_data = testData,
#   prediction_LF,
#   outcome,
#   time,
#   horizon_time = 3
# )
# CIndex_LF(
#   rfsrc.model,
#   test_data = testData,
#   prediction_LF,
#   outcome,
#   time,
#   horizon_time = 3
# )
# 
# 
# Brier_score_LF(
#   res.cox,
#   test_data = testData,
#   prediction_LF,
#   outcome,
#   time,
#   horizon_time = 3
# )
# Brier_score_LF(
#   rfsrc.model,
#   test_data = testData,
#   prediction_LF,
#   outcome,
#   time,
#   horizon_time = 3
# )
# 
# 
# IBS_score_LF(
#   res.cox,
#   test_data = testData,
#   prediction_LF,
#   outcome,
#   time,
#   horizon_time_v = seq(0.01, 10, 0.1)
# )
# IBS_score_LF(
#   rfsrc.model,
#   test_data = testData,
#   prediction_LF,
#   outcome,
#   time,
#   horizon_time_v = seq(0.01, 10, 0.5)
# )
# 
# IBS_score_LF(
#   rfsrc.model,
#   test_data = testData,
#   prediction_LF,
#   outcome,
#   time,
#   horizon_time_v = seq(0.01, 10, 0.5)
# )


# calibration_LF(rfsrc.model,
#                test_data = testData,
#                prediction_LF,
#                outcome,
#                time,
#                horizon_time = 3) 
# 
# calibration_v_LF(rfsrc.model,
#                  test_data = testData,
#                  prediction_LF,
#                  outcome,
#                  time,
#                  horizon_time_v = c(3,5))

# performance_measure_LF(measure_fun_l = list(TROC_LF, CIndex_LF,Brier_score_LF,IBS_score_LF,calibration_LF),
#                        arg_l = list(
#                          list(
#                            model = rfsrc.model,
#                            test_data = testData,
#                            prediction_f = prediction_LF,
#                            outcome = outcome,
#                            time = time,
#                            horizon_time_v = horizon_time_v,
#                            idd = FALSE
#                          ),
#                          list(
#                            model = rfsrc.model,
#                            test_data = testData,
#                            prediction_f = prediction_LF,
#                            outcome = outcome,
#                            time = time,
#                            horizon_time = 3
#                          ),
#                          list(
#                            model = rfsrc.model,
#                            test_data = testData,
#                            prediction_f = prediction_LF,
#                            outcome = outcome,
#                            time = time,
#                            horizon_time = 3
#                          ),
#                          list(
#                            model = rfsrc.model,
#                            test_data = testData,
#                            prediction_f = prediction_LF,
#                            outcome = outcome,
#                            time = time,
#                            horizon_time_v = seq(0.5,5,0.5)
#                          ),
#                          list(
#                            model = rfsrc.model,
#                            test_data = testData,
#                            prediction_f = prediction_LF,
#                            outcome = outcome,
#                            time = time,
#                            horizon_time = 3
#                          )
#                        ))


# res <- performance_measure_all( model = rfsrc.model,
#                          test_data = testData,
#                          prediction_f = prediction_LF,
#                          outcome = outcome,
#                          time = time,
#                          horizon_time_v = horizon_time_v)
# 
# 
# export_model_performance(model_performance = res, path_to_export_dt = here() , name_to_save = "test")


conditions <- c("!(lnT == TRUE & age_at_first_drawing == TRUE)", #to not have age in two ways
                "!(Index60 == TRUE & AUC_glucose == TRUE  & AUC_ceptide == TRUE)", #to not have strongly correlated variables simultaniously
                "!((AB_group == TRUE) & (GAD == TRUE | IAA == TRUE | IA2 == TRUE))", #to not have strongly correlated variables simultaniously
                "!(GRS2 == FALSE & age_at_first_drawing == FALSE & BMI  == FALSE & lnT  == FALSE & Index60  == FALSE & AUC_glucose  == FALSE & AUC_ceptide  == FALSE)") #to remove all 
response = "Surv(time_since_first_screening, T1D.Indicator)"
formula_v <- create_formula(variables = variables, response = response, conditions = conditions)
formula_v[1:10]
fit_model_LF("coxph",formula_v[1], x = TRUE, data = trainData)
fit_model_LF("rfsrc",formula_v[2], x = TRUE, data = trainData,nodesize = 100, forest = T, mtime = horizon_time_v)

rfsrc.model <- rfsrc(
  formula(formula_v[2]),
  ntree = 1000,
  nodesize = 100,
  forest = T,
  ntime = horizon_time_v,
  seed = -17072019,
  data = data.frame(trainData)
)

subs <- 1
models <- create_models(model_2_fit = "coxph", formula_v = formula_v[subs], arg_l = list(x = TRUE, data = trainData))


measure_models_LF(models = models[subs],test_data = testData,prediction_f = prediction_LF,outcome = outcome, time = time,horizon_time_v = horizon_time_v,path_to_export_dt = here(), formula_v = formula_v[subs], band = TRUE, affix = "test")

extract_performance_measure(path_to_export_dt,affix,formula_v)
