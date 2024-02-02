 path_to_export_dt <- paste0(here(),"/analysis_28_06")
# path_to_export_dt <-  here()
bonus_variables =c("DPTRS","DPTRS60","M120","CPH","LR")
# bonus_variables =NULL
# stage 0 -----------------------------------------------------------------

# load parameters for stage 0
parameters <- parameters_by_stage(stage = "stage0")
formula_v <- create_formula(variables = parameters$variables, response = parameters$response, conditions = parameters$conditions, bonus_variables = bonus_variables)

outcome <- parameters$outcome
time <-parameters$time
variables <-parameters$variables
trainData <-trainData
testData <-testData
conditions <-parameters$condition
formula_to_compare <- paste0(parameters$response,"~",extract_best_formula(formula_v,affix ="Cox_Stage0",path_to_export_dt =  path_to_export_dt, n = 1, horizon = 3))
res_0 <- compare_TimeROC_l(formula_to_compare = formula_to_compare,formula_v = formula_v,model_2_fit = "coxph",trainData = trainData,testData = testData,prediction_f = prediction_LF,outcome = outcome, time = time,horizon_time = c(3,5), idd = TRUE)
write.csv(x = res_0, file = paste0(path_to_export_dt,"/tables/ROC_pvalue_Cox_Stage0.csv"))
dt <- read.csv("tables/ROC_pvalue_Cox_Stage0.csv")
gg <- plot_with_pvalue(formula_v,affix ="Cox_Stage0",path_to_export_dt =  path_to_export_dt, horizon = 3, title ="Model performances compared to the best model", pvalue_v = 0.5)
ggplotly(gg)
gg$data %>% filter(AUC- max(gg$data$AUC) > -0.03 & pvalue > 0.05) %>% dim
ROC_best <- compute_TimeROC(formula_to_compare = formula_to_compare,model_2_fit = "coxph",trainData = trainData,testData = testData,prediction_f = prediction_LF,outcome = outcome, time = time,horizon_time = c(3,5), idd = TRUE)
confint(ROC_best)
# stage 1 -----------------------------------------------------------------
# load parameters for stage 1
# 
 dt <- data_demographics_ogtt %>% 
select(all_of(c(parameters$variables,parameters$time,parameters$outcome,"Draw.Date.x","AB_group","ADA_dysglycemia", "DPTRS","DPTRS60","M120","CPH","LR"))) %>% 
  drop_na()

# select mAB normoglycemia stage 1

dt <- dt %>% filter(ADA_dysglycemia == "Euglycemia" & !(AB_group %in% c("GAD","IAA","IA2A")))
trainData <-
  dt[dt$Draw.Date.x < "2012-02-20", ] %>% mutate_if(is.character, as.factor) %>%  select(all_of(c(parameters$variables,parameters$time,parameters$outcome,  "DPTRS","DPTRS60","M120","CPH","LR"))) %>% stats::na.omit(all_of(parameters$variables)) %>% filter(!!rlang::sym(parameters$time) != 0)
testData <-
  dt[dt$Draw.Date.x >= "2012-02-20", ]  %>% mutate_if(is.character, as.factor) %>%  select(all_of(c(parameters$variables,parameters$time,parameters$outcome,"DPTRS","DPTRS60","M120","CPH","LR"))) %>% stats::na.omit(all_of(parameters$variables)) %>% filter(!!rlang::sym(parameters$time) != 0)


parameters <- parameters_by_stage(stage = "stage1")
formula_v <- create_formula(variables = parameters$variables, response = parameters$response, conditions = parameters$conditions, bonus_variables = bonus_variables)
# formula_v_best <- extract_best_formula(formula_v,affix ="Cox_Stage0",path_to_export_dt =  here())
# formula_to_compare <- paste0(parameters$response,"~",formula_v_best[1])
# formula_v_to_compare  <-paste0(parameters$response,"~",formula_v_best[2:3])

outcome <- parameters$outcome
time <-parameters$time
variables <-parameters$variables
trainData <-trainData
testData <-testData
conditions <-parameters$condition
# formula_v <- create_formula(variables = parameters$variables, response = parameters$response, conditions = parameters$conditions, bonus_variables = NULL)
formula_to_compare <- paste0(parameters$response,"~",extract_best_formula(formula_v,affix ="Cox_Stage1",path_to_export_dt =  path_to_export_dt, n = 1, horizon = 3))
# formula_v <- create_formula(variables = parameters$variables, response = parameters$response, conditions = parameters$conditions, bonus_variables = bonus_variables)
res_1 <- compare_TimeROC_l(formula_to_compare = formula_to_compare,formula_v = formula_v,model_2_fit = "coxph",trainData = trainData,testData = testData,prediction_f = prediction_LF,outcome = outcome, time = time,horizon_time = c(3,5), idd = TRUE)
write.csv(x = res_1, file = paste0(path_to_export_dt,"/tables/ROC_pvalue_Cox_Stage1.csv"))

gg <- plot_with_pvalue(formula_v,affix ="Cox_Stage1",path_to_export_dt =
path_to_export_dt, horizon = 3, title ="Model performances compared to the best model", pvalue_v = 0.5)
 ggplotly(gg)
 gg$data %>% filter(AUC- max(gg$data$AUC) > -0.03 & pvalue > 0.05) %>% dim
 
 ROC_best <- compute_TimeROC(formula_to_compare = formula_to_compare,model_2_fit = "coxph",trainData = trainData,testData = testData,prediction_f = prediction_LF,outcome = outcome, time = time,horizon_time = c(3,5), idd = TRUE)
 confint(ROC_best)
# stage 2 -----------------------------------------------------------------
# load parameters for stage 2
parameters <- parameters_by_stage(stage = "stage2")
formula_v <- create_formula(variables = parameters$variables, response = parameters$response, conditions = parameters$conditions, bonus_variables = bonus_variables)
# formula_v_best <- extract_best_formula(formula_v,affix ="Cox_Stage0",path_to_export_dt =  here())
# formula_to_compare <- paste0(parameters$response,"~",formula_v_best[1])
# formula_v_to_compare  <-paste0(parameters$response,"~",formula_v_best[2:3])

dt <- data_demographics_ogtt %>% 
  select(all_of(c(parameters$variables,parameters$time,parameters$outcome,"Draw.Date.x","AB_group","ADA_dysglycemia","DPTRS","DPTRS60","M120","CPH","LR"))) %>% 
  drop_na()
# select mAB normoglycemia stage 2

dt <- dt %>% filter(ADA_dysglycemia == "Dysglycemia" & !(AB_group %in% c("GAD","IAA","IA2A")))
trainData <-
  dt[dt$Draw.Date.x < "2012-02-20", ] %>% mutate_if(is.character, as.factor) %>%  select(all_of(c(parameters$variables,parameters$time,parameters$outcome,"DPTRS","DPTRS60","M120","CPH","LR"))) %>% stats::na.omit(all_of(parameters$variables)) %>% filter(!!rlang::sym(parameters$time) != 0)
testData <-
  dt[dt$Draw.Date.x >= "2012-02-20", ]  %>% mutate_if(is.character, as.factor) %>%  select(all_of(c(parameters$variables,parameters$time,parameters$outcome, "DPTRS","DPTRS60","M120","CPH","LR"))) %>% stats::na.omit(all_of(parameters$variables)) %>% filter(!!rlang::sym(parameters$time) != 0)

outcome <- parameters$outcome
time <-parameters$time
variables <-parameters$variables
trainData <-trainData
testData <-testData
conditions <-parameters$condition
# formula_v <- create_formula(variables = parameters$variables, response = parameters$response, conditions = parameters$conditions, bonus_variables = NULL)
formula_to_compare <- paste0(parameters$response,"~",extract_best_formula(formula_v,affix ="Cox_Stage2",path_to_export_dt =  path_to_export_dt, n = 1, horizon = 3))
# formula_v <- create_formula(variables = parameters$variables, response = parameters$response, conditions = parameters$conditions, bonus_variables = bonus_variables)
res_2 <- compare_TimeROC_l(formula_to_compare = formula_to_compare,formula_v = formula_v,model_2_fit = "coxph",trainData = trainData,testData = testData,prediction_f = prediction_LF,outcome = outcome, time = time,horizon_time = c(3,5), idd = TRUE)
write.csv(x = res_2, file = paste0(path_to_export_dt,"/tables/ROC_pvalue_Cox_Stage2.csv"))
gg <- plot_with_pvalue(formula_v,affix ="Cox_Stage2",path_to_export_dt =
                         path_to_export_dt, horizon = 3, title ="Model performances compared to the best model", pvalue_v = 0.5)
ggplotly(gg)
gg$data %>% filter(AUC- max(gg$data$AUC) > -0.03 & pvalue > 0.05) %>% dim

ROC_best <- compute_TimeROC(formula_to_compare = formula_to_compare,model_2_fit = "coxph",trainData = trainData,testData = testData,prediction_f = prediction_LF,outcome = outcome, time = time,horizon_time = c(3,5), idd = TRUE)
confint(ROC_best)
