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
fit_save_performance(outcome  =  parameters$outcome,
                     time = parameters$time,
                     variables = parameters$variables,
                     horizon_time_v = c(2, 3, 5, 7, 10),
                     trainData = trainData,
                     testData = testData,
                     conditions = parameters$condition, 
                     bonus_variables = c("DPTRS","DPTRS60","M120","CPH","LR"),
                     model_2_fit = "coxph",
                     path_to_export_dt = path_to_export_dt,
                     affix = "Cox_Stage2")                     

# rfsrc model
fit_save_performance(outcome  =  parameters$outcome,
                     time = parameters$time,
                     variables = parameters$variables,
                     horizon_time_v = c(2, 3, 5, 7, 10),
                     trainData = trainData,
                     testData = testData,
                     conditions = parameters$condition,
                     bonus_variables = c("DPTRS","DPTRS60","M120","CPH","LR"),
                     model_2_fit = "rfsrc",
                     path_to_export_dt = path_to_export_dt,
                     affix = "RF_Stage2")