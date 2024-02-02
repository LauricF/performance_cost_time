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
dt <- data_demographics_ogtt %>% 
  select(all_of(c(variables,time,outcome,"Draw.Date.x"))) %>% 
  drop_na()

# select 1AB only

dt <- dt %>% filter(AB_group %in% c("GAD","IAA","IA2A"))
variables <- c("GRS2", "GAD", "IAA", "IA2", "Participant.Sex", "age_at_first_drawing", "BMI",
               "lnT","Index60", "AUC_glucose", "AUC_ceptide")
trainData <-
  dt[dt$Draw.Date.x < "2012-02-20", ] %>% mutate_if(is.character, as.factor) %>%  select(all_of(c(variables, outcome, time))) %>% stats::na.omit(all_of(variables)) %>% filter(!!rlang::sym(time) != 0)
testData <-
  dt[dt$Draw.Date.x >= "2012-02-20", ]  %>% mutate_if(is.character, as.factor) %>%  select(all_of(c(variables, outcome, time))) %>% stats::na.omit(all_of(variables)) %>% filter(!!rlang::sym(time) != 0)

conditions <- c("!(lnT == TRUE & age_at_first_drawing == TRUE)", #to not have age in two ways
                "!(Index60 == TRUE & (AUC_glucose == TRUE  | AUC_ceptide == TRUE))", #to not have strongly correlated variables simultaniously
                "!(GAD == TRUE | IAA == TRUE | IA2 == TRUE)", #to not have strongly correlated variables simultaniously
                "!(GRS2 == FALSE & age_at_first_drawing == FALSE & BMI  == FALSE & lnT  == FALSE & Index60  == FALSE & AUC_glucose  == FALSE & AUC_ceptide  == FALSE)") #to remove all 
response = "Surv(time_since_first_screening, T1D.Indicator)"
formula_v <- create_formula(variables = variables, response = response, conditions = conditions)

models <- create_models(model_2_fit = "coxph", formula_v = formula_v, arg_l = list(x = TRUE, data = trainData))

sub <- 1:length(formula_v)
n1 <- Sys.time()
res <- measure_models_LF(models =  models[sub],test_data = testData,prediction_f = prediction_LF,outcome = outcome, time = time,horizon_time_v = horizon_time_v,path_to_export_dt = here(), formula_v = formula_v[sub], band = FALSE, affix = "Cox_1AB")
Sys.time()- n1

res_dt <- extract_performance_measure(path_to_export_dt =  here(),formula_v = formula_v[sub], affix = "Cox_1AB")

C_dt <- presence_in_formula(res_dt[[2]], c("GRS2","Index60"))

dt1 <- C_dt %>% mutate(group_variable  = case_when(
  GRS2 & Index60 ~ "GRS2 & Index60" ,
  GRS2 ~ "GRS2",
  Index60 ~ "index60",
  TRUE ~ "none"))

tAUC <- presence_in_formula(res_dt[[1]], c("GRS2","Index60"))

dt2 <- tAUC %>% mutate(group_variable  = case_when(
  GRS2 & Index60 ~ "GRS2 & Index60" ,
  GRS2 ~ "GRS2",
  Index60 ~ "index60",
  TRUE ~ "none")) %>% 
  filter(horizon_time == 3)

plot_dt <- dt1 %>% left_join(dt2)

gg <- ggplot(plot_dt, aes( x = AUC, y = Cindex, colour = group_variable, label = formula)) +
  geom_point() + theme_bw()

library(plotly)
ggplotly(gg)
