identify_model_differences <- function(formula_v,affix1, affix2, stage, path_to_export_dt =  here(), horizon = 3, title = NULL){
  
  pvalue <- read.csv(paste0(path_to_export_dt,"/tables/ROC_pvalue_Cox_vs_RF_", stage,".csv"))
  pvalue <- pvalue %>% select(pvalue, year, formula) %>% filter(year == horizon) %>% mutate(formula = gsub(" ","",formula))
  
  res_dt <- extract_performance_measure(path_to_export_dt =  path_to_export_dt,formula_v = formula_v, affix = affix1)
  tAUC <- res_dt[[1]]
  tAUC <- tAUC %>% filter(horizon_time == horizon)
  C_dt <-res_dt[[2]]
  B_dt <-res_dt[[3]]
  
  plot_dt1 <- C_dt %>% left_join(tAUC) %>% left_join(B_dt) %>% mutate(model = gsub(paste0("_",stage),"",affix1))
  
  res_dt <- extract_performance_measure(path_to_export_dt =  path_to_export_dt,formula_v = formula_v, affix = affix2)
  tAUC <- res_dt[[1]]
  tAUC <- tAUC %>% filter(horizon_time == horizon)
  C_dt <-res_dt[[2]]
  B_dt <-res_dt[[3]]
  
  plot_dt2 <- C_dt %>% left_join(tAUC) %>% left_join(B_dt)  %>% mutate(model = gsub(paste0("_",stage),"",affix2))
  
  plot_dt <- rbind(plot_dt1, plot_dt2)  %>% mutate(formula = gsub(" ","",formula))
  
  plot_dti1 <- plot_dt1 %>% mutate(COX = AUC) %>% select(formula, COX)
  plot_dti2 <- plot_dt2 %>% mutate(RF = AUC)%>% select(formula, RF)
  
  dt <- plot_dti1 %>% left_join(plot_dti2,by = c("formula" = "formula")) %>% 
    mutate(diff_RF_minus_COX = RF-COX) %>% mutate(formula = gsub(" ","",formula))
  
  plot_dt <- plot_dt %>% left_join(dt, c("formula" = "formula")) %>% left_join(pvalue, by = c("formula" = "formula")) %>% mutate(formula = gsub("\\+"," \\+ ",formula)) %>% 
    mutate(pvalue_sign = factor(pvalue < 0.05)) %>% 
    mutate(difference = case_when(
      pvalue_sign == FALSE ~ "No significative difference",
      pvalue_sign == TRUE & diff_RF_minus_COX > 0~ "RF has better performances",
      TRUE ~ "Cox has better performances",
    ))
  
return(plot_dt)
}

res <- identify_model_differences(formula_v,affix1 ="Cox_Stage2", affix2 ="RF_Stage2",stage = "Stage2", path_to_export_dt =  path_to_export_dt, horizon = 3, title ="Model performances compared to the best model")

table(res$difference)
res_dt <- presence_in_formula(res,c("GRS2" , "AB_group" , "Sex" , "lnT" , "AUC_glucose" , "Z_BMI" , "C_peptide_index_30" , "Hba1c", "beta2_score"))

table(res_dt$difference)/2

table(res_dt$difference, res_dt$C_peptide_index_30)/2

table(res_dt$difference, res_dt$GRS2)


table(res_dt$difference, res_dt$AB_group)

table(res_dt$difference, res_dt$Sex)

table(res_dt$difference, res_dt$lnT)

table(res_dt$difference, res_dt$AUC_glucose)

table(res_dt$difference, res_dt$Z_BMI)

table(res_dt$difference, res_dt$Hba1c)

table(res_dt$difference, res_dt$beta2_score)


res_dt %>% filter(C_peptide_index_30 == TRUE) %>% arrange(diff_RF_minus_COX) %>% tail() %>% View()

data(veteran, package = "randomForestSRC")

# load parameters for stage 0
parameters <- parameters_by_stage(stage = "stage0")

dt <- data_demographics_ogtt %>% 
  select(any_of(c(parameters$variables,parameters$time,parameters$outcome,"Draw.Date.x","AB_group", c("DPTRS","DPTRS60","M120","CPH","LR")))) %>% 
  drop_na()
# select 1AB only

dt <- dt %>% filter(AB_group %in% c("GAD","IAA","IA2A"))
trainData <-
  dt[dt$Draw.Date.x < "2012-02-20", ] %>% mutate_if(is.character, as.factor) %>%  select(all_of(c(parameters$variables,parameters$time,parameters$outcome,c("DPTRS","DPTRS60","M120","CPH","LR")))) %>% stats::na.omit(all_of(parameters$variables)) %>% filter(!!rlang::sym(parameters$time) != 0)
testData <-
  dt[dt$Draw.Date.x >= "2012-02-20", ]  %>% mutate_if(is.character, as.factor) %>%  select(all_of(c(parameters$variables,parameters$time,parameters$outcome,c("DPTRS","DPTRS60","M120","CPH","LR")))) %>% stats::na.omit(all_of(parameters$variables)) %>% filter(!!rlang::sym(parameters$time) != 0)
v.obj <- rfsrc(Surv(time_since_first_screening, T1D.Indicator) ~ age_at_first_drawing + BMI + C_peptide_index_30 , trainData, nsplit = 10, ntree = 100)

## partial effect of age on mortality
partial.obj <- partial(v.obj,
                       partial.type = "mort",
                       partial.xvar = "C_peptide_index_30",
                       partial.values = v.obj$xvar$C_peptide_index_30,
                       partial.time = v.obj$time.interest)
pdta <- get.partial.plot.data(partial.obj)

plot(lowess(pdta$x, pdta$yhat, f = 1/3))


## partial effects of karnofsky score on survival
## 

C_peptide_index_30 <- quantile(v.obj$xvar$C_peptide_index_30)
partial.obj <- partial(v.obj,
                       partial.type = "surv",
                       partial.xvar = "C_peptide_index_30",
                       partial.values = C_peptide_index_30,
                       partial.time = v.obj$time.interest)
pdta <- get.partial.plot.data(partial.obj)

matplot(pdta$partial.time, t(pdta$yhat), type = "l", lty = 1,
        xlab = "time", ylab = "karnofsky adjusted survival")
legend("topright", legend = paste0("C_peptide_index_30 = ", C_peptide_index_30), fill = 1:5)


trainData %>% ggplot(aes(x = C_peptide_index_30)) +
  geom_histogram() + theme_bw()
