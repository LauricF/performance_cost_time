pvalue <- function(x, ...) {
  # Construct vectors of data y, and Stages (strata) g
  y <- unlist(x)
  g <- factor(ifelse(grepl("Training",names(y)),1,2))
  if (is.numeric(y)) {
    # For numeric variables, perform a standard 2-sample t-test
    p <- t.test(y ~ g)$p.value
  } else {
    # For categorical variables, perform a chi-squared test of independence
    p <- chisq.test(table(y, g))$p.value
  }
  # Format the p-value, using an HTML entity for the less-than sign.
  # The initial empty string places the output on the line below the variable label.
  c(sub("<", "&lt;", format.pval(p, digits=3, eps=0.001)))
}


variables <- c("T1D","Stage","Sex","age","GRS2","IA2","Z-BMI","BMI","AUC Cpeptide","AUC glucose","Index60","Cpeptide index 30","Beta2 score","Hba1c")

dt <- data_demographics_ogtt %>% mutate(Sex = Participant.Sex,
                                        age = age_at_first_drawing,
                                        `AUC glucose` = AUC_glucose,
                                        `AUC Cpeptide` = AUC_ceptide,
                                        `Beta2 score` = beta2_score * 1000,
                                        `Cpeptide index 30` = C_peptide_index_30,
                                        T1D = ifelse(T1D.Indicator== 1, "T1D", "T1D free"),
                                        IA2 = ifelse(IA2 == 1, "IA2", "IA2 free"),
                                        `Z-BMI`= Z_BMI,
                                        Stage = case_when(
                                          AB_group %in% c("GAD","IAA","IA2A") ~ "0",
                                          ADA_dysglycemia == "Euglycemia" & !AB_group %in% c("GAD","IAA","IA2A") ~ "1",
                                          TRUE ~ "2"),
                                        T1D = factor(T1D),
                                        Stage = factor(Stage)) %>% 
  mutate(dataset = ifelse(Draw.Date.x < "2012-10-22", "Training dataset","Testing dataset"),
         dataset = factor(dataset, levels = c("Training dataset","Testing dataset"))) %>% 
  dplyr::select(all_of(c(variables,"dataset"))) %>% 
  drop_na()

my.render.cont <- function(x) {
  with(stats.default(x), 
       sprintf("%0.2f (%0.1f)", MEAN, SD))
}

 table1(~ Sex+age+GRS2+IA2+`Z-BMI`+BMI+`AUC Cpeptide`+ `AUC glucose` + Index60 + `Cpeptide index 30` + `Beta2 score` + Hba1c + Stage| dataset, data=dt, render.continuous=my.render.cont, overall = F, extra.col=list(`P-value`=pvalue))

table1(~ Sex+age+GRS2+IA2+`Z-BMI`+BMI+`AUC Cpeptide`+ `AUC glucose` + Index60 + `Cpeptide index 30` + `Beta2 score` + Hba1c | Stage*dataset, data=dt, render.continuous=my.render.cont, extra.col=list(`P-value`=pvalue))
