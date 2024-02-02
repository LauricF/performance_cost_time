# plot all performance at once --------------------------------------------


plot_summary1 <- function(formula_v,affix,path_to_export_dt =  here(), group = "paste(GRS,Metabolic,Autoantibody, sep = \" & \")", title = affix, horizon = 3, alpha_point = 0.2) {
  res_dt <- extract_performance_measure(path_to_export_dt =  path_to_export_dt,formula_v = formula_v, affix)
  C_dt <-make_group(res_dt[[2]],group = group)
  B_dt <-make_group(res_dt[[3]],group = group)
  tAUC <- make_group(res_dt[[1]],group = group)
  tAUC <- tAUC %>% filter(horizon_time == horizon)
  
  plot_dt <- C_dt %>% left_join(tAUC) %>% left_join(B_dt) %>% mutate(formula = clean_formula(formula)) %>% mutate(
    group_variable = case_when(
      group_variable == "Base" ~ "Base (clinical + AB)",
      group_variable == "GRS" ~ "Base + GRS",
      group_variable == "Metabolic" ~ "Base + Metabolic",
      group_variable == "GRS & Metabolic" ~ "Base + GRS + Metabolic",
      TRUE ~ group_variable
    )
  ) %>% mutate(group_variable = factor(group_variable, levels = c("Base (clinical + AB)","Base + GRS", "Base + Metabolic","Base + GRS + Metabolic","Classic models")))
  
  gg <- ggplot(plot_dt, aes( x = AUC, y = Brier_score, colour = group_variable, label = formula, text = paste("AUC: ", round(AUC, digits = 2), "Brier score: ", round(Brier_score, digits = 2)))) +
    geom_point(alpha = alpha_point) +
    geom_point(data = subset(plot_dt, group_variable == 'Classic models'),
               aes(x = AUC, y = Brier_score, colour = group_variable)) +
    labs(title= title, y = "Brier score") +
    scale_colour_viridis_d(option = "H", direction = -1) +
    theme_bw() +
    ylab("Brier score") +
    xlab("Time dependant ROC AUC") +
    labs(colour = "Variables included in the model")
  return(gg)
}

plot_summary2 <- function(formula_v,affix1,affix2,path_to_export_dt =  here(), group = "paste(GRS,Metabolic,Autoantibody, sep = \" & \")", title = affix, horizon = 3) {
  
  res_dt <- extract_performance_measure(path_to_export_dt =  path_to_export_dt,formula_v = formula_v, affix = affix1)
  C_dt <-make_group(res_dt[[2]],group = group)
  B_dt <-make_group(res_dt[[3]],group = group)
  tAUC <- make_group(res_dt[[1]],group = group)
  tAUC <- tAUC %>% filter(horizon_time == horizon)
  
  plot_dt1 <- C_dt %>% left_join(tAUC) %>% left_join(B_dt) %>% mutate(model = affix1)
  
  res_dt <- extract_performance_measure(path_to_export_dt =  path_to_export_dt,formula_v = formula_v, affix = affix2)
  C_dt <-make_group(res_dt[[2]],group = group)
  B_dt <-make_group(res_dt[[3]],group = group)
  tAUC <- make_group(res_dt[[1]],group = group)
  tAUC <- tAUC %>% filter(horizon_time == horizon)
  
  plot_dt2 <- C_dt %>% left_join(tAUC) %>% left_join(B_dt) %>% mutate(model = affix2)
  
  plot_dt <- rbind(plot_dt1, plot_dt2) %>% mutate(formula = clean_formula(formula))
  
  gg <- ggplot(plot_dt, aes( x = AUC, y = Brier_score, shape = model, colour = group_variable, label = formula,text = paste("AUC: ", round(AUC, digits = 2), "Brier score: ", round(Brier_score, digits = 2)))) +
    geom_point() +
    geom_point(data = subset(plot_dt, group_variable == 'Classic models'),
               aes(x = AUC, y = Brier_score, shape = model, colour = group_variable)) +
    labs(title= title) +
    theme_bw() +
    ylab("Brier score") +
    labs(colour = "variables included in the model")
  return(gg)
}

make_group <- function(dt, group = "paste(GRS,Metabolic,Autoantibody,`Classic models`, sep = \" & \")"){
  dt <- presence_in_formula(dt, c("GRS2","Index60|AUC_glucose|AUC_ceptide|C_peptide_index_30|beta2_score|Hba1c|Index60|DPTRS|DPTRS60|M120|CPH|LR","GAD|IAA|IA2","Participant.Sex|age_at_first_drawing|BMI|lnT|Z_BMI","^Index60$|^DPTRS$|^DPTRS60$|^M120$|^CPH$|^LR$"))
  dt <- dt %>% rename(GRS = GRS2,
                      Metabolic = `Index60|AUC_glucose|AUC_ceptide|C_peptide_index_30|beta2_score|Hba1c|Index60|DPTRS|DPTRS60|M120|CPH|LR`,
                      Autoantibody = `GAD|IAA|IA2`,
                      Clinical = `Participant.Sex|age_at_first_drawing|BMI|lnT|Z_BMI`,
                      `Classic models` = `^Index60$|^DPTRS$|^DPTRS60$|^M120$|^CPH$|^LR$`)
  dt <- dt %>% 
    mutate(GRS = ifelse(GRS == TRUE, "GRS","toremove"),
           Metabolic = ifelse(Metabolic == TRUE & `Classic models` == FALSE, "Metabolic","toremove"),
           Autoantibody = ifelse(Autoantibody == TRUE, "Autoantibody","toremove"),
           Clinical = ifelse(Clinical == TRUE, "Clinical","toremove"),
           `Classic models` = ifelse(`Classic models` == TRUE, "Classic models","toremove")) %>% 
    mutate(group_variable = eval(parse(text = group))) %>% 
    # mutate(group_variable = paste(GRS,Metabolic,Autoantibody, sep = " & ")) %>% 
    mutate(group_variable = gsub("toremove & | & toremove","", group_variable)) %>% 
    mutate(group_variable = ifelse(group_variable == "toremove","Base",group_variable))
  
  return(dt)
}


plot_summary3 <- function(formula_v,affix,path_to_export_dt =  here(), group = "paste(Index60,AUC_glucose,AUC_ceptide,C_peptide_index_30,beta2_score,Hba1c,`Classic models`, sep = \" & \")", title = affix, horizon = 3) {
  res_dt <- extract_performance_measure(path_to_export_dt =  path_to_export_dt,formula_v = formula_v, affix)
  C_dt <-make_group2(res_dt[[2]],group = group)
  B_dt <-make_group2(res_dt[[3]],group = group)
  tAUC <- make_group2(res_dt[[1]],group = group)
  tAUC <- tAUC %>% filter(horizon_time == horizon)
  
  plot_dt <- C_dt %>% left_join(tAUC) %>% left_join(B_dt) %>% mutate(formula = clean_formula(formula))
  
  gg <- ggplot(plot_dt, aes( x = AUC, y = Brier_score, colour = group_variable, label = formula),text = paste("AUC: ", round(AUC, digits = 2), "Brier score: ", round(Brier_score, digits = 2))) +
    geom_point() +
    geom_point(data = subset(plot_dt, group_variable == 'Classic models'),
               aes(x = AUC, y = Brier_score, colour = group_variable)) +
    labs(title= title, y = "Brier score") +
    scale_colour_viridis_d(option = "plasma") +
    theme_bw() +
    ylab("Brier score") +
    labs(colour = "Variables included in the model")
  return(gg)
}


make_group2 <- function(dt, group = "paste(Index60,AUC_glucose,AUC_ceptide,C_peptide_index_30,beta2_score,Hba1c,`Classic models`, sep = \" & \")"){
  dt <- presence_in_formula(dt,c("Index60","AUC_glucose","AUC_ceptide","C_peptide_index_30","beta2_score","Hba1c","DPTRS|DPTRS60|M120|CPH|LR","Participant.Sex|age_at_first_drawing|BMI|lnT|Z_BMI|GRS2"))
  dt <- dt %>% rename(Clinical = `Participant.Sex|age_at_first_drawing|BMI|lnT|Z_BMI|GRS2`,
                      `Classic models` = `DPTRS|DPTRS60|M120|CPH|LR`)
  dt <- dt %>% 
    mutate(Clinical = ifelse(Clinical == TRUE, "Clinical","toremove"),
           Index60 = ifelse(Index60 == TRUE, "Index60","toremove"),
           AUC_glucose = ifelse(AUC_glucose == TRUE, "AUC_glucose","toremove"),
           AUC_ceptide = ifelse(AUC_ceptide == TRUE, "AUC_ceptide","toremove"),
           C_peptide_index_30 = ifelse(C_peptide_index_30 == TRUE, "C_peptide_index_30","toremove"),
           beta2_score = ifelse(beta2_score == TRUE, "beta2_score","toremove"),
           Hba1c = ifelse(Hba1c == TRUE, "Hba1c","toremove"),
           `Classic models` = ifelse(`Classic models` == TRUE, "Classic models","toremove")) %>% 
    mutate(group_variable = eval(parse(text = group))) %>% 
    # mutate(group_variable = paste(GRS,Metabolic,Autoantibody, sep = " & ")) %>% 
    mutate(group_variable = gsub("toremove & | & toremove","", group_variable)) %>% 
    mutate(group_variable = ifelse(group_variable == "toremove","No metabolic variables",group_variable))
  
  return(dt)
}
# parameters <- parameters_by_stage(stage = "stage0")
# formula_v <- create_formula(variables = parameters$variables, response = parameters$response, conditions = parameters$conditions)
# gg <- plot_summary1(formula_v,affix = "Cox_Stage0",path_to_export_dt =  here(),group = "paste(GRS,Metabolic, sep = \" & \")")
# gg
# ggplotly(gg)

# plot_calibration_of_best  -----------------------------------------------

plot_calibration_of_best <- function(formula_v,affix,path_to_export_dt =  here(), n =10, horizon = 3) {
  
  res_dt <- extract_performance_measure(path_to_export_dt =  path_to_export_dt,formula_v = formula_v, affix = affix)
  C_dt <-res_dt[[2]]
  tAUC <- res_dt[[1]] %>% filter(horizon_time == horizon)
  
  dt <- C_dt %>% left_join(tAUC)
  D = dt[order(dt$Cindex, dt$AUC, decreasing = FALSE), ]
  # front = D[which(!duplicated(cummin(D$Cindex))), ]
  
  res <- D$formula[1:n]
  for(i in 1:n){
    # image <- load.image(paste0(path_to_export_dt,"/figures/calibration",affix,"_",res[i],".JPEG"))
    myJPG <- stack(paste0(path_to_export_dt,"/figures/calibration",affix,"_",res[i],".JPEG"))
    plotRGB(myJPG)  
    # plot(image,axes = FALSE)
  }
  return()
}

# plot_calibration_of_best(formula_v,affix ="Cox_Stage0",path_to_export_dt =  here())

extract_best_formula <- function(formula_v,affix,path_to_export_dt =  here(), n =10, horizon) {
  
  res_dt <- extract_performance_measure(path_to_export_dt =  path_to_export_dt,formula_v = formula_v, affix = affix)
  Brier_dt <-res_dt[[3]]
  tAUC <- res_dt[[1]] %>% filter(horizon_time == horizon)
  
  dt <- tAUC %>% left_join(Brier_dt)
  D = dt[order(1-dt$AUC, dt$Brier_score, decreasing = FALSE), ]
  # front = D[which(!duplicated(cummin(D$Cindex))), ]
  
  res <- D$formula[1:n]
  return(res)
}

# extract_best_formula(formula_v,affix ="Cox_Stage0",path_to_export_dt =  here())


find_close_formula <- function(formula_v,affix,path_to_export_dt =  here(),variable_of_interest, horizon = 3, title = affix, alpha_point = 0.7){
  res_dt <- extract_performance_measure(path_to_export_dt =  path_to_export_dt,formula_v = formula_v, affix = affix)
  tAUC <- res_dt[[1]]
  tAUC <- tAUC %>% filter(horizon_time == horizon)
  C_dt <-res_dt[[2]]
  B_dt <-res_dt[[3]]
  
  plot_dt <- C_dt %>% left_join(tAUC) %>% left_join(B_dt)
  
  pattern <- paste0(pattern = " \\+ |",variable_of_interest)
  plot_dt <- presence_in_formula(plot_dt, variable_of_interest)
  plot_dt$group_variable <- gsub(pattern = pattern, replacement = "", x =  plot_dt$formula) # remove formula operation
  
  # remove formula which do not have a twin
  dups <- duplicated(plot_dt[,"group_variable"])
  group_variables_dulpicated <-  plot_dt[ which(dups),"group_variable"] %>% pull()
  plot_dt <- plot_dt[plot_dt$group_variable %in% group_variables_dulpicated,]
  
  variable_of_interest_enq <- enquo(variable_of_interest)
  plot_dt <-plot_dt %>% mutate(variable_of_interest := ifelse(!!variable_of_interest_enq,variable_of_interest,paste0("without ",variable_of_interest)))
  
  gg <- ggplot(plot_dt, aes( x = AUC, y = Brier_score, label = formula, colour =  .data[[variable_of_interest]]), text = paste("AUC: ", round(AUC, digits = 2), "Brier score: ", round(Brier_score, digits = 2))) +
    geom_point(alpha = alpha_point) +
    geom_line(aes(group = group_variable),alpha = 0.1,linewidth = 0.1, colour = "black") +
    labs(title= title) +
    theme_bw() +
    ylab("Brier score")
  return(gg)
}

find_close_formula_article <- function(formula_v,affix,path_to_export_dt =  here(),variable_of_interest, group, horizon = 3, title = affix){
  res_dt <- extract_performance_measure(path_to_export_dt =  path_to_export_dt,formula_v = formula_v, affix = affix)
  C_dt <-res_dt[[2]]
  B_dt <-res_dt[[3]]
  tAUC <- make_group(res_dt[[1]],group = group)
  tAUC <- tAUC %>% filter(horizon_time == horizon)
  
  plot_dt <- C_dt %>% left_join(tAUC, by = c("formula" = "formula" )) %>% left_join(B_dt,by = c("formula" = "formula","horizon_time" = "horizon_time" ))
  
  pattern <- paste0(pattern = " \\+ |",variable_of_interest)
  plot_dt <- presence_in_formula(plot_dt, variable_of_interest)
  plot_dt$group_variable <- gsub(pattern = pattern, replacement = "", x =  plot_dt$formula) # remove formula operation
  
  # remove formula which do not have a twin
  dups <- duplicated(plot_dt[,"group_variable"])
  group_variables_dulpicated <-  plot_dt[ which(dups),"group_variable"] %>% pull()
  plot_dt <- plot_dt[plot_dt$group_variable %in% group_variables_dulpicated,]
  
  plot_dt <-plot_dt %>% mutate(!! (variable_of_interest) := ifelse(!!rlang::sym(variable_of_interest),"Present","Missing")) %>% 
    mutate(formula = clean_formula(formula))
  
  gg <- ggplot(plot_dt, aes( x = AUC, y = Brier_score, label = formula, colour =  .data[[variable_of_interest]], text = paste("AUC: ", round(AUC, digits = 2), "Brier score: ", round(Brier_score, digits = 2)))) +
    geom_point(alpha = 0.7) +
    geom_line(aes(group = group_variable),alpha = 0.1,linewidth = 0.1, colour = "black") +
    labs(title= title) +
    theme_bw() +
    ylab("Brier score") +
    labs(colour = paste0("Presence of ", variable_of_interest, " in model"))
  return(gg)
}

find_same_formula_different_models <- function(formula_v,affix1, affix2, path_to_export_dt =  here(), horizon = 3, title = NULL){
  res_dt <- extract_performance_measure(path_to_export_dt =  path_to_export_dt,formula_v = formula_v, affix = affix1)
  tAUC <- res_dt[[1]]
  tAUC <- tAUC %>% filter(horizon_time == horizon)
  C_dt <-res_dt[[2]]
  B_dt <-res_dt[[3]]
  
  plot_dt1 <- C_dt %>% left_join(tAUC) %>% left_join(B_dt) %>% mutate(model = affix1)
  
  res_dt <- extract_performance_measure(path_to_export_dt =  path_to_export_dt,formula_v = formula_v, affix = affix2)
  tAUC <- res_dt[[1]]
  tAUC <- tAUC %>% filter(horizon_time == horizon)
  C_dt <-res_dt[[2]]
  B_dt <-res_dt[[3]]
  
  plot_dt2 <- C_dt %>% left_join(tAUC) %>% left_join(B_dt)  %>% mutate(model = affix2)
  
  plot_dt <- rbind(plot_dt1, plot_dt2)  %>% mutate(formula = clean_formula(formula))
  
  gg <- ggplot(plot_dt, aes( x = AUC, y = Brier_score, label = formula, colour =  model, text = paste("AUC: ", round(AUC, digits = 2), "Brier score: ", round(Brier_score, digits = 2)))) +
    geom_point(alpha = 0.7) +
    geom_line(aes(group = formula),alpha = 0.1,linewidth = 0.1, colour = "black") +
    labs(title= title) +
    theme_bw() +
    ylab("Brier score") +
    labs(fill = "Significative difference in AUC")
  return(gg)
}

# gg <- find_same_formula_different_models(formula_v,affix1 = paste0(model,"_Stage0"), ,affix2 = paste0("RF","_Stage0"), path_to_export_dt =  path_to_export_dt, horizon = 3)
# ggplotly(gg)

# gg <- find_close_formula(formula_v,affix ="Cox_Stage0",path_to_export_dt =  here(),variable_of_interest = "GRS2", horizon_time = 3)
# gg  
# ggplotly(gg)

# pvalues to compare the best model to the less good models ----------------------

plot_with_pvalue <- function(formula_v,affix,path_to_export_dt =  here(),variable_of_interest, horizon = 3, title = affix, pvalue_v = 0.05){
  pvalue <- read.csv(paste0(path_to_export_dt,"/tables/ROC_pvalue_",affix,".csv")) %>% 
    filter(year == horizon) %>% 
    mutate(significant = pvalue < pvalue_v,
           significant = factor(significant),
           formula = gsub("^\\s","",formula,)) %>% 
    select(formula,pvalue,significant)
  
  res_dt <- extract_performance_measure(path_to_export_dt =  path_to_export_dt,formula_v = formula_v, affix = affix)
  tAUC <- res_dt[[1]]
  tAUC <- tAUC %>% filter(horizon_time == horizon)
  C_dt <-res_dt[[2]]
  B_dt <-res_dt[[3]]
  
  plot_dt <- C_dt %>% left_join(tAUC) %>% left_join(B_dt) %>% left_join(pvalue) %>% mutate(significant = case_when( is.na(pvalue) ~ "Model with the highest AUC",                                         significant == FALSE ~ "No",
                                  significant == TRUE ~ "Yes")) %>% mutate(formula = clean_formula(formula))
  gg <- ggplot(plot_dt, aes( x = AUC, y = Brier_score, label = formula, colour =  significant, label2 = pvalue)) +
    geom_point(alpha = 0.7) +
    labs(title= title) +
    theme_bw() + 
    labs(colour = "Is the ROC AUC at 3 years significiantly\ndifferent from the best model?") +
    ylab("Brier score")
  return(gg)
}

plot_same_formula_different_models <- function(formula_v,affix1, affix2, stage, path_to_export_dt =  here(), horizon = 3, title = NULL){
  
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
  
  color_group <- c("blue","black", "red")
  gg <- ggplot(plot_dt, aes( x = AUC, y = Brier_score, label = formula)) +
    geom_point(aes(fill =  model), size=3, shape=21, stroke=0,alpha = 0.7) +
    geom_line(aes(group = formula, colour = difference),alpha = 0.7,linewidth = 0.01) +
    scale_colour_manual(values=color_group) +
    labs(title= title) +
    theme_bw() +
    labs(colour = "") +
    ylab("Brier score")
  return(gg)
}

# gg <- plot_same_formula_different_models(formula_v,affix1 = paste0(model,"_Stage0"), affix2 = paste0("RF","_Stage0"), stage = "Stage0", path_to_export_dt =  path_to_export_dt, horizon = 3, title = NULL)
# ggplotly(gg)

# compare cost of different models ----------------------------------------


plot_cost <- function(formula_v,cost_dt,affix,path_to_export_dt =  here(),variable_of_interest, horizon = 3, title = affix){
  cost <- cost_estimate_all_formula(formula_v, cost_dt) %>% 
    rowwise() %>% 
    mutate(formula = str_split(formula,"~ ")[[1]][2]) %>% ungroup()
  pvalue <- read.csv(paste0(path_to_export_dt,"/tables/ROC_pvalue_",affix,".csv")) %>% 
    filter(year == horizon) %>% 
    mutate(significant = pvalue < 0.05,
           significant = factor(significant),
           formula = gsub("^\\s","",formula,)) %>% 
    select(formula,pvalue,significant)
  
  res_dt <- extract_performance_measure(path_to_export_dt =  path_to_export_dt,formula_v = formula_v, affix = affix)
  tAUC <- res_dt[[1]]
  tAUC <- tAUC %>% filter(horizon_time == horizon)
  C_dt <-res_dt[[2]]
  B_dt <-res_dt[[3]]
  
  plot_dt <- C_dt %>% left_join(tAUC) %>% left_join(B_dt) %>% left_join(pvalue) %>% left_join(cost)
  
  gg <- ggplot(plot_dt, aes( x = AUC, y = Brier_score, label = formula, group =  significant, colour = cost, text = paste("Cost: $", round(cost, digits = 2)))) +
    geom_point(alpha = 0.7) +
    labs(title= title, colour = "cost in $\nto predict risk", y = "Brier score") +
    theme_bw() +
    # scale_colour_continuous(type = "viridis") 
    scale_colour_binned(type = "viridis",
                        breaks = seq(0,120, 20))
  return(gg)
}

plot_cost2 <- function(formula_v,cost_l,affix,path_to_export_dt =  here(),variable_of_interest, horizon = 3, title = affix){
  cost <- cost_estimate_all_formula2(formula_v, cost_l) %>% 
    rowwise() %>% 
    mutate(formula = str_split(formula,"~ ")[[1]][2]) %>% ungroup()
  # pvalue <- read.csv(paste0(path_to_export_dt,"/tables/ROC_pvalue_",affix,".csv")) %>% 
  #   filter(year == horizon) %>% 
  #   mutate(significant = pvalue < 0.05,
  #          significant = factor(significant),
  #          formula = gsub("^\\s","",formula,)) %>% 
  #   select(formula,pvalue,significant)
  # 
  res_dt <- extract_performance_measure(path_to_export_dt =  path_to_export_dt,formula_v = formula_v, affix = affix)
  tAUC <- res_dt[[1]]
  tAUC <- tAUC %>% filter(horizon_time == horizon)
  C_dt <-res_dt[[2]]
  B_dt <-res_dt[[3]]
  
  # plot_dt <- C_dt %>% left_join(tAUC) %>% left_join(B_dt) %>% 
  #   left_join(pvalue) %>% left_join(cost)
  
  plot_dt <- C_dt %>% left_join(tAUC) %>% left_join(B_dt) %>% left_join(cost) %>% mutate(formula = clean_formula(formula))
  gg <- ggplot(plot_dt, aes( x = AUC, y = Brier_score, label = formula, colour = cost, text = paste("Cost: ", round(cost, digits = 2)))) +
    geom_point(alpha = 0.7) +
    labs(title= title, colour = "cost in $\nto predict risk", y = "Brier score") +
    theme_bw() +
    # scale_colour_continuous(type = "viridis") 
    scale_colour_binned(type = "viridis",
                        breaks = seq(0,300, 25)) + 
    theme(legend.key.width = unit(1, "cm"),
          legend.key.height = unit(4, "cm")
    )
  return(gg)
}

plot_cost_for_article <- function(formula_v,cost_l, group, affix,path_to_export_dt =  here(),variable_of_interest, horizon = 3, title = affix){
  cost <- cost_estimate_all_formula2(formula_v, cost_l) %>% 
    rowwise() %>% 
    mutate(formula = str_split(formula,"~ ")[[1]][2]) %>% ungroup()
  # pvalue <- read.csv(paste0(path_to_export_dt,"/tables/ROC_pvalue_",affix,".csv")) %>% 
  #   filter(year == horizon) %>% 
  #   mutate(significant = pvalue < 0.05,
  #          significant = factor(significant),
  #          formula = gsub("^\\s","",formula,)) %>% 
  #   select(formula,pvalue,significant)
  # 
  res_dt <- extract_performance_measure(path_to_export_dt =  path_to_export_dt,formula_v = formula_v, affix = affix)
  C_dt <-make_group(res_dt[[2]],group = group)
  B_dt <-make_group(res_dt[[3]],group = group)
  tAUC <- make_group(res_dt[[1]],group = group)
  tAUC <- tAUC %>% filter(horizon_time == horizon)
  
  # plot_dt <- C_dt %>% left_join(tAUC) %>% left_join(B_dt) %>% 
  #   left_join(pvalue) %>% left_join(cost)
  
  plot_dt <- C_dt %>% left_join(tAUC) %>% left_join(B_dt) %>% left_join(cost) %>% mutate(formula = clean_formula(formula))
  gg <- ggplot(plot_dt, aes( x = AUC, y = Brier_score, label = formula, colour = cost,
                             , text = paste("Cost: $", round(cost, digits = 2)))) +
    geom_point(alpha = 0.7) +
    labs(title= title, colour = "Cost in $\nto predict\nrisk", y = "Brier score") +
    theme_bw() +
    # scale_colour_continuous(type = "viridis") 
    scale_colour_binned(type = "viridis",
                        breaks = seq(0,300, 25)) + 
    theme(legend.key.width = unit(1, "cm"),
          legend.key.height = unit(4, "cm")
    )
  return(gg)
}

##### compare patient times of different models------------------
plot_time <- function(formula_v,data_time,affix,path_to_export_dt =  here(), horizon = 3, title = affix){
  time <- time_all_formula(formula_v, data_time) 
  time <- time %>% 
    rowwise() %>% 
    mutate(formula = str_split(formula,"~ ")[[1]][2]) %>% ungroup()
  # pvalue <- read.csv(paste0(path_to_export_dt,"/tables/ROC_pvalue_",affix,".csv")) %>% 
  #   filter(year == horizon) %>% 
  #   mutate(significant = pvalue < 0.05,
  #          significant = factor(significant),
  #          formula = gsub("^\\s","",formula,)) %>% 
  #   select(formula,pvalue,significant)
  
  res_dt <- extract_performance_measure(path_to_export_dt =  path_to_export_dt,formula_v = formula_v, affix = affix)
  tAUC <- res_dt[[1]]
  tAUC <- tAUC %>% filter(horizon_time == horizon)
  C_dt <-res_dt[[2]]
  B_dt <-res_dt[[3]]
  
  # plot_dt <- C_dt %>% left_join(tAUC) %>% left_join(B_dt) %>% left_join(pvalue) %>% left_join(time)
  plot_dt <- C_dt %>% left_join(tAUC) %>% left_join(B_dt)  %>% left_join(time)  %>% mutate(formula = clean_formula(formula))
  plot_dt <- plot_dt %>% mutate(visit = ifelse(visit == 1, "clinicians needed", "can be done from home"))
  gg <- ggplot(plot_dt, aes( x = AUC, y = Brier_score, label = formula, colour = time, shape = factor(visit),text = paste("Time: ", round(time, digits = 0)," min") )) +
    geom_point(alpha = 0.7) +
    labs(title= title, colour = "Time needed to\nacquire data", y = "Brier score") +
    labs(shape = "Visit")+
    theme_bw() +
    # scale_colour_continuous(type = "viridis") 
    scale_colour_viridis_b(option = "C",
                           breaks = seq(0,165, 20), direction = -1) +
    # scale_colour_binned(type = magma_scale,
    #                     breaks = seq(0,2.5, 0.5)) + 
    theme(legend.key.width = unit(1, "cm"),
          legend.key.height = unit(4, "cm")
    )
  return(gg)
}

plot_time_article <- function(formula_v,data_time, group, affix,path_to_export_dt =  here(), horizon = 3, title = affix){ 
  time <- time_all_formula(formula_v, data_time) 
  time <- time %>% 
    rowwise() %>% 
    mutate(formula = str_split(formula,"~ ")[[1]][2]) %>% ungroup()
  # pvalue <- read.csv(paste0(path_to_export_dt,"/tables/ROC_pvalue_",affix,".csv")) %>% 
  #   filter(year == horizon) %>% 
  #   mutate(significant = pvalue < 0.05,
  #          significant = factor(significant),
  #          formula = gsub("^\\s","",formula,)) %>% 
  #   select(formula,pvalue,significant)
  
  res_dt <- extract_performance_measure(path_to_export_dt =  path_to_export_dt,formula_v = formula_v, affix = affix)
  C_dt <-make_group(res_dt[[2]],group = group)
  B_dt <-make_group(res_dt[[3]],group = group)
  tAUC <- make_group(res_dt[[1]],group = group)
  tAUC <- tAUC %>% filter(horizon_time == horizon)
  
  # plot_dt <- C_dt %>% left_join(tAUC) %>% left_join(B_dt) %>% left_join(pvalue) %>% left_join(time)
  plot_dt <- C_dt %>% left_join(tAUC) %>% left_join(B_dt)  %>% left_join(time) %>% mutate(formula = clean_formula(formula))
  plot_dt <- plot_dt %>% mutate(visit = ifelse(visit == 1, "clinicians needed", "can be done from home"))
  gg <- ggplot(plot_dt, aes( x = AUC, y = Brier_score, label = formula, colour = time, text = paste("Time: ", round(time, digits = 0)," min"))) +
    geom_point(alpha = 0.7) +
    labs(title= title, colour = "Patient time\nneeded to\nacquire data\n(minutes)", y = "Brier score", x = "Time dependant ROC AUC") +
    theme_bw() +
    # scale_colour_continuous(type = "viridis") 
    scale_colour_viridis_b(option = "C",
                           breaks = seq(0,160, 20), direction = -1) +
    # scale_colour_binned(type = magma_scale,
    #                     breaks = seq(0,2.5, 0.5)) + 
    theme(legend.key.width = unit(1, "cm"),
          legend.key.height = unit(4, "cm")
    )
  return(gg)
}
