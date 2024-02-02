model <- "Cox"
path_to_export_dt <-  paste0(here(),"/analysis_28_06")
path_to_export_dt <-  here()
bonus_variables =c("DPTRS","DPTRS60","M120","CPH","LR")
bonus_variables =NULL
parameters <- parameters_by_stage(stage = "stage0")
formula_v <- create_formula(variables = parameters$variables, response = parameters$response, conditions = parameters$conditions, bonus_variables = bonus_variables)
gg <- plot_summary1(formula_v,affix = paste0(model,"_Stage0"),group = "paste(GRS,Metabolic,`Classic models`, sep = \" & \")", path_to_export_dt =  path_to_export_dt)
gg
ggplotly(gg)

gg <- find_close_formula(formula_v,affix ="Cox_Stage0",path_to_export_dt =  path_to_export_dt,variable_of_interest = "GRS2", horizon = 3)
gg
ggplotly(gg)

gg <- find_close_formula(formula_v,affix ="Cox_Stage0",path_to_export_dt =  path_to_export_dt,variable_of_interest = "Index60", horizon = 3)
gg
ggplotly(gg)

gg <- plot_with_pvalue(formula_v,affix ="Cox_Stage0",path_to_export_dt =  path_to_export_dt, horizon = 3,title = "stage 0")
ggplotly(gg)


gg <-plot_cost(formula_v,cost_dt,affix = "Cox_Stage0",path_to_export_dt =  path_to_export_dt,variable_of_interest, horizon = 3)
ggplotly(gg)

gg <- plot_cost2(formula_v,cost_l,affix = "Cox_Stage0",path_to_export_dt = path_to_export_dt,variable_of_interest, horizon = 3)
ggplotly(gg)

parameters <- parameters_by_stage(stage = "stage1",formula_v,affix ="Cox_Stage0",path_to_export_dt = path_to_export_dt,variable_of_interest = "GRS2", horizon_time = 3)
formula_v <- create_formula(variables = parameters$variables, response = parameters$response, conditions = parameters$conditions,bonus_variables =bonus_variables)

gg <- plot_summary1(formula_v,affix = paste0(model,"_Stage1"),path_to_export_dt =  path_to_export_dt, group = "paste(GRS,Metabolic,`Classic models`, sep = \" & \")", horizon = 7)
gg
ggplotly(gg)

gg <- find_close_formula(formula_v,affix ="Cox_Stage1",path_to_export_dt = path_to_export_dt,variable_of_interest = "GRS2", horizon = 7)
gg
ggplotly(gg)

parameters <- parameters_by_stage(stage = "stage2")
formula_v <- create_formula(variables = parameters$variables, response = parameters$response, conditions = parameters$conditions,bonus_variables =bonus_variables)
gg <- plot_summary1(formula_v,affix = paste0(model,"_Stage2"),path_to_export_dt =  path_to_export_dt,group = "paste(GRS,Metabolic,`Classic models`, sep = \" & \")")
gg
ggplotly(gg)

gg <- find_close_formula(formula_v,affix ="Cox_Stage2",path_to_export_dt = path_to_export_dt,variable_of_interest = "GRS2", horizon = 3)
gg
ggplotly(gg)
  