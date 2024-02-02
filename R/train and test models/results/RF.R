model <- "RF"
parameters <- parameters_by_stage(stage = "stage0")
formula_v <- create_formula(variables = parameters$variables, response = parameters$response, conditions = parameters$conditions)
gg <- plot_summary1(formula_v,affix = paste0(model,"_Stage0"),path_to_export_dt =  here(),group = "paste(GRS,Metabolic, sep = \" & \")")
gg
ggplotly(gg)

gg <- find_close_formula(formula_v,affix =paste0(model,"_Stage0"),path_to_export_dt =  here(),variable_of_interest = "GRS2", horizon_time = 3)
gg
ggplotly(gg)


parameters <- parameters_by_stage(stage = "stage1")
formula_v <- create_formula(variables = parameters$variables, response = parameters$response, conditions = parameters$conditions)
gg <- plot_summary1(formula_v,affix = paste0(model,"_Stage1"),path_to_export_dt =  here(),group = "paste(GRS,Metabolic, sep = \" & \")")
gg
ggplotly(gg)

gg <- find_close_formula(formula_v,affix =paste0(model,"_Stage1"),path_to_export_dt =  here(),variable_of_interest = "GRS2", horizon_time = 3)
gg
ggplotly(gg)


parameters <- parameters_by_stage(stage = "stage2")
formula_v <- create_formula(variables = parameters$variables, response = parameters$response, conditions = parameters$conditions)
gg <- plot_summary1(formula_v,affix = paste0(model,"_Stage2"),path_to_export_dt =  here(),group = "paste(GRS,Metabolic, sep = \" & \")")
gg
ggplotly(gg)

gg <- find_close_formula(formula_v,affix =paste0(model,"_Stage2"),path_to_export_dt =  here(),variable_of_interest = "GRS2", horizon_time = 3)
gg
ggplotly(gg)
