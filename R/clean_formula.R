clean_formula <- function(formula, tex = FALSE){
  formula_i <- formula
  formula_i <- gsub("age_at_first_drawing", "age",formula_i)
  formula_i <- gsub("lnT", "log(age)",formula_i)
  formula_i <- gsub("Z_BMI", "z-BMI",formula_i)
  formula_i <- gsub("_", "-",formula_i)
  
  if(tex){
    formula_i <- gsub("60", "$_{60}$",formula_i)
    formula_i <- gsub("120", "$_{120}$",formula_i)
  }
  return(formula_i)
}


clean_label <- function(formula){
  formula_i <- formula
  formula_i <- gsub("_stage0", "$_{S0}$",formula_i)
  formula_i <- gsub("_stage1", "$_{S1}$",formula_i)
  formula_i <- gsub("_stage2", "$_{S2}$",formula_i)
  formula_i <- gsub("cost", "Cost",formula_i)
  formula_i <- gsub("time", "Time",formula_i)
  return(formula_i)
}
