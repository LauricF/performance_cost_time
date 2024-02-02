# create multiple model.design

create_models <- function(model_2_fit, formula_v, arg_l) {
      res <- list()
      for (i in 1:length(formula_v)) {
        print(i)
        # if(i == 57){
        #   1+1
        # }
        print(formula_v[i])
        args <- c(list(model_name = model_2_fit, formula = formula_v[i]), arg_l)
        res[[i]] <- do.call(fit_model_LF,args = args)
      }
      return(res)
  }
  
# https://win-vector.com/2018/09/01/r-tip-how-to-pass-a-formula-to-lm/

fit_model_LF <- function(model_name, formula,...) {
  extra <- list(...)
  # transform argument of there are not in a bad format
  if(class(formula) != "formula"){ # transform potential string into formula object
    formula <- formula(formula)
  }
  if("tbl" %in% class(extra$data)){ # insure that the trainData is a dataframe and not a tbl as this can create some problem with method such as rfsrc
    extra$data <- data.frame(extra$data)
  }
  # fit model
  if( "coxph"  == model_name){
    res <- coxph(formula,data = extra$data, x = extra$x)
  }
  if("rfsrc" == model_name){
    if(is.null(extra$ntree)){extra$ntree = 200}
    if(is.null(extra$forest)){extra$forest = T}
    res <- rfsrc(formula =formula,data = extra$data, ntree = extra$ntree, nodesize = extra$nodesize, forest = extra$forest, ntime = extra$ntime )
  }
  if(!exists("res",inherits = FALSE)){
    message("The model that you want to fit, ", model_name,", is unknown")
  }
  return(res)
}
