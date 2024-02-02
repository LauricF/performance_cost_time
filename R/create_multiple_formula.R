# given a string of variables create a vectors of formula with all the combination of variables

allCombinations <- function(response = "", covars, formulae = TRUE) { # taken from wiqid package
  if (!is.character(response) || !is.character(covars)) 
    stop("'response' and 'covars' must be character vectors")
  if (length(response) > 1) 
    stop("Only one response variable possible.")
  if (any(response == covars)) 
    stop("The response cannot also be a covariate.")
  covars <- unique(covars)
  ncovs <- length(covars)
  tfmat <- matrix(FALSE, 2^ncovs, ncovs)
  for (i in 1:ncovs) tfmat[, i] <- rep(c(FALSE, TRUE), each = 2^(i - 
                                                                   1))
  if (ncovs > 1) 
    tfmat <- tfmat[order(rowSums(tfmat)), ]
  RHS <- apply(tfmat, 1, function(x) paste(covars[x], collapse = " + "))
  RHS[1] <- "1"
  forms <- paste(response, RHS, sep = " ~ ")
  if (formulae) {
    return(forms)
  }
  else {
    colnames(tfmat) <- covars
    rownames(tfmat) <- forms
    return(tfmat)
  }
}

create_formula <- function(variables,response, conditions = NULL, bonus_variables = NULL){
  res <- allCombinations(response = response, variables, formulae = FALSE)
  res_dt <- data.frame(res)
  if(!is.null(conditions)){
  conditions <- paste0(conditions,collapse = " & ")
  res_dt <- res_dt %>% filter(eval(parse(text = conditions)))
  }
  res <- rownames(res_dt)
  if(!is.null(bonus_variables)){
    res <- c(res, paste(response,bonus_variables, sep = " ~ "))
  }
  return(res)
}
# parameters <- parameters_by_stage(stage = "stage0")
# formula_v <- create_formula(variables = parameters$variables, response = parameters$response, conditions = parameters$conditions, variables_bonus = c("DPTRS","DPTRS60","M120","CPH","LR"))
