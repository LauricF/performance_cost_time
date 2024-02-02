presence_in_formula <- function(res_dt,vars_v){
  for(vars in vars_v){
  res_dt <- res_dt %>% 
    rowwise() %>% 
    mutate({{vars}} := grepl(vars,formula))}
  
  return(res_dt)
}

