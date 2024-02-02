# http://127.0.0.1:10541/library/SurvMetrics/doc/SurvMetrics-vignette.html

fit_save_performance <- function(outcome  =  "T1D.Indicator",
                                 time = "time_since_first_screening",
                                 variables = c("GRS2", "GAD", "IAA", "IA2", "Participant.Sex", "age_at_first_drawing", "BMI",
                                               "lnT","Index60", "AUC_glucose", "AUC_ceptide"),
                                 horizon_time_v = c(2, 3, 5, 7, 10),
                                 trainData,
                                 testData,
                                 conditions = c("!(lnT == TRUE & age_at_first_drawing == TRUE)", #to not have age in two ways
                                                "!(Index60 == TRUE & (AUC_glucose == TRUE  | AUC_ceptide == TRUE))", #to not have strongly correlated variables simultaniously
                                                "!(GAD == TRUE & IAA == TRUE & IA2 == TRUE)", #to not have strongly correlated variables simultaniously
                                                "!(GRS2 == FALSE & age_at_first_drawing == FALSE & BMI  == FALSE & lnT  == FALSE & Index60  == FALSE & AUC_glucose  == FALSE & AUC_ceptide  == FALSE)"),
                                 bonus_variables = NULL,
                                 model_2_fit,
                                 path_to_export_dt, affix){
  
  response <-  paste0("Surv(",time,",", outcome,")")
  formula_v <- create_formula(variables = variables, response = response, conditions = conditions, bonus_variables = bonus_variables)
  n1 <- Sys.time()
  if(model_2_fit == "coxph"){
    models <- create_models(model_2_fit = model_2_fit, formula_v = formula_v, arg_l = list(x = TRUE, data = trainData))
    res <- measure_models_LF(models =  models,test_data = testData,prediction_f = prediction_LF,outcome = outcome, time = time,horizon_time_v = horizon_time_v,path_to_export_dt = path_to_export_dt, formula_v = formula_v, band = FALSE, affix = affix)
    
  } else{
    no_cores <- detectCores()-1
    cl <- makeCluster(no_cores)
    registerDoParallel(cl)
    # foreach(i = 1:length(formula_v)) %dofuture% {
    foreach(i = 1:length(formula_v), .packages = c('randomForestSRC', 'pec','timeROC','survival','SurvMetrics','rms','riskRegression','stringr','dplyr','naniar','here')) %dopar% {
      source(paste0(here(),"/R/prediction_for_each_class.R"))
      source(paste0(here(),"/R/fit_models.R"))
      source(paste0(here(),"/R/f_model_performance.R"))
      source(paste0(here(),"/R/presence_in_formula.R"))
      model <- create_models(model_2_fit = "rfsrc", formula_v = formula_v[i], arg_l = list(x = TRUE, data = trainData))
      res <- measure_models_LF(models =  model,test_data = testData,prediction_f = prediction_LF,outcome = outcome, time = time,horizon_time_v = horizon_time_v,path_to_export_dt = path_to_export_dt, formula_v = formula_v[i], band = FALSE, affix = affix)
    }
    stopCluster(cl)
  }
  print(Sys.time()- n1)  
}

# function to call all performance measure on 1 model ---------------------------

performance_measure_LF <-
  function(measure_fun_l, arg_l) {
    res <- list()
    for (i in 1:length(measure_fun_l)) {
      res[[i]] <- do.call(measure_fun_l[[i]], arg_l[[i]])
    }
    return(res)
  }

# main function to call several performance measure at once ---------------
# not using ICI and Brier score because results change a lot just by changing slightly the results.
performance_measure_all <- function(model,test_data,prediction_f,outcome, time,horizon_time_v, band){
  res <- performance_measure_LF(measure_fun_l = list(TROC_LF, CIndex_LF,Brier_score_LF,calibration_v_LF),
                                arg_l = list(
                                  list(
                                    model = model,
                                    test_data = test_data,
                                    prediction_f = prediction_f,
                                    outcome = outcome,
                                    time = time,
                                    horizon_time_v = horizon_time_v,
                                    idd = FALSE
                                  ),
                                  list(
                                    model =model,
                                    test_data = test_data,
                                    prediction_f = prediction_f,
                                    outcome = outcome,
                                    time = time,
                                    horizon_time = horizon_time_v[length(horizon_time_v)]
                                  ),
                                  list(
                                    model =model,
                                    test_data = test_data,
                                    prediction_f = prediction_f,
                                    outcome = outcome,
                                    time = time,
                                    horizon_time = horizon_time_v
                                  ),
                                  list(
                                    model = model,
                                    test_data = test_data,
                                    prediction_f = prediction_f,
                                    outcome = outcome,
                                    time = time,
                                    horizon_time_v = horizon_time_v,
                                    band = band
                                  )
                                ))
  return(res)
}

# function to call all performance measure on several models ---------------------------

measure_models_LF <-
  function(models_l,test_data,prediction_f,outcome, time,horizon_time_v,path_to_export_dt, formula_v, band, affix) {
    res <- list()
    
    for (i in 1:length(models_l)) {
      print(i)
      print(formula_v[i])
      model_performance <- performance_measure_all(model = models_l[[i]],test_data,prediction_f,outcome, time,horizon_time_v, band)
      name_to_save <- paste0(affix,"_",str_split(formula_v[i],"~ ")[[1]][2])
      export_model_performance(model_performance, path_to_export_dt, name_to_save = name_to_save)
      res[[formula_v[i]]] <- model_performance
    }
    return(res)
  }

# export results ----------------------------------------------------------

export_model_performance <- function(model_performance, path_to_export_dt, name_to_save){
  #export Time ROC AUC
  write.csv(x = model_performance[[1]], file = paste0(path_to_export_dt,"/tables/T_ROC",name_to_save,".csv"),row.names = FALSE)
  #export C Index
  write.csv(x =  model_performance[[2]], file = paste0(path_to_export_dt,"/tables/C_Index",name_to_save,".csv"),row.names = FALSE)
  # export Brier score
  write.csv(x =  model_performance[[3]], file = paste0(path_to_export_dt,"/tables/Brier",name_to_save,".csv"),row.names = FALSE)
  #export calibration data frame
  write.csv(x = model_performance[[4]], file = paste0(path_to_export_dt,"/tables/T_calibration",name_to_save,".csv"),row.names = FALSE)
  # image of calibration
  dt <- model_performance[[4]]
  p_calibration <- calibration_figure(dt)
  ggsave(plot = p_calibration, filename =paste0(path_to_export_dt,"/figures/calibration",name_to_save,".JPEG"), dpi = 300, width = 8, height = 8)
  
}

# extract results previously saved ----------------------------------------------------------
extract_performance_measure <- function(path_to_export_dt,affix,formula_v) {
  #extract Time ROC AUC
  timeROC_l <- list()
  for(i in 1:length(formula_v)){
    formula_i <- str_split(formula_v[i],"~ ")[[1]][2]
    name_to_load <- paste0(affix,"_",formula_i)
    timeROCi <- read.csv( file = paste0(path_to_export_dt,"/tables/T_ROC",name_to_load,".csv"))
    timeROCi$formula <-  formula_i
    timeROC_l[[i]] <- timeROCi
  }
  timeROC_dt <- do.call("rbind",timeROC_l) 
  #extract C Index
  C_l <- list()
  for(i in 1:length(formula_v)){
    formula_i <- str_split(formula_v[i],"~ ")[[1]][2]
    name_to_load <- paste0(affix,"_",formula_i)
    C_i <- read.csv( file = paste0(path_to_export_dt,"/tables/C_Index",name_to_load,".csv"))
    C_i$formula <-  formula_i
    C_l[[i]] <- C_i
  }
  C_dt <- do.call("rbind",C_l) 
  #extract Brier score Index
  B_l <- list()
  for(i in 1:length(formula_v)){
    formula_i <- str_split(formula_v[i],"~ ")[[1]][2]
    name_to_load <- paste0(affix,"_",formula_i)
    B_i <- read.csv( file = paste0(path_to_export_dt,"/tables/Brier",name_to_load,".csv"))
    B_i$formula <-  formula_i
    B_l[[i]] <- B_i
  }
  B_dt <- do.call("rbind",B_l) 
  res <- list(timeROC_dt = timeROC_dt, C_dt = C_dt,B_dt = B_dt)  
}

# time dependant ROC AUC --------------------------------------------------


TROC_LF <-
  function(model,
           test_data,
           prediction_f,
           outcome,
           time,
           horizon_time_v, 
           idd) {
    res_l <- list()
    for (h in 1:length(horizon_time_v)) {
      # predict risk at horizon time
      # prediction_at_horizon_time <- do.call(prediction_f,args =  list(model, newdata = test_data,horizon_time = i))
      prediction_at_horizon_time <- do.call(prediction_f,arg = list(object = model, testdata = test_data, horizon_time = horizon_time_v[h]))
      res.timeROC <- timeROC(
        T = test_data[[time]],
        delta = test_data[[outcome]],
        marker = prediction_at_horizon_time,
        cause = 1,
        times = horizon_time_v[h],
        ROC = TRUE,
        iid = idd
      )
      if(idd){
        res_l[[h]] <-  data.frame(horizon_time = horizon_time_v[h],AUC = res.timeROC$AUC[2], low2.5 = confint(res.timeROC)$CI_AUC[1], up975 =confint(res.timeROC)$CI_AUC[2])
      } else {
        res_l[[h]] <-  data.frame(horizon_time = horizon_time_v[h],AUC = res.timeROC$AUC[2])
      }
    }
    res <- do.call("rbind",res_l)
    return(res)
  }



# C index -----------------------------------------------------------------

CIndex_LF <- function(model,
                      test_data,
                      prediction_f,
                      outcome,
                      time,
                      horizon_time) {
  prediction_at_horizon_time <- do.call(prediction_f,arg = list(object = model, testdata = test_data, horizon_time = horizon_time))
  surv_obj <-  Surv(test_data[[time]], test_data[[outcome]])
  res <- Cindex(surv_obj, predicted = 1-prediction_at_horizon_time, t_star = horizon_time)
  res <- data.frame(Cindex = res[[1]])
  return(res)
}

# Brier score


Brier_score_LF <- function(model,
                           test_data,
                           prediction_f,
                           outcome,
                           time,
                           horizon_time) {
  res_l <- list()
  for(h in 1:length(horizon_time)){
  prediction_at_horizon_time <- do.call(prediction_f,arg = list(object = model, testdata = test_data, horizon_time = horizon_time[h]))
  surv_obj <-  Surv(test_data[[time]], test_data[[outcome]])
  Brier_score_h <- Brier_LF(surv_obj, pre_sp = 1-prediction_at_horizon_time, t_star = horizon_time[h])
  res_l[[h]] <- data.frame( Brier_score_h, horizon_time = horizon_time[h])
   }
  res <- do.call("rbind",res_l)
  return(res)
}

#  need to use my own function because there is a mistake in the sirvMetric function, see https://github.com/skyee1/SurvMetrics/issues/3 and my brilliant comment
Brier_LF <- function (object, pre_sp, t_star = -1) {
  if (inherits(object, "coxph")) {
    obj <- object
    test_data <- pre_sp
    t_star0 <- t_star
    distime <- sort(unique(as.vector(obj$y[obj$y[, 2] == 
                                             1])))
    if (t_star0 <= 0) {
      t_star0 <- median(distime)
    }
    vec_coxph <- predictSurvProb(obj, test_data, t_star0)
    object_coxph <- Surv(test_data$time, test_data$status)
    object <- object_coxph
    pre_sp <- vec_coxph
    t_star <- t_star0
  }
  if (inherits(object, c("rfsrc"))) {
    obj <- object
    test_data <- pre_sp
    t_star0 <- t_star
    distime <- obj$time.interest
    if (t_star0 <= 0) {
      t_star0 <- median(distime)
    }
    med_index <- order(abs(distime - t_star0))[1]
    mat_rsf <- predict(obj, test_data)$survival
    vec_rsf <- mat_rsf[, med_index]
    object_rsf <- Surv(test_data$time, test_data$status)
    object <- object_rsf
    pre_sp <- vec_rsf
    t_star <- t_star0
  }
  if (inherits(object, c("survreg"))) {
    obj <- object
    test_data <- pre_sp
    t_star0 <- t_star
    distime <- sort(unique(as.vector(obj$y[obj$y[, 2] == 
                                             1])))
    if (t_star0 <= 0) {
      t_star0 <- median(distime)
    }
    pre_sp <- predictSurvProb2survreg(obj, test_data, t_star0)
    object <- Surv(test_data$time, test_data$status)
    t_star <- t_star0
  }
  if (is.na(t_star)) {
    stop("Cannot calculate Brier Score at NA")
  }
  if (t_star <= 0) {
    t_star <- median(object[, 1][object[, 2] == 1])
  }
  if (length(t_star) != 1) {
    stop("Brier Score can only be calculated at a single time point")
  }
  if (!inherits(object, "Surv")) {
    stop("object is not of class Surv")
  }
  if (any(is.na(object))) {
    stop("The input vector cannot have NA")
  }
  if (any(is.na(pre_sp))) {
    stop("The input probability vector cannot have NA")
  }
  if (length(object) != length(pre_sp)) {
    stop("The prediction survival probability and the survival object have different lengths")
  }
  time <- object[, 1]
  status <- object[, 2]
  t_order <- order(time)
  time <- time[t_order]
  status <- status[t_order]
  pre_sp <- pre_sp[t_order]
  sum_before_t <- 0
  sum_after_t <- 0
  Gtstar <- Gt(object, t_star)
  for (i in c(1:length(time))) {
    if (time[i] < t_star & (status[i] == 1)) {
      Gti <- Gt(object, time[i])
      if (is.na(Gti)) {
        next
      }
      sum_before_t <- sum_before_t + 1/Gti * (pre_sp[i])^2
      next
    }
    if (time[i] >= t_star) {
      if (is.na(Gtstar)) {
        next
      }
      sum_after_t <- sum_after_t + 1/Gtstar * (1 - pre_sp[i])^2
    }
  }
  BSvalue <- (sum_before_t + sum_after_t)/length(time)
  names(BSvalue) <- "Brier Score"
  res <- data.frame(Brier_score = round(BSvalue[[1]],4))
  return(res)
}


# IBS (Integrated Brier Score) --------------------------------------------

IBS_score_LF <- function(model,
                         test_data,
                         prediction_f,
                         outcome,
                         time,
                         horizon_time_v){
  #initialise matrix to save prediction at different horizon time
  mat <- matrix(nrow = nrow(test_data), ncol =  length(horizon_time_v))
  for(i in 1:length(horizon_time_v)){
    mat[,i] <- do.call(prediction_f,arg = list(object = model, testdata = test_data, horizon_time = horizon_time_v[i]))
  }
  surv_obj <-  Surv(test_data[[time]], test_data[[outcome]])
  res <- IBS(surv_obj, sp_matrix = mat, horizon_time_v)
  return(res)
}


# calibration plot --------------------------------------------------------
####### Calibration ##########
# largely inspired from https://onlinelibrary.wiley.com/doi/epdf/10.1002/sim.8570


calibration_LF <- function(model, test_data, prediction_f,outcome, time, horizon_time, band) {
  # largely inspired from article and particularly code provided in 
  # supplementary material at
  # https://onlinelibrary.wiley.com/doi/epdf/10.1002/sim.8570
  
  # Return error if dataset is less than 20 people
  if (nrow(test_data) < 20){
    stop('More than 20 people are required for Calibration')
  }
  
  # prediction
  prediction_at_horizon_time <- do.call(prediction_f,arg = list(object = model, testdata = test_data, horizon_time = horizon_time))
  test_data$prediction <- ifelse(prediction_at_horizon_time>=1,0.999,prediction_at_horizon_time)
  test_data$prediction <- ifelse( test_data$prediction<=0,0.001, test_data$prediction)
  test_data$prediction.cll <- log(-log(1-test_data$prediction))
  if(any_na( test_data$prediction.cll)){
    a = 2+2
  }
  ################################################################################
  # Calibration for predictions survival probabilities
  ################################################################################
  # note to future myself, you changed the initial rcs(prediction.cll,3) by 
  # rcs(prediction.cll,c(",paste(knots,collapse = ","),"))" the idea is to put the knots position at meaningful position. 
  # this is due to the fact that in Teddy 95% of children have a probability to develop T1D close to zeros and 
  # so the splines would provide a poor fit by fitting only on the children with very low risk ( by default 3 knots are put at 0.25,0.5 and 0.75 of the population) 
  # and so a poor fit was obtained for high risk
  # you might spend more hours on this problem.
  
  knots <-  quantile(test_data$prediction.cll, na.rm = TRUE, probs = seq(0, 1, 0.2))[2:5]
  
  formula <- formula( paste0("Surv(",time,",",outcome,") ~ rcs(prediction.cll,c(",paste(knots,collapse = ","),"))"))
  calibrate.cox <- coxph(formula,x=T, data=test_data)
  
  predict.grid.cox <- seq(quantile(test_data$prediction,probs=0.01, na.rm = TRUE), quantile(test_data$prediction,probs=0.99, na.rm=TRUE),length=100)
  predict.grid.cox.cll.df <- data.frame(prediction.cll = log(-log(1-predict.grid.cox)))
  
  fit.pred <- predictCox(calibrate.cox,  newdata=predict.grid.cox.cll.df,times=horizon_time, type = "survival", se = FALSE, iid = FALSE, band = band)
  predict.calibrate.cox <- 1 - fit.pred$survival
  if(band){
    band.cox.up <- 1 - fit.pred$survival.lower
    band.cox.low <- 1 - fit.pred$survival.upper
    calibration <- data.frame(predict.grid = predict.grid.cox,predict.calibrate = predict.calibrate.cox,band.low = band.cox.low, band.up = band.cox.up, horizon_time = horizon_time)
  } else{
    calibration <- data.frame(predict.grid = predict.grid.cox,predict.calibrate = predict.calibrate.cox, horizon_time = horizon_time)
  }
  return(calibration)
}

calibration_v_LF <- function(model, test_data, prediction_f,outcome, time, horizon_time_v,band){
  calibration_l <- list()
  for(i in 1:length(horizon_time_v)){
    calibration_l[[i]] <- calibration_LF(model, test_data, prediction_f,outcome, time, horizon_time_v[i],band)
  }
  calibration_dt <- do.call("rbind",calibration_l )
  return(calibration_dt)
}


calibration_figure <- function(dt){
  horizon_time_v <- unique(dt$horizon_time)
  dt_plot <- dt %>% mutate(horizon_time =paste0(horizon_time, " years")) %>% 
    mutate( horizon_time = factor(horizon_time, levels = paste0(horizon_time_v, " years")))
  band <- "band.low" %in% colnames(dt) # assess if the band has been computed
  if(band){
    p_calibration <- ggplot(dt_plot,aes(x = predict.grid, y = predict.calibrate, colour = horizon_time, fill = horizon_time)) +
      geom_line() +
      coord_cartesian(xlim= c(0,1), ylim = c(0,1)) +
      geom_ribbon(aes(ymin = band.low, ymax = band.up), alpha = 0.1, show.legend = FALSE, linetype = 0) +
      geom_abline(intercept = 0, slope = 1, linetype="dotted") +
      xlab("Predicted probability of Type 1 diabetes") +
      ylab("Observed probability of Type 1 diabetes") +
      labs(fill='prediction horizon at') +
      labs(colour='horizon at') +
      theme_bw() +
      theme(legend.position = "bottom")
  }
  else{
    p_calibration <- ggplot(dt_plot,aes(x = predict.grid, y = predict.calibrate, colour = horizon_time, fill = horizon_time)) +
      geom_line() +
      coord_cartesian(xlim= c(0,1), ylim = c(0,1)) +
      geom_abline(intercept = 0, slope = 1, linetype="dotted") +
      xlab("Predicted probability of Type 1 diabetes") +
      ylab("Observed probability of Type 1 diabetes") +
      labs(fill='prediction horizon at') +
      labs(colour='horizon at') +
      theme_bw() +
      theme(legend.position = "bottom")
  }
  return(p_calibration)
}
