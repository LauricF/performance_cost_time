# prediction for each class

prediction_LF <- function(object, ...) {
  extra <- list(...)
  if( "coxph" %in%  class(object)){
    # res <- do.call(predict,arg = list(object = object, newdata = extra$testdata, time = extra$horizon_time))
  res <- 1-c(predictCox(object = object, newdata = extra$testdata, times = extra$horizon_time)$survival)
    # fit <- survfit(object, newdata = extra$testdata)
    # prediction_dt  <- surv_summary(fit, data = extra$testdata)
    # # prediction_dt <- res.sum %>% left_join(child_to_predict)
    # prediction_dt <- prediction_dt %>% mutate(risk = (1- surv) *100, up = (1 -lower)*100, low = (1- upper)*100)
    # # prediction_dt <- prediction_dt %>% left_join(child_to_predict)
    #   res <- prediction_dt %>% 
    #     mutate(strata = as.numeric(as.character(strata))) %>% 
    #    group_by(strata) %>% 
    #    dplyr::summarize(risk  = approx(time, risk,  xout=extra$horizon_time)$y,
    #                     up = approx(time, up,  xout=extra$horizon_time)$y,
    #                     low = approx(time, low,  xout=extra$horizon_time)$y,
    #                     time = extra$horizon_time) 
    
     }
  if("rfsrc" %in%  class(object)){
      res_pred <- predict(object = object, newdata = data.frame(extra$testdata), type = "prob")
      index.time.interest <- which.min(abs(res_pred$time.interest - extra$horizon_time)) # find closest time from the horizon time we want to predict to
      res <- 1-res_pred$survival[,index.time.interest] # extract the  prediction at this time
  }
  if(!exists("res",inherits = FALSE)){
    res <- NA
    message("The class of the object, ", class(object) ,", is unknown")
  }
  return(res)
}
