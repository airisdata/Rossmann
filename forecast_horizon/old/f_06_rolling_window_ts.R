rolling_window_ts <- function(one_store_df, window = -1){
  
  train_ts <- one_store_df$train_df$Sales
  val_ts <- one_store_df$val_df$Sales
  
  one_store_ts <- c(train_ts, val_ts)
  
  if(window == -1)
    window <- length(train_ts)
  
  # function
  model_and_meanForecats <- function(n_ofObs_fromVal_inModel) {
    
    #print(paste("model and predict, including", n_ofObs_fromVal_inModel, 
    #            "observations from validation"))
 
    last_ind <- length(train_ts) + n_ofObs_fromVal_inModel
    first_ind <- last_ind - window + 1
    
    fit <- auto.arima(one_store_ts[first_ind:last_ind])
    
    
    max_h <- length(one_store_ts) - last_ind
    forecast_all_h <- forecast(fit, h = max_h)$mean
    
    res <- tibble(
      n_ofObs_fromVal_inModel = rep(n_ofObs_fromVal_inModel, max_h),
      h = 1:max_h,
      forecast_v = forecast_all_h,
      actual = one_store_ts[(last_ind+1):length(one_store_ts)]
    )
    
    res
  }
  
  library(pbapply)
  res <- pblapply(0:(length(val_ts) - 1), model_and_meanForecats)
  bind_rows(res)
}

#res <- rolling_window_ts(one_store_df )
#res
