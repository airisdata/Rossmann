model_arima <- function(one_ts){
  auto.arima(one_ts)
}

forecast_ts <- function(fit, ...){
  forecast(fit, h = input$max_pr_hor, ...)$mean
}


