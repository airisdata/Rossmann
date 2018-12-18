#######################################
############ Include all the models funcitons

df_2_fit <- reactive({
  #browser()
  df_2_fit <- df_splitted()$df_2
  
  if("arima" %in% input$models_to_train)
    df_2_fit$fit_arima <- fit_and_forcst_arima()
  
  if("arima_xreg" %in% input$models_to_train)
    df_2_fit$fit_arima_xreg <- fit_and_forcst_arima_xreg()
  
  if("ets" %in% input$models_to_train)
    df_2_fit$fit_ets <- fit_and_forcst_ets()
  
  if("nnetar" %in% input$models_to_train)
    df_2_fit$fit_nnetar <- fit_and_forcst_nnetar()
  
  if("nnetar_xreg" %in% input$models_to_train)
    df_2_fit$fit_nnetar_xreg <- fit_and_forcst_nnetar()
  
  if("setar" %in% input$models_to_train)
    df_2_fit$fit_setar <- fit_and_forcst_setar()
  
  if("tvar" %in% input$models_to_train)
    df_2_fit$fit_tvar <- fit_and_forcst_tvar()
  
  if("tbats" %in% input$models_to_train)
    df_2_fit$fit_tvar <- fit_and_forcst_tbats()
  
  if("prophet_xreg_check" %in% input$models_to_train)
    df_2_fit$fit_propher <- fit_and_forcst_prophet()
  
  if("xgboost_xreg" %in% input$models_to_train)
    df_2_fit$fit_xgboost <- fit_and_forcst_xgboost()
  
  
  df_2_fit
})