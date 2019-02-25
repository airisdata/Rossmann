############# arima
fit_and_forcst_arima <- reactive({
  
  withProgress(message = 'Make model:', value = 0, {
    incProgress(1/3, detail = paste("arima"))
    fit <- auto.arima(df_splitted()$x)
    models_info$fit_arima <- fit
  })
  
  forecast(fit, h = input$max_pr_hor)$mean
})

############# arima
fit_and_forcst_arima_xreg <- reactive({
  
  withProgress(message = 'Make model:', value = 0, {
    incProgress(1/3, detail = paste("arima"))
    fit <- auto.arima(df_splitted()$x, xreg = df_splitted()$xreg_mat_1)
    models_info$fit_arima_xreg <- fit
  })
  
  forecast(fit, h = input$max_pr_hor, xreg = df_splitted()$xreg_mat_2)$mean
})

############# ets
fit_and_forcst_ets <- reactive({
  
  withProgress(message = 'Make model:', value = 0, {
    incProgress(1/3, detail = paste("ets"))
    fit <- ets(df_splitted()$x)
    models_info$fit_ets <- fit
  })
  
  forecast(fit, h = input$max_pr_hor)$mean
})

############# nnetar
fit_and_forcst_nnetar <- reactive({
  #browser()
  withProgress(message = 'Make model:', value = 0, {
    incProgress(1/3, detail = paste("nnetar"))
    fit <- nnetar(df_splitted()$x)
    models_info$fit_nnetar <- fit
  })
  
  forecast(fit, h = input$max_pr_hor)$mean
})

############# nnetar_xreg
fit_and_forcst_nnetar_xreg <- reactive({
  
  withProgress(message = 'Make model:', value = 0, {
    incProgress(1/3, detail = paste("nnetar_xreg"))
    fit <- nnetar(df_splitted()$x, xreg = df_splitted()$xreg_mat_1)
    models_info$fit_nnetar_xreg <- fit
  })
  
  forecast(fit, h = input$max_pr_hor, xreg = df_splitted()$xreg_mat_2)$mean
})


############# hybrid
fit_and_forcst_hybrid <- reactive({
  #browser()
  withProgress(message = 'Make model:', value = 0, {
    incProgress(1/3, detail = paste("hybrid (arima-nnet"))
    #browser()
    
    #fit_a <- auto.arima(df_splitted()$x)
    #fit_n <- nnetar(fit_a$residuals)    
    
    fit_a <- nnetar(df_splitted()$x)
    fit_n <- arima(fit_a$residuals)    
    
    #models_info$fit_hybrid <- paste(fit_a, fit_n)
    models_info$fit_hybrid <- "hybrid"
  })
  
  forecast(fit_a, h = input$max_pr_hor)$mean +
    forecast(fit_n, h = input$max_pr_hor)$mean
})

############# setar
fit_and_forcst_setar <- reactive({

  #browser()
  withProgress(message = 'Make model:', value = 0, {
    incProgress(1/3, detail = paste("setar"))
    #browser()
    
    x <- df_splitted()$x
    #x <- x[!is.na(x)]

    detach(package:dplyr) ## uload, otherwise lag from stats is masked
    res <- selectSETAR(x, m=5, plot = F)

    par_v <- res$bests
    par_v <- par_v[!stringr::str_detect(names(par_v), "AIC")] 
    par_v
    fit <- do.call(setar, c(list(x = x), par_v))
    
    library(dplyr)
    
    models_info$fit_setar <- fit
  })
  
  predict(fit, n.ahead = input$max_pr_hor)
})


############# tvar
fit_and_forcst_tvar <- eventReactive(input$update_model_but, {
  
  withProgress(message = 'Make model:', value = 0, {
    incProgress(1/3, detail = paste("tvar"))
    
    
    st <- as.numeric(input$store)
    loc_df <- dfs()$tr_df 
    
    if(input$remove_sunday)
      loc_df <- loc_df %>%
      dplyr::filter(DayOfWeek != 7)
    
    if(input$fill_na) {
      loc_df <- loc_df %>%
        mutate(Sales = ifelse(Sales == 0, NA, Sales)) %>%
        group_by(Store) %>%
        tidyr::fill(Sales) %>%
        ungroup()
      
      loc_df <- loc_df %>%
        na.omit ## remove rows for which fill did not work - first row for example
    }
    
    
    ############ outliers
    loc_df <- loc_df %>%
      group_by(Store) %>%
      arrange(Date) %>%
      ungroup()
    
    x <- loc_df %>% dplyr::filter(Store == input$store) %>% 
      dplyr::select(Sales) %>% unlist
    
    if(input$put_in_outlier){
      x[round(length(x)/2)]  <- 
        input$outlier_multiplier * sd(x, na.rm = T) + mean(x, na.rm = T)
      
    }
    
    if(input$clean_outliers)
      x <- x %>% tsclean()
    
    loc_df[loc_df$Store == input$store , "Sales"] <- x
    
    
    ##############
    
    
    
    #browser()
    set.seed(input$tvar_seed)
    other_stores <- loc_df$Store %>% unique()  
    other_stores <- other_stores[other_stores != input$store]
    chosen_stores <- sample(1:length(other_stores), 
                            min(input$tvar_numOther, 
                                length(other_stores)))
    
    someStores_df <- loc_df %>%
      dplyr::filter(Store %in%  c(chosen_stores, input$store)) %>%
      mutate(Store = paste0("store_", Store)) %>%
      dplyr::select(Store, Sales, Date) %>%
      spread(key = Store, value = Sales) %>%
      arrange(Date) %>%
      na.omit() %>%
      dplyr::select(contains("store"))
    
    
    fit <- TVAR(someStores_df, 
                lag=input$tvar_lags, plot=FALSE)
    
    
    models_info$fit_tvar <- fit
  }  
  )
  
  predict(fit, n.ahead = input$max_pr_hor) %>% as_tibble() %>%
    dplyr::select(paste0("store_", as.numeric(input$store))) %>%
    unlist
})



############# tbats
fit_and_forcst_tbats <- reactive({
  
  withProgress(message = 'Make model:', value = 0, {
    incProgress(1/3, detail = paste("tbats"))
    fit <- tbats(df_splitted()$x)
    models_info$fit_tbats <- fit
  })
  
  forecast(fit, h = input$max_pr_hor)$mean
})


############# prophet
## https://facebook.github.io/prophet/docs/seasonality,_holiday_effects,_and_regressors.html

fit_and_forcst_prophet <- reactive({
  
  withProgress(message = 'Make model:', value = 0, {
    incProgress(1/3, detail = paste("prophet"))
    #browser()
    loc_df <- df_splitted()$df_1 %>% 
      dplyr::select(Date, Sales, Promo, StateHoliday, Promo_lag, Promo_lag2, Promo_lead) %>%
      rename(ds = Date, y = Sales)
    
    fit <- prophet()
    for(nn in c("Promo", "StateHoliday", "Promo_lag", "Promo_lag2", "Promo_lead"))
      fit <- add_regressor(fit, nn)  
    
    
    fit <- prophet(loc_df)
    
    
    models_info$fit_prophet <- fit
  })
  
  dates_df <- tibble(
    ds = (max(loc_df$ds) + 1):(max(loc_df$ds) + 
                                 days(2* input$max_pr_hor) ) %>% 
      as.Date() 
  )
   
  
  dates_df <- make_future_dataframe(fit, periods = input$max_pr_hor) %>%
    head(48)
      
  forecast <- predict(fit, dates_df)
  
  forecast$yhat
})

############# xgboost
fit_and_forcst_xgboost <- reactive({
  #browser()
  withProgress(message = 'Make model:', value = 0, {
    incProgress(1/3, detail = paste("xgboost"))
    
    fit <- xgbar(df_splitted()$x, xreg = df_splitted()$xreg_mat_1)
    models_info$fit_nxgboost <- summary(fit)
  })
  
  forecast(fit, h = input$max_pr_hor, xreg = df_splitted()$xreg_mat_2)$mean
})


### deep learning from B
fit_and_forcst_DL <- reactive({
  library(feather)
  library(dplyr)
  
  withProgress(message = 'Loading DL results', value = 0, {
    incProgress(1/3, detail = paste("now"))
    
    #nn_tr <- feather::read_feather("/mnt/disks/data_disk/rossmann_data/neural_net_predictions/train_preds_df_func_full_model.feather")
    
    nn_val <-  feather::read_feather(
      file.path(rossmann_data_path, "neural_net_predictions", 
                "valid_preds_df_func_full_model.feather")
    )
    
    #nn_val <- feather::read_feather("/mnt/disks/data_disk/rossmann_data/neural_net_predictions/valid_preds_df_func_full_model.feather")
    #nn_tr$data_type <- "tr"
    #nn_val$data_type <- "val"    

  })

  
  #browser()
  split_date <- one_store_df()$Date %>% unique() %>% 
    tail(input$max_pr_hor) %>% `[`(1)
  
  
  st <- as.numeric(input$store)
  
  nn_val <- nn_val %>%
    dplyr::filter(Store == st) 
  
  
  nn_val %>%
    dplyr::filter(Date >= split_date) %>%
    #mutate(Date = as.character(Date)) %>%
    #dplyr::filter(Date %in% dates_in_test)
    dplyr::select(PredictedSales) %>%
    unlist()
      
    
})


