### read all data
dfs <- reactive({
  withProgress(message = 'Loading data', value = 0, {
    incProgress(1/3, detail = paste("training"))
    
    tr_df <- feather::read_feather("../dataFiles/train.feather") %>%
      arrange(Date) %>%
      mutate(Date = ymd(Date))
    
    incProgress(1/3, detail = paste("test"))
    
    ts_df <- feather::read_feather("../dataFiles/test.feather") %>%
      arrange(Date) %>%
      mutate(Date = ymd(Date))
  })
  
  lst(tr_df = tr_df, ts_df = ts_df)
})


## subset data - one store only
one_store_df <- reactive({
  #browser()
  st <- as.numeric(input$store)
  loc_df <- dfs()$tr_df %>%
    dplyr::filter(Store == st) 
  
  if(input$remove_sunday)
    loc_df <- loc_df %>%
    dplyr::filter(DayOfWeek != 7)
  
  if(input$fill_na) {
    loc_df <- loc_df %>%
      mutate(Sales = ifelse(Sales == 0, NA, Sales)) %>%
      tidyr::fill(Sales) 
    
    loc_df <- loc_df %>%
      na.omit ## remove rows for which fill did not work - first row for example
  }
  loc_df
})


## split data to train/validation
df_splitted <- eventReactive(input$update_model_but, {
  #pr_hor <- input$max_pr_hor
  #browser()
  split_date <- one_store_df()$Date %>% unique() %>% 
    tail(input$max_pr_hor) %>% `[`(1)
  
  df <- one_store_df()
  
  df <- df %>%
    mutate(DayOfWeek = as.character(DayOfWeek)) %>%
    mutate(Promo_lag = dplyr::lag(Promo)) %>%
    mutate(Promo_lag2 = dplyr::lag(Promo, 2)) %>%
    mutate(Promo_lead = lead(Promo)) #%>%
  #mutate(Promo_lead2 = lead(Promo, 2))#%>%
  #mutate(Hol_lag = dplyr::lag(StateHoliday)) %>%
  #mutate(Hol_lag2 = dplyr::lag(StateHoliday, 2)) %>%
  #mutate(Hol_lead = lead(StateHoliday)) #%>%
  #mutate(Hol_lead2 = lead(StateHoliday, 2))
  #mutate(Month = as.character(month(Date)))
  #mutate(Promo_lag3 = dplyr::lag(Promo, 3))
  
  # df <- df %>%
  #   mutate(as.factor(DayOfWeek))
  
  # df <- df %>%
  #   na.omit()  # often first row may be missing
  
  df_1 <- df %>%
    dplyr::filter(Date < split_date)
  
  #browser()
  xreg_mat_1 <- df_1 %>% dplyr::select(Promo, DayOfWeek, SchoolHoliday, 
                                contains("lag"), contains("lead"))#, Month)
  dummies <- caret::dummyVars(~ ., data = xreg_mat_1, fullRank = T)
  xreg_mat_1 <- predict(dummies, newdata = xreg_mat_1) %>% as.matrix()
  
  df_2 <- df %>%
    dplyr::filter(Date >= split_date)
  
  xreg_mat_2 <- df_2 %>% dplyr::select(Promo, DayOfWeek, SchoolHoliday, 
                                contains("lag"), contains("lead"))#, Month)
  xreg_mat_2 <- predict(dummies, newdata = xreg_mat_2) %>% as.matrix()
  
  
  x <- ts(df_1$Sales, frequency = input$seasonality)
  if(input$put_in_outlier){
    x[round(length(x)/2)]  <- 
      input$outlier_multiplier * sd(x, na.rm = T) + mean(x, na.rm = T)
    
  }
  
  if(input$clean_outliers)
    x <- x %>% tsclean()
  
  df_1[ , "Sales"] <- x
  
  list(df_1 = df_1, df_2 = df_2, x = x, 
       xreg_mat_1 = xreg_mat_1, xreg_mat_2 = xreg_mat_2)
  
}, ignoreNULL = F)

