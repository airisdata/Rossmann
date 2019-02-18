create_xregVars <- function(df){
  
  train_df <- df$train_df %>%
    ungroup() %>%
    group_by(Store) %>%
    arrange(Date)
  
  val_df <- df$val_df %>%
    ungroup() %>%
    group_by(Store) %>%
    arrange(Date)
  

  ######################################################
  ######################################################
  train_df <- train_df %>%
    mutate(DayOfWeek = as.character(DayOfWeek)) %>%
    mutate(Promo_lag = dplyr::lag(Promo)) %>%
    mutate(Promo_lag2 = dplyr::lag(Promo, 2)) %>%
    mutate(Promo_lead = lead(Promo)) %>%
    ungroup()
  
  val_df <- val_df %>%
    mutate(DayOfWeek = as.character(DayOfWeek)) %>%
    mutate(Promo_lag = dplyr::lag(Promo)) %>%
    mutate(Promo_lag2 = dplyr::lag(Promo, 2)) %>%
    mutate(Promo_lead = lead(Promo)) %>%
    ungroup()
  
  ######################################################
  ######################################################
  xreg_mat_train <- train_df %>% 
    dplyr::select(Promo, DayOfWeek, SchoolHoliday, 
                  contains("lag"), contains("lead"))#, Month)
  
  ## create dummies rule
  dummies_rule <- caret::dummyVars(~ ., data = xreg_mat_train, fullRank = T)
  
  xreg_mat_train <- predict(dummies_rule, newdata = xreg_mat_train) %>% 
    as.matrix()
  
  ###########################################################
  xreg_mat_val <- val_df %>% 
    dplyr::select(Promo, DayOfWeek, SchoolHoliday, 
                  contains("lag"), contains("lead"))#, Month)
  
  xreg_mat_val <- predict(dummies_rule, newdata = xreg_mat_val) %>% 
    as.matrix()
  
  list(xreg_mat_train = xreg_mat_train, xreg_mat_val = xreg_mat_val)
}

# Example
#create_xregVars(df)


# x <- ts(df_1$Sales, frequency = input$seasonality)
# if(input$put_in_outlier){
#   x[round(length(x)/2)]  <- 
#     input$outlier_multiplier * sd(x, na.rm = T) + mean(x, na.rm = T)
#   
# }
# 
# if(input$clean_outliers)
#   x <- x %>% tsclean()
# 
# df_1[ , "Sales"] <- x
