general_model <- function(df, m_f, mod_name){
  ###########################################
  ######################  Clean data
  ###########################################
  source("forecast_horizon/functions/f_03_clean_data.R")
  df_loc <- df
  df_loc$train_df <- clean_data(df_loc$train_df, remove_sunday = T, fill_na = T)
  df_loc$val_df <- clean_data(df_loc$val_df, remove_sunday = T, fill_na = T)
  
  
  ###########################
  ############### Do models
  if(recalc){
    ############## Train all models (see bellow for parrallel)
    ## all_models_arima <- df_loc$train_df %>%
    ##   group_by(Store) %>%
    ##   filter(Store %in% 1:20) %>%
    ##   do(tibble(mod = list(forecast::auto.arima(.$Sales))))
    
    
    ############## Train all models in parralell
    library(multidplyr)
    cluster <- create_cluster(4)
    all_models_arima <- df_loc$train_df %>%
      filter(Store %in% 1:20) %>%
      partition(Store, cluster = cluster) %>%
      do(dplyr::tibble(mod = list(m_f(.)))) %>%
      collect() %>%
      arrange(Store)
    #system("pkill R")
    save(all_models_arima, file = file.path(rossmann_data_path, "ARIMA_data", paste0(mod_name, "_models.Rdata")))
    
  }else{
    load(file.path(rossmann_data_path, "ARIMA_data", paste0(mod_name, "_models.Rdata")))
  }
  
  #########################################
  ########################### Predict
  
  n_of_days_to_predict <- length(unique(df_loc$val_df$Date))
  n_of_days_to_predict
  
  predictions <- lapply(all_models_arima$mod, function(fit)
    forecast::forecast(fit, n_of_days_to_predict)$mean)
  
  names(predictions) <- paste0("store_", 
                               all_models_arima$Store)
  res <- as_tibble( predictions) 
  res$Date <- unique(df_loc$val_df$Date) %>% sort
  
  res <- res %>%
    tidyr::gather(key = "Store", value = "pred", -Date) %>%
    mutate(Store = as.integer(str_remove(Store, "store_")))
  
  res_arima <- res %>%
    full_join(df_loc$val_df, by = c("Date", "Store")) %>%
    mutate(model = "arima") %>%
    select(Store, Date, Sales, pred, model)
  
}

