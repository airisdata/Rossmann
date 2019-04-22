df_loc <- df

ff_func <- function(x){
  library(TSA)
  #browser()
  p  <- periodogram(x)
  
  dd = data.frame(freq=p$freq, spec=p$spec)
  order = dd[order(-dd$spec),]
  top2 = head(order, 2)

  if(1/top2$f[1] > 0.3 * length(x))
    use_freq <- 1/top2$f[2]
  else
    use_freq <- 1/top2$f[1]
  
  if(is.na(use_freq) | use_freq > 0.3* length(x))
    use_freq <- 1/top2$f[3]
  

  if(is.na(use_freq) | use_freq > 0.3* length(x))
    use_freq <- 7
  
  
  print(paste("Frequency for stlm: ", use_freq))
    
  xx <- ts(x, frequency = use_freq)
  fit <- stlm(xx, method = "arima")
  fit
}


############## Train all models (see bellow for parrallel)
all_models_stlmArima <- df_loc$train_df %>%
  group_by(Store) %>%
  #filter(Store %in% 1:20) %>%
  arrange(Date) %>%
  do(tibble(mod = list(ff_func(.$Sales))))


n_of_days_to_predict <- length(unique(df_loc$val_df$Date))
n_of_days_to_predict

predictions <- lapply(all_models_stlmArima$mod, function(fit)
  forecast::forecast(fit, n_of_days_to_predict)$mean)

names(predictions) <- paste0("store_", 
                             all_models_stlmArima$Store)
res <- as_tibble( predictions) 
res$Date <- unique(df_loc$val_df$Date) %>% sort

res <- res %>%
  tidyr::gather(key = "Store", value = "pred", -Date) %>%
  mutate(Store = as.integer(str_remove(Store, "store_")))

res_stlmArima <- res %>%
  full_join(df_loc$val_df, by = c("Date", "Store")) %>%
  mutate(model = "stlmArima") %>%
  select(Store, Date, Sales, pred, model)
