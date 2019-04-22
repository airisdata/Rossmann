###########################################
######################  Clean data
###########################################
#source("forecast_horizon/functions/f_03_clean_data.R")

df_loc <- df

#df_loc$train_df <- clean_data(df_loc$train_df, remove_sunday = T, fill_na = T)
#df_loc$val_df <- clean_data(df_loc$val_df, remove_sunday = T, fill_na = T)


############ Train all models (see bellow for parrallel)
all_models_arima_seas7 <- df_loc$train_df %>%
  group_by(Store) %>%
  #filter(Store %in% 1:20) %>%
  arrange(Date) %>%
  do(tibble(mod = list(forecast::auto.arima(ts(.$Sales, frequency = 7)))))

############## Train all models in parralell
# library(multidplyr)
# cluster <- create_cluster(4)
# all_models_arima_seas7 <- df_loc$train_df %>%
#   #filter(Store %in% 1:20) %>%
#   partition(Store, cluster = cluster) %>%
#   do(dplyr::tibble(mod = list(forecast::auto.arima(stats::ts(.$Sales, frequency = 7))))) %>%
#   collect() %>%
#   arrange(Store)
# #system("pkill R")
# 
# save(all_models_arima_seas7, file = file.path(rossmann_data_path, "ARIMA_data", "all_arima_seas7_models.Rdata"))
# #load(file.path(rossmann_data_path, "ARIMA_data", "all_arima_seas7_models.Rdata"))

#check
plot(forecast(all_models_arima_seas7$mod[[1]]))

bic_arima <- sapply(1:length(all_models_arima$mod), function(x) all_models_arima_seas7$mod[[x]]$bic)

###############
check_this <- which(bic_arima == max(bic_arima))
plot(forecast(all_models_arima$mod[[check_this]]))
################
check_this <- which(bic_arima == min(bic_arima))
plot(forecast(all_models_arima$mod[[check_this]]))
###############


n_of_days_to_predict <- length(unique(df_loc$val_df$Date))
n_of_days_to_predict

predictions <- lapply(all_models_arima_seas7$mod, function(fit)
  forecast::forecast(fit, n_of_days_to_predict)$mean)



names(predictions) <- paste0("store_", all_models_arima_seas7$Store)
res <- as_tibble( predictions) 
res$Date <- unique(df_loc$val_df$Date) %>% sort

res <- res %>%
  tidyr::gather(key = "Store", value = "pred", -Date) %>%
  mutate(Store = as.integer(str_remove(Store, "store_")))

res_arima_seas7 <- res %>%
  full_join(df_loc$val_df, by = c("Date", "Store")) %>%
  mutate(model = "arima_seas7") %>%
  select(Store, Date, Sales, pred, model)
