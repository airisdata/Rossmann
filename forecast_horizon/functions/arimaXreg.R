############## arimaXreg
source("forecast_horizon/functions/f_04_create_xregVars.R")

###########################################
###################### Xreg vars
###########################################
xreg <- create_xregVars(df)

xreg_df_train <- xreg$xreg_mat_train %>%
  as_tibble %>%
  bind_cols(df$train_df)

xreg_df_val <- xreg$xreg_mat_val %>%
  as_tibble %>%
  bind_cols(df$val_df)


###########################################
######################  Clean data
###########################################
# xreg_df_train <- clean_data(xreg_df_train, remove_sunday = T, fill_na = T)
# xreg_df_val <- clean_data(xreg_df_val, remove_sunday = T, fill_na = T)

###########################################
######################  Make models
###########################################
library(multidplyr)
# system("pkill R")
cluster <- create_cluster(4)
all_models_arimaXreg <- xreg_df_train %>%
  #filter(Store %in% 1:20) %>%
  partition(Store, cluster = cluster) %>%
  do(dplyr::tibble(mod = list(forecast::auto.arima(.$Sales,
            xreg = as.matrix(dplyr::select(., Promo, DayOfWeek, SchoolHoliday,
                                           tidyselect::contains("lag"), tidyselect::contains("lead"))))))) %>%
  collect() %>%
  arrange(Store)


save(all_models_arimaXreg, file = file.path(rossmann_data_path, "ARIMA_data", "all_arimaXreg_models.Rdata"))
#load(file.path(rossmann_data_path, "ARIMA_data", "all_arimaXreg_models.Rdata"))

## try forecast
# forecast(all_models_arimaXreg$mod[[1]], 2,
#          xreg = as.matrix(select(xreg_df_val, Promo, DayOfWeek, SchoolHoliday,
#                                  contains("lag"), contains("lead")
#                                  ))[1:2, ])

###########################################
######################  Predictions
###########################################
xreg_df_val <- xreg_df_val %>% arrange(Date)
new_dates <- unique(xreg_df_val$Date) %>% sort

predictions <- lapply(all_models_arimaXreg$mod, function(fit)
  forecast::forecast(fit, length(new_dates), 
                     xreg = as.matrix(select(xreg_df_val, Promo, DayOfWeek, SchoolHoliday,
                                             contains("lag"), contains("lead")
                                             ))[1:length(new_dates), ])$mean)

names(predictions) <- paste0("store_", all_models_arimaXreg$Store)
res <- as_tibble( predictions) 
res$Date <- new_dates

res <- res %>%
  tidyr::gather(key = "Store", value = "pred", -Date) %>%
  mutate(Store = as.integer(str_remove(Store, "store_")))

res_arimaXreg <- res %>%
  full_join(xreg_df_val, by = c("Date", "Store")) %>%
  mutate(model = "arimaXreg") %>%
  select(Store, Date, Sales, pred, model)

