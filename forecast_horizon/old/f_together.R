library(dplyr)
library(lubridate)
library(forecast)
library(stringr)

for(f in dir("forecast_horizon/functions/")){
  print(paste0("forecast_horizon/functions/", f))
  source(paste0("forecast_horizon/functions/", f))
}

sys_vars <- readLines("/etc/bash.bashrc")
rossmann_data_path <- sys_vars[str_detect(sys_vars, "ROSS_DATA_PATH")] %>%
  str_extract('\\".+\\"') %>%
  str_remove_all('\\"')

df <- getSplitData_allStores(file_train = 
                               file.path(rossmann_data_path, "ARIMA_data", "train.feather"), 
                             days_in_val = 60)  

xreg <- create_xregVars(df)

df <- clean_data(df, remove_sunday = T, fill_na = T)

one_store_df <- choose_one_store(df, 3)

#model_arima(one_store_df$train_df$Sales)

###

#rolling_window_ts(one_store_df, h = 1)
res_calculated <- rolling_window_ts(one_store_df)

res <- res_calculated %>%
  mutate(ape = abs(forecast_v - actual)/abs(actual)) %>%
  group_by(h) %>%
  ## exclude cases, where there is only small number of observations
  filter(n() > 5) %>%
  summarise(mape = mean(ape),
            se = sd(ape)/sqrt(dplyr::n())) %>%
  ungroup() %>%
  rename(horizon = h)


res <- res %>%
  mutate(lower = mape - se, 
         upper = mape + se)

library(ggplot2)
ggplot(res) +
  geom_errorbar(aes(x = horizon, ymin=upper, ymax=lower)) +
  geom_point( aes(x = horizon, y = mape), size = 4, shape=21, 
              fill="blue") +
  ggtitle("MAPE vs horizon")
  

  
