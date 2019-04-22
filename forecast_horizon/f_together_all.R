library(dplyr)
library(lubridate)
library(forecast)
library(stringr)

###########################################
######################  Read data
###########################################
sys_vars <- readLines("/etc/bash.bashrc")
rossmann_data_path <- sys_vars[str_detect(sys_vars, "ROSS_DATA_PATH")] %>%
  str_extract('\\".+\\"') %>%
  str_remove_all('\\"')

source("forecast_horizon/functions/f_01_getSplitData_allStores.R")
orig_df <- getSplitData_allStores(file_train = 
                               file.path(rossmann_data_path, "ARIMA_data", "train.feather"), 
                             last_in_train = "2015-05-02")  

#check
max(orig_df$train_df$Date)
min(orig_df$val_df$Date)

df <- orig_df

##################################
######################  Choose stores
df_st <- df$train_df

stores_with_all_data <- df_st %>%
  filter(Sales != 0) %>%
  filter(!is.na(Sales)) %>%
  group_by(Store) %>%
  mutate(number_of_obs = n()) %>%
  ungroup() %>%
  filter(number_of_obs == max(number_of_obs)) %>%
  select(Store) %>%
  arrange(Store) %>% 
  distinct() %>%
  unlist()

stores_with_all_data

df$train_df <- df$train_df %>%
  filter(Store %in% stores_with_all_data)

df$val_df <- df$val_df %>%
  filter(Store %in% stores_with_all_data)

###########################################
######################  Get predictions
###########################################
source("forecast_horizon/functions/arimaMod.R")
source("forecast_horizon/functions/arimaMod_seas7.R")
source("forecast_horizon/functions/arimaXreg.R")
source("forecast_horizon/functions/stlm.R")
source("forecast_horizon/functions/DL.R")

###########################################
######################  Combine
###########################################

res_full <- bind_rows(res_arima, res_DL)
#res_full <- bind_rows(res_arima, res_DL, res_arimaXreg, res_arima_seas6)
#res_full <- bind_rows(res_arima, res_DL, res_arima_seas7, res_stlmArima)
###########################################
######################  Metric
###########################################
res_full <- res_full %>%
  mutate(horizon = as.integer(Date - min(Date)))

##############################################################################

## all stores
unique(res_full$Store)

## largest error?
res_full %>%
  mutate(ape = abs(pred - Sales)/abs(Sales)) %>%
  group_by(model, horizon) %>%
  filter(ape == max(ape)) %>%
  filter(horizon == 1)

max_error_ts <- orig_df$train_df %>%
  filter(Store == 85) %>%
  select(Sales) %>%
  unlist

dygraphs::dygraph(tibble(day = 1:length(max_error_ts), sales = max_error_ts))

fit <- filter(all_models_stlmArima, Store == 423)$mod[[1]]
fit <- filter(all_models_arima, Store == 423)$mod[[1]]

plot(forecast::forecast(fit, 30))

##############################################################################

res_full <- res_full %>%
  mutate(ape = abs(pred - Sales)/abs(Sales)) %>%
  na.omit() %>%
  group_by(model, horizon, Date) %>%
  summarise(mape = mean(ape),
            se = sd(ape)/sqrt(dplyr::n())) %>%
  ungroup()

#res
res_full <- res_full %>%
  mutate(lower = mape - se, 
         upper = mape + se)

###########################################
######################  Plot
###########################################
library(ggplot2)
p <- ggplot(res_full) +
  geom_errorbar(aes(x = Date, ymin=upper, ymax=lower)) +
  geom_point( aes(x = Date, y = mape, fill = model), size = 2, shape=21) +
  ggtitle("MAPE vs horizon")

plot(p)

p2 <- p + facet_grid(~model)
plot(p2)

library(plotly)
ggplotly(p2)

ggsave(p2, filename = "p2.jpeg")
