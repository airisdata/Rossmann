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

###########################################
######################  Get predictions
###########################################
source("forecast_horizon/functions/arimaMod.R")
source("forecast_horizon/functions/arimaXreg.R")
source("forecast_horizon/functions/DL.R")

###########################################
######################  Combine
###########################################

#res <- bind_rows(res_arima, res_DL)
res_full <- bind_rows(res_arima, res_DL, res_arimaXreg)

###########################################
######################  Metric
###########################################
res_full <- res_full %>%
  mutate(horizon = as.integer(Date - min(Date)))

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

