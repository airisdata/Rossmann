############ read data
nn_val <-  feather::read_feather(
  file.path(rossmann_data_path, "neural_net_predictions", 
            "valid_preds_df_func_full_model.feather")
)

########### do cleaning
res_DL <- nn_val %>%
  rename(pred = PredictedSales) %>%
  mutate(Date = lubridate::ymd(Date)) %>% 
  select(Store, Date, Sales, pred, DayOfWeek) %>%
  clean_data(remove_sunday = T, fill_na = T) %>%
  mutate(model = "DL")  %>%
  select(-DayOfWeek)
