##########################################
##########################################
getSplitData_allStores <- function(file_train = "../dataFiles/train.csv", 
                      days_in_val = 60){
  
  print("Reading data")
  # df <- feather::read_feather(file_train) %>%
  #   arrange(Date) %>%
  #   mutate(Date = ymd(Date))
  
  df <- readr::read_csv(file_train) %>%
    arrange(Date) %>%
    mutate(Date = ymd(Date))
  
  sequence(range(df$Date))
  
  all_dates <- seq.Date(from = min(df$Date), 
           to = max(df$Date), by = "day")  
  
  cut_date <- tail(all_dates, days_in_val)[1]
  
  train_df <- df %>%
    dplyr::filter(Date <= cut_date)
  
  val_df <- df %>%
    dplyr::filter(Date > cut_date)
  
  list(train_df = train_df, val_df = val_df)
}

# example
# df <- getSplitData_allStores(file_train = "dataFiles/train.csv", 
#                              ratio = 0.7)
# df$train_df %>% filter(Store == 1)
