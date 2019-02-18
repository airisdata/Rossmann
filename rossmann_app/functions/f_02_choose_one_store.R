choose_one_store <- function(df, store_number = 1){
  
  train_df <- df$train_df %>%
    dplyr::filter(Store == store_number) 
  
  val_df <- df$val_df %>%
    dplyr::filter(Store == store_number) 
  
  list(train_df = train_df, val_df = val_df)
}
#example
#choose_one_store(df, store_number = 10)
