clean_data <- function(df, 
                       remove_sunday = T,
                       fill_na = T
                       ){
  
  train_df <- df$train_df
  val_df <- df$val_df
  
  ######################### Sunday
  if(remove_sunday){
    train_df <- train_df %>%
      dplyr::filter(DayOfWeek != 7)
    
    val_df <- val_df %>%
      dplyr::filter(DayOfWeek != 7)
  }
      
  ######################### NA
  if(fill_na) {
    train_df <- train_df %>%
      mutate(Sales = ifelse(Sales == 0, NA, Sales)) %>%
      tidyr::fill(Sales) 
    
    val_df <- val_df %>%
      mutate(Sales = ifelse(Sales == 0, NA, Sales)) %>%
      tidyr::fill(Sales) 
    
    ## remove rows for which fill did not work - first row for example
    train_df <- train_df %>%
      na.omit 
    
    val_df <- val_df %>%
      na.omit 
  }
  
  list(train_df = train_df, val_df = val_df)
}