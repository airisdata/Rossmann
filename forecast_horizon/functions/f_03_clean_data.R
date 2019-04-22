clean_data <- function(df, 
                       remove_sunday = T,
                       fill_na = T
                       ){

  # ######################### Sunday
  # if(remove_sunday){
  #   df <- df %>%
  #     dplyr::filter(DayOfWeek != 7)
  # }
  #     
  # ######################### NA
  # if(fill_na) {
  #   df <- df %>%
  #     filter(! Sales == 0) %>%
  #     #mutate(Sales = ifelse(Sales == 0, NA, Sales)) %>%
  #     tidyr::fill(Sales) 
  #   
  #   ## remove rows for which fill did not work - first row for example
  #   df <- df %>%
  #     filter(!is.na(Sales)) %>%
  #     arrange(Date)
  # }
  # 
  # df %>% na.omit
  df
}