df_metric <- reactive({
  #browser()
  df <- df_2_fit() %>%
    dplyr::select("Sales", contains("fit")) 
  
  if(ncol(df) == 1) ## no fit yet
    return(NULL)
  
  names(df) <- str_remove_all(names(df), "fit_") 
  df %>%
    na.omit() %>% ## just in case if one of models fails in some of positions
    gather(key = "model", value = "fitted", -Sales) %>%
    group_by(model) %>%
    mutate(e = Sales - fitted) %>%
    mutate(rmse = signif(sqrt(mean(e^2)), digits = 4),
           rmspe = signif(sqrt(mean((e/Sales)^2)), digits = 4),
           mae = signif(mean(abs(e)), digits = 4)) %>%
    dplyr::select(-fitted, -e, -Sales) %>%
    distinct() %>%
    arrange(rmse)
})