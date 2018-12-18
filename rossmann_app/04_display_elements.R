###### Display
output$df_metric_out <- DT::renderDT({
  df_metric() 
}, options = list(dom = 't'))


output$df_oneStore_out <- DT::renderDT({
  one_store_df()
},
options = list(
  pageLength = 5
  #initComplete = JS("function(settings, json) {alert('Table displayed');}")
)
)

output$dy_plot <- renderDygraph({
  
  # dyHide <-function(dygraph) {
  #   dyPlugin(
  #     dygraph = dygraph,
  #     name = "Hide",
  #     #path = system.file("plugins/hide.js", package = "dygraphs")
  #     path = "plugins/hide.js"
  #   )
  # }
  
  full_df <- bind_rows(df_splitted()$df_1, df_2_fit())
  
  #browser()
  
  plot_data <- xts::xts(dplyr::select(full_df, Sales, contains("fit")), full_df$Date)
  
  dates_1 <- max(df_splitted()$df_1$Date) - days(10)
  dates_2 <- max(df_splitted()$df_2$Date)
  
  dygraph(plot_data) %>%
    dyRangeSelector(dateWindow = c(dates_1, dates_2)) %>%
    #dyHide() %>%
    dyOptions(drawPoints = T, pointSize = 2)
  
  
  
})

output$model_display_ui <- renderUI({
  #browser()
  selectInput("model_display", "Show info for model",
              names(models_info))
})

output$modInfo_out <- renderPrint({
  #browser()
  names(models_info)
  ifelse(!is_empty(names(models_info)),
         summary(models_info[[input$model_display]]),
         "no models yet")
})

