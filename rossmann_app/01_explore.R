### distortion

distortion <- reactive({
  distortion <- rep(0, 100)
  distortion[25] <- 10
  distortion[50] <- 5
  distortion
})

output$distortion_plot <- renderPlot({
  ggplot(data.frame(x = 1:length(distortion()), y = distortion())) +
    geom_line(aes(x = x, y = y))
})

output$distortion_plot_repeat <- renderPlot({
  ggplot(data.frame(x = 1:length(distortion()), y = distortion())) +
    geom_line(aes(x = x, y = y))
})

output$ar1_plot <- renderPlot({
  val <- distortion()[1]
  for(i in 2:length(distortion())){
    val[i] = input$ar1_coeff  * val[i-1] + distortion()[i]
  }
  
  ggplot(data.frame(x = 1:length(val), y = val)) +
    geom_line(aes(x = x, y = y))
})

output$ar2_plot <- renderPlot({
  val <- distortion()[1:2]
  for(i in 3:length(distortion())){
    val[i] = input$ar2_coeff1  * val[i-1] + input$ar2_coeff2  * val[i-2] + distortion()[i]
  }
  
  ggplot(data.frame(x = 1:length(val), y = val)) +
    geom_line(aes(x = x, y = y))
})


output$ma1_plot <- renderPlot({
  val <- distortion()[1]
  for(i in 2:length(distortion())){
    val[i] = input$ma1_coeff * distortion()[i-1] + distortion()[i]
  }
  
  ggplot(data.frame(x = 1:length(val), y = val)) +
    geom_point(aes(x = x, y = y), size = 1) +
    geom_linerange(aes(x = x, ymax = y, ymin = 0))
})

output$ma2_plot <- renderPlot({
  val <- distortion()[1:2]
  for(i in 3:length(distortion())){
    val[i] = input$ma2_coeff2 * distortion()[i-1] +
      input$ma2_coeff2 * distortion()[i-2] + distortion()[i]
  }
  
  ggplot(data.frame(x = 1:length(val), y = val)) +
    geom_point(aes(x = x, y = y), size = 1) +
    geom_linerange(aes(x = x, ymax = y, ymin = 0))
})

