tabItem_explore <- tabItem(
  tabName = "arma",
  fluidRow(
    box(width = 6, 
        box(title = "Distortions plot", 
            plotOutput("distortion_plot", width = 300, height = 200),
            width = 12
        ),
        
        box(
          withMathJax(),
          helpText('AR(1) with only predictable distortions'),
          helpText("$$x_t = \\phi  * x_{t-1} + e_t$$"),
          sliderInput("ar1_coeff", "Coefficient in AR1", -1, 1, step = 0.01, value = 0.9, 
                      width = '50%'),
          plotOutput("ar1_plot", width = 300, height = 200),
          width = 12
        ),
        
        
        box(
          helpText('AR(2) with only predictable distortions'),
          helpText("$$x_t = \\phi_1  * x_{t-1} + \\phi_2  * x_{t-2} + e_t$$"),
          sliderInput("ar2_coeff1", "1st Coefficient in AR2", -1, 1, step = 0.01, value = 0.8, 
                      width = '50%'),
          sliderInput("ar2_coeff2", "2nd Coefficient in AR2", -1, 1, step = 0.01, value = -0.6, 
                      width = '50%'),
          plotOutput("ar2_plot", width = 300, height = 200),
          width = 12
        )
        
        
    ),
    
    box(width = 6,
        box(title = "Distortions plot", 
            plotOutput("distortion_plot_repeat", width = 300, height = 200),
            width = 12
        ),
        
        box(
          withMathJax(),
          helpText('MA(1) with only predictable distortions'),
          helpText("$$x_t = \\phi  * e_{t-1} + e_t$$"),
          sliderInput("ma1_coeff", "Coefficient in MA1", -1, 1, step = 0.01, value = 0.9,
                      width = '50%'),
          plotOutput("ma1_plot", width = 300, height = 200),
          width = 12
        ),
        
        
        box(
          helpText('MA(2) with only predictable distortions'),
          helpText("$$x_t = \\phi_1  * e_{t-1} + \\phi_2  * e_{t-2} + e_t$$"),
          sliderInput("ma2_coeff1", "1st Coefficient in MA2", -1, 1, step = 0.01, value = 0.8,
                      width = '50%'),
          sliderInput("ma2_coeff2", "2nd Coefficient in MA2", -1, 1, step = 0.01, value = -0.6,
                      width = '50%'),
          plotOutput("ma2_plot", width = 300, height = 200),
          width = 12
        )
        
        
    )
    
    
  )
  
)