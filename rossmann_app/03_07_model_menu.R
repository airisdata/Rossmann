menuItem_model <- menuItem(
  "Model",
  icon = icon("th"),
  tabName = "model",
  menuSubItem("Make model", "model_choose"),
  conditionalPanel(
    "input.sidebarmenu === 'model_choose'",
    
    numericInput("max_pr_hor", "Prediction horizon", value = 48),
    
    
    checkboxGroupInput(
      "models_to_train",
      "Choose models to be trained",
      choices = c(
        "ets",
        "arima",
        "arima_xreg",
        "nnetar",
        "nnetar_xreg",
        "setar",
        "tvar",
        "tbats",
        "prophet_xreg_check",
        "xgboost_xreg",
        "ensemble"
      )
    )
    
  ),
  
  menuSubItem("tvar parameters", "tvar_params"),
  conditionalPanel(
    "input.sidebarmenu === 'tvar_params'",
    numericInput("tvar_lags", "number of lags for tvar", 5),
    numericInput("tvar_seed", "random seed to choose other stores", 1),
    numericInput("tvar_numOther", "number of other stores", 5)
    
  ),
  
  menuSubItem("Play with Data", "play_data", icon = icon("dashboard")),
  conditionalPanel(
    "input.sidebarmenu === 'play_data'",
    checkboxInput("remove_sunday", "Remove sundays from data?",
                  value = T),
    numericInput("seasonality", "Seasonality in days (6 if sundays removed)", 6),
    checkboxInput(
      "fill_na",
      "Fill numbers on holidays by previous value? \n
            (Only do together with Sundays' data removal)",
      value = T
    ),
    
    checkboxInput(
      "put_in_outlier",
      "Put ouliers in the middle of train series",
      value = F
    ),
    numericInput(
      "outlier_multiplier",
      "To create outlier take mean value of ts and add sd multiplied by ",
      10
    ),
    checkboxInput(
      "clean_outliers",
      "Clean outliers with tsclean? (this may catch some originally present outliers as well)",
      value = F
    )
    
  ),
  actionButton("update_model_but", "Update model")
)
