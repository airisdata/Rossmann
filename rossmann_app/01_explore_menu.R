# Module UI
exploreMenu_UI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  menuItem(
    text = "Forecasting basics",
    tabName = "ts_explore",
    icon = icon("dashboard"),
    selected = T,
    menuSubItem("ARMA", "arma"),
    menuSubItem("integration in ARIMA", "arIma"),
    menuSubItem("external featrues", "armax"),
    menuSubItem("vector arima", "xarma"),
    menuSubItem("Threshold models", "threshold_models"),
    menuSubItem("Dynamic linear models", "dlm"),
    menuSubItem("tree-methods", "trees"),
    menuSubItem("neural net", "neural_net"),
    menuSubItem("data: outliers, missing, trend shift", "data_preparation"),
    menuSubItem("BoxCox", "BoxCox"),
    menuSubItem("extrapolation capabilities", "extrapolation"),
    menuSubItem("trend and seasonality", "trend_and_seasonality")
  )
}