#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(tidyverse)
library(ggplot2)
library(lubridate)
library(plotly)
library(forecast)
library(dygraphs)
library(xts)
library(DT)
library(tsDyn)
library(caret)
#lag <- stats::lag  # to overcome lag masking by dplyr
library(prophet)

### xgboost
# https://github.com/ellisp/forecastxgb-r-package
# devtools::install_github("ellisp/forecastxgb-r-package/pkg")
library(forecastxgb)

## max pr hor
#max_pr_hor <- 48 

sys_vars <- readLines("/etc/bash.bashrc")
rossmann_data_path <- sys_vars[str_detect(sys_vars, "ROSS_DATA_PATH")] %>%
  str_extract('\\".+\\"') %>%
  str_remove_all('\\"')


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  models_info <- reactiveValues()
  
  source("01_explore.R", local = T)  
  
  source("02_read_split_data.R", local = T)  

  source("03_make_models.R", local = T)
  
  source("03_02_triger_model_computation.R", local = T)
  
  source("03_05_metric.R", local = T)
  
  source("04_display_elements.R", local = T)
  
})
