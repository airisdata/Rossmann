#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(DT)
library(dygraphs)
library(dplyr)


source("01_explore_menu.R", local = T)
source("01_explore_ui.R", local = T)
source("03_07_model_menu.R", local = T)


ui <- dashboardPage(
  dashboardHeader(title = "TS models for Rossman data"),
  dashboardSidebar(
    sidebarMenu(
      id = "sidebarmenu",
      
      #menuItem_explore, # include tabItem from file
      exploreMenu_UI("explore_menu"),
      
      menuItem(
        "Choose store",
        tabName = "choose_store",
        icon = icon("dashboard")
      ),
      conditionalPanel(
        "input.sidebarmenu === 'choose_store'",
        numericInput("store", "store", 1)
      ),
      
      menuItem_model  # include tabItem from file
      
      
    )
    
    
    
  ),
  dashboardBody(tabItems(
    
    tabItem_explore, # include tabItem from file
    
    tabItem(tabName = "choose_store",
            box(
              DT::dataTableOutput("df_oneStore_out"),
              width = 12
            )),
    
    tabItem(
      tabName = "model_choose",
      box(title = "Plot of the chosen ts",
          dygraphOutput("dy_plot"),
          width = 12),
      fluidRow(
        box(
          uiOutput("model_display_ui"),
          verbatimTextOutput("modInfo_out")
        ),
        
        box(title = "Performance on the test part of ts",
            DT::dataTableOutput("df_metric_out"))
      )
    )
  ))
)
