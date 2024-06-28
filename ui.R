library(shiny)
library(readxl)
library(shinyjs)
library(SMRD)
library(DT)
library(gt)
library(gtExtras)
library(tidyverse)
library(scales)
library(glue)
library(dplyr)
require(plyr)

ui <- fluidPage(
  tags$style(HTML("
    .gt_table {
      font-size: 18px;
      line-height: 1.5;
      width: 100% !important;
    }
    .pagination-controls {
      text-align: center;
      margin-top: 20px;
    }
  ")),
  
  fluidRow(
    titlePanel(h1("Gráficos de probabilidad", align = "center",
                  style = "font-family: Arial Black;
                color: #15297c;
                border: 2px solid LightGray;
                border-radius: 5px;
                padding: 20px"))
  ),
  
  column(12,
         tabsetPanel(
           type = "pills",
           tabPanel(
             title = "Visualización datos", 
             sidebarLayout(
               sidebarPanel(
                 width = 3,
                 radioButtons("data_input", "Fuente de los datos:",
                              choices = list(
                                "Utilizar una base de datos de R" = "predefined",
                                "Cargar tu propia base de datos" = "upload"
                              )),
                 uiOutput("data_selection_ui"),
                 uiOutput("col_response_ui"),
                 style = "margin-top: 20px;"
               ),
               mainPanel(
                 width = 9,
                 textInput("search_input", "Búsqueda:", placeholder = "Ingresa el valor que deseas encontrar..."),
                 gt_output("table_output"),
                 div(class = "pagination-controls", uiOutput("pagination_ui"))
               )
             )
           ),
           tabPanel(
             title = "Gráficos de probabilidad"),
           tabPanel(
             title = "Teoría")
         )
  )
)