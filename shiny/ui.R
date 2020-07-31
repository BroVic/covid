# Source file: ui.R
# App version: 2.0.1
# -----------------------------

library(shiny)
source('globals.R')

local({
  
  obj <- readCovidObj(dirs$.cache)
  obj$data <- transformData(obj$data)
  countries <- get_country_names(obj)
  vars <- getvariables(obj$data)
  
  # Dimensions
  wd <- list(
    full = 12L,
    right = 4L,
    mid = 4L,
    left = 4L
  )
  
  ui <- fluidPage(
    
    includeCSS("www/ui.css"),
    
    titlePanel("", windowTitle = "COVID-19 Data Visualizer"),
    
    conditionalPanel(
      js_expr_nocountry(),
      mainPanel(plotOutput("myplot"), width = wd$full)
    ),
    
    fluidRow(
      column(width = wd$left,
             inputPanel(
               selectInput(
                 cntryInputId,
                 cntryInputLabel,
                 choices = c("Select one or more." = "", countries),
                 selected = NULL,
                 multiple = TRUE
               )
             )
      ),
      
      column(width = wd$mid,
             conditionalPanel(
               js_expr_nocountry(),
               inputPanel(
                 conditionalPanel(
                   'input.country["length"] == 1',
                   checkboxGroupInput(
                     varInputIdCheck,
                     varInputLabel,
                     c("Cases" = vars$cases, "Deaths" = vars$deaths),
                     selected = c(vars$cases, vars$deaths)
                   )
                 ),
                 conditionalPanel(
                   'input.country["length"] > 1',
                   radioButtons(
                     varInputIdRadio,
                     varInputLabel,
                     c("Cases" = vars$cases, "Deaths" = vars$deaths),
                     selected = vars$cases
                   )
                 )
               )
             )
      ),
      
      column(width = wd$right,
             conditionalPanel(
               js_expr_nocountry(),
               inputPanel(
                 radioButtons(
                   "plotType",
                   "Type of Chart",
                   c(
                     "Time-Series" = 'tsplot',
                     "Cumulative" = "cumplot"
                   ),
                   selected = 'tsplot'
                 )
               )
             )
      )
      
    ),
    
    br(),
    
    fluidRow(column(
      width = wd$left,
      div(
        appversion,
        br(),
        a("Feedback",
          href = "https://github.com/BroVic/covid/issues",
          target = "_blank",
          id = "fdbk-link"),
        id = "endpt"
      )
    ))
  )
  
})
