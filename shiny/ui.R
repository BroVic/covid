# Source file: ui.R
# App version: 2
# -----------------------------

library(shiny)
source('globals.R')

local({
  
  obj <- readCovidObj(dirs$.cache)
  obj$data <- transformData(obj$data)
  countries <- get_country_names(obj)
  
  nn <- colnames(obj$data)
  vars <- structure(as.list(nn), names = nn)
  cases <- vars$cases
  deaths <- vars$deaths
  
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
      sprintf("input.%s != ''", cntryInputId),
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
             )),
      
      column(width = wd$mid,
             inputPanel(
               checkboxGroupInput(
                 varInputId,
                 varInputLabel,
                 c("Cases" = cases, "Deaths" = deaths),
                 selected = cases
               )
             )),
      
      column(width = wd$right,
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
             ))
    ),
    
    br(),
    
    fluidRow(column(
      width = wd$left,
      span(
        a("Feedback",
             href = "https://github.com/BroVic/covid/issues",
             target = "_blank"),
        id = "link_github"
      )
    ))
  )
  
})
