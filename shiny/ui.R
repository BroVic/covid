library(shiny)
source('globals.R')

local({
  
  obj <- readCovidObj(dirs$.cache, prefix)
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
    
    mainPanel(plotOutput("myplot"), width = wd$full),
    
    fluidRow(
      column(width = wd$left,
             inputPanel(
               selectInput(
                 cntryInputId,
                 cntryInputLabel,
                 choices = countries,
                 selected = "Nigeria",
                 multiple = TRUE
               ),
               
               em("Select one or more.")
             )),
      
      column(width = wd$mid,
             inputPanel(
               checkboxGroupInput(
                 varInputId,
                 varInputLabel,
                 c("Cases" = cases, "Deaths" = deaths),
                 selected = cases
               ),
               
               em(textOutput("message"))
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
      
      actionLink("openFdbk", "Feedback"),
      
      conditionalPanel(
        "var fdbk = false; if (!input.openFdbk) fdbk = toggleFdbk(fdbk);",
        div(
          my_infobox_panel(),
          class = 'infobox'
        )
      )
    ))
  )
  
})
