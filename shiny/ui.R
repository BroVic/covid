library(shiny)
source('globals.R')

local({
  obj <- readCovidObj(dirs$.cache, prefix)
  countries <- get_country_names(obj)
  names(countries) <- chartr('_', " ", countries)
    
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
  
  
  shinyUI(fluidPage(

    mainPanel(plotOutput("myplot"), width = wd$full),
    
    fluidRow(
      column(width = wd$left,
             inputPanel(
               selectInput(cntryInputId,
                           cntryInputLabel,
                           choices = countries,
                           selected = "Nigeria",
                           multiple = TRUE),
               
               em("Select one or more.")
             )),
      
      column(width = wd$mid,
             inputPanel(
               radioButtons(
                 varInputId,
                 varInputLabel,
                 c("Cases only" = cases,
                   "Deaths only" = deaths,
                   "Cases & Deaths" = 'both'),
                 selected = 'both'
               ),
               
               em(textOutput("message"))
             )),
      
      column(width = wd$right)
    )
    
  ))
})
