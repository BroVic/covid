lapply(c("shiny", "shinythemes"), library, character.only = TRUE)
source('globals.R')

local({
  obj <- readCovidObj(dirs$.cache, prefix)
  countries <- get_country_names(obj)
  startCntry <- "Nigeria"
  
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
    theme = "Flatly",
    
    mainPanel(plotOutput("myplot"), width = wd$full),
    
    fluidRow(
      column(width = wd$left,
             inputPanel(
               selectInput(cntryInputId,
                           cntryInputLabel,
                           countries,
                           startCntry,
                           TRUE),
               
               "Select one or more."
             )),
      
      column(width = wd$mid,
             inputPanel(
               checkboxGroupInput(
                 varInputId,
                 varInputLabel,
                 c("Cases" = cases,
                   "Deaths" = deaths),
                 selected = c(cases, deaths),
                 inline = TRUE
               )
             )),
      
      column(width = wd$right,
             
             textOutput("message"))
    )
    
  ))
})
