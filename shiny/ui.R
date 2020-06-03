library(shiny)
source('globals.R')

# txt <- 
#   c(
#     "This is a minimalist application created primarily to lend credence",
#     a("this script", 
#       href = "https://gist.github.com/BroVic/32eca9d3ae3334bfe573e4aecb35c522",
#       target = '_blank'
#     ),
#     ""
#   )


local({
  
  obj <- readCovidObj(dirs$.cache, prefix)
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
             ))
    ),
    
    br(),
    
    fluidRow(
      column(
        width = wd$left,
        
        actionLink("openFdbk", "Feedback"),
        
        conditionalPanel(
          "input.openFdbk == true && input.closeFdbk != true",
          inputPanel(
            span(
              "This is a minimalist application created primarily to lend credence to",
              
              a('this script',
                href = "https://gist.github.com/BroVic/32eca9d3ae3334bfe573e4aecb35c522",
                target = '_blank'),
              
              ", which contains the initial work carried out to examine COVID-19 cases and/or deaths"
            ), 
            
            span(
              "If you would like us to add a feature or you find a bug, kindly post a message ",
              a('here',
                href = 'https://github.com/BroVic/covid/issues/new',
                target = "_blank")
            ),
            
            span(
              "For the source code, visit",
              strong(a("this page",
                       href = 'https://github.com/BroVic/covid/shiny',
                       target = "_blank"))
            ),
            
            br(),
            
            actionLink("closeFdbk", "Close")
          )
        )
      )
    )
  ))
})
