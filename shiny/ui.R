library(shiny)
source('globals.R')

data <- readCovidObj(dirs$.cache, prefix)$data
countries <- unique(data$countriesAndTerritories)

shinyUI(
  fluidPage(
    selectInput('country', "Country", countries, selected = 'Nigeria'),
    
    mainPanel(plotOutput("myplot"), width = 12)
  )
)
