library(shiny)
library(here)
source('globals.R')

data <- readData('.', prefix)$data
countries <- unique(data$countriesAndTerritories)


shinyUI(
  fluidPage(
    selectInput('country', "Country", countries, selected = 'Nigeria'),
    
    mainPanel(plotOutput("myplot"), width = 12)
  )
)
