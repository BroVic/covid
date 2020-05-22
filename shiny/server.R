library(shiny)
source('globals.R')

shinyServer(function(input, output) {
  covid <- readData('.', prefix)
  # data <- covid$data
  # 
  # appData <- reactive({
  #   transformData(data, input$country)
  # })
  output$myplot <-
    renderPlot(create_ggplot(covid, input$country))
})