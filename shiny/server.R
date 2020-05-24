library(shiny)
source('globals.R')

shinyServer(function(input, output) {
  covid <- decide_and_execute_data_sourcing(dirs$.cache, prefix, today)
  
  output$myplot <-
    renderPlot(create_ggplot(covid, input$country))
})