library(shiny)
source('globals.R')

shinyServer(function(input, output, session) {
  covid <- decide_and_execute_data_sourcing(dirs$.cache, prefix, today)

  observe({
    varChc <- input$variable
    numCntry <- length(input$country)
    if (varChc == 'both' && numCntry > 1) {
      
      updateSelectInput(
        session,
        cntryInputId,
        cntryInputLabel,
        choices = get_country_names(covid),
        selected = input$country[[1]]
      )
      
      output$message <- renderText(paste("To display more than one country,",
                                         "select only 1 of the variables."))
    }
    else if (varChc != 'both')
      output$message <- renderText("")
  })

  output$myplot <-
    renderPlot(create_ggplot(covid, input$country, input$variable))
})