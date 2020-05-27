library(shiny)
source('globals.R')

shinyServer(function(input, output, session) {
  covid <- decide_and_execute_data_sourcing(dirs$.cache, prefix, today)

  observe({
    varChc <- input$variable
    numCntry <- length(input$country)
    msg <- ""
    if (varChc == 'both') {
      ind <- if (numCntry == 2L) 2L else 1L
      updateSelectInput(
        session,
        cntryInputId,
        cntryInputLabel,
        choices = get_country_names(covid),
        selected = input$country[[ind]]
      )
      msg <- paste("To display more than one country,",
                   "select only 1 of the variables.")
    }
    output$message <- renderText(msg)
  })

  output$myplot <-
    renderPlot({
      if (length(input$country) >= 10L) {
        plot(1:10, type = 'n', axes = FALSE, xlab = '', ylab = '')
        text(5, 5, "This palette does not support more than 9 different colours")
      }
      else
        create_ggplot(covid, input$country, input$variable)
    })
})