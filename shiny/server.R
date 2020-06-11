# Source file: server.R
# App version: 2
# -----------------------------

library(shiny)
library(magrittr)
source('globals.R')

shinyServer(function(input, output, session) {
  covid <- source_data(dirs$.cache, prefix)  
  covid$data %<>%     # compound assignment
    transformData
  msg <- character()
  
  observe({
    if (length(input$variable) == 2L) {
      ind <- if (length(input$country) >= 2L) 2L else 1L
      updateSelectInput(
        session,
        cntryInputId,
        cntryInputLabel,
        choices = get_country_names(covid),
        selected = input$country[[ind]]
      )
    }
  })

  output$myplot <-
    renderPlot({
      cnt <- input$country
      if (!is.character(cnt))
        return()
      if (length(cnt) >= 10L) {
        plot(1:10, type = 'n', axes = FALSE, xlab = '', ylab = '')
        text(5, 5, "This palette does not support more than 9 different colours")
      }
      else {
        vartyp <- input$variable
        plotyp <- input$plotType
        if (plotyp == 'cumplot')
          vartyp <- make_cum(vartyp)
        create_ggplot(covid, cnt, vartyp, plotyp)
      }
    })
  
  output$message <- renderText(msg)
})