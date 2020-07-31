# Source file: server.R
# -----------------------------

library(shiny)
library(magrittr)
source('globals.R')

shinyServer(function(input, output, session) {
  covid <- source_data(dirs$.cache, prefix)  
  covid$data %<>%     # compound assignment
    transformData
  vars <- getvariables(covid$data)
  msg <- character()

  output$myplot <-
    renderPlot({
      cnt <- input$country
      if (!is.character(cnt))
        return()
      max.colours <- 10L
      numCountry <- length(cnt)
      if (numCountry >= max.colours) {
        plot(1:10, type = 'n', axes = FALSE, xlab = '', ylab = '')
        text(5, 5, "This palette does not support more than 9 different colours")
      }
      else {
        vartyp <- if (numCountry == 1L) 
          input$variable.check
        else if (numCountry > 1L && numCountry < max.colours)
          input$variable.radio
        
        plotyp <- input$plotType
        
        if (plotyp == 'cumplot')
          vartyp <- make_cum(vartyp)
        
        create_ggplot(covid, cnt, vartyp, plotyp)
      }
    })
  
  output$message <- renderText(msg)
})