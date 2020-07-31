# Source file: ui.R
# -----------------------------

library(shiny)

source('globals.R')

local({
  
  title <- "COVID-19 Data Visualizer"
  obj <- readCovidObj(dirs$.cache)
  obj$data <- transformData(obj$data)
  countries <- get_country_names(obj)
  vars <- getvariables(obj$data)
  
  # Class attributes
  inputcolumns <- 'inputcolumns'
  inputpanels <- 'inputpanels'
  
  # Dimensions
  wd <- list(
    full = 12L,
    right = 4L,
    mid = 4L,
    left = 4L
  )
  
  ui <- fluidPage(
    
    includeCSS("www/ui.css"),
    
    titlePanel(title, windowTitle = title),
    
    div(    # id = mainarea
      
      div(
        conditionalPanel(
          js_expr_nocountry(),
          div(
            mainPanel(
              plotOutput("myplot"),
              width = wd$full)
            ),
          id = 'plotpanel'),
        id = 'plotarea'),
      
      fluidRow(
        ## Country selection
        div(
          column(
            width = wd$left,
            div(
              inputPanel(
                selectizeInput(
                  cntryInputId,
                  label = cntryInputLabel,
                  choices = c("Select one or more." = "", countries),
                  selected = NULL,
                  multiple = TRUE,
                  options = list(
                    closeAfterSelect = I('true'),
                    selectOnTab = I('true'),
                    placeholder = 'Select one or more countries'
                  )
                )
              ),
              class = inputpanels,
              id = 'inputpanel1'
            )
          ),
          class = inputcolumns,
          id = 'inputcolumn1'
        ),
        ## variable selection
        div(
          column(
            width = wd$mid,
            conditionalPanel(
              js_expr_nocountry(),
              div(
                inputPanel(
                  conditionalPanel(
                    'input.country["length"] == 1',
                    checkboxGroupInput(
                      varInputIdCheck,
                      varInputLabel,
                      c("Cases" = vars$cases, "Deaths" = vars$deaths),
                      selected = c(vars$cases, vars$deaths)
                    )
                  ),
                  conditionalPanel(
                    'input.country["length"] > 1',
                    radioButtons(
                      varInputIdRadio,
                      varInputLabel,
                      c("Cases" = vars$cases, "Deaths" = vars$deaths),
                      selected = vars$cases
                    )
                  )
                ),
                class = inputpanels,
                id = 'inputpanel2'
              )
            )
          ),
          class = inputcolumns,
          id = 'inputcolumn2'
        ),    
        ## Chart selection
        div(
          column(
            width = wd$right,
            conditionalPanel(
              js_expr_nocountry(),
              div(
                inputPanel(
                  radioButtons(
                    "plotType",
                    "Type of Chart",
                    c("Time-Series" = 'tsplot', "Cumulative" = "cumplot"),
                    selected = 'tsplot'
                  )
                ),
                class = inputpanels,
                id = 'inputpanel3'
              )
            )
          ),
          class = inputcolumns,
          id = 'inputcolumn3'
        )
      ),    # !fluidRow()
      id = 'mainarea'
    ),
    
    br(),
    
    fluidRow(
      column(
        width = wd$left,
        div(
          appversion,
          br(),
          a(
            "Feedback",
            href = "https://github.com/BroVic/covid/issues",
            target = "_blank",
            id = "fdbk-link"
          ),
          id = "endpt"
        )
      )
    )
  )    # !fluidPage()
  
})    # !local()
