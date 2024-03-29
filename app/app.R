

# Calculate cumulative values of chill variables by station and date range

# Libraries
library(azmetr)
library(dplyr)
library(ggplot2)
library(htmltools)
library(lubridate)
library(shiny)
library(vroom)

# Functions
#source("./R/fxnABC.R", local = TRUE)

# Scripts
#source("./R/scr##DEF.R", local = TRUE)


# UI --------------------

ui <- htmltools::htmlTemplate(
  
  "azmet-shiny-template.html",
  
  sidebarLayout = sidebarLayout(
    position = "left",
    
    sidebarPanel(
      id = "sidebarPanel",
      width = 4,
      
      verticalLayout(
        helpText(em(
          "Select an AZMet station, specify the chill variable, and set dates for the start and end of the period of interest. Then, click or tap 'CALCULATE CUMULATIVE VALUES'."
        )),
        
        br(),
        selectInput(
          inputId = "azmetStation", 
          label = "AZMet Station",
          choices = stationNames[order(stationNames$stationName), ]$stationName,
          selected = "Aguila"
        ),
        
        selectInput(
          inputId = "chillVariable",
          label = "Chill Variable",
          choices = chillVariables,
          selected = "Hours below 32 °F"
        ),
        
        dateInput(
          inputId = "startDate",
          label = "Start Date",
          value = initialStartDate,
          min = initialStartDate,
          max = Sys.Date() - 1,
          format = "MM d, yyyy",
          startview = "month",
          weekstart = 0, # Sunday
          width = "100%",
          autoclose = TRUE
        ),
        
        dateInput(
          inputId = "endDate",
          label = "End Date",
          value = initialEndDate,
          min = initialStartDate,
          max = initialEndDate,
          format = "MM d, yyyy",
          startview = "month",
          weekstart = 0, # Sunday
          width = "100%",
          autoclose = TRUE
        ),
        
        br(),
        actionButton(
          inputId = "calculateCumulativeValues", 
          label = "CALCULATE CUMULATIVE VALUES",
          class = "btn btn-block btn-blue"
        )
      )
    ), # sidebarPanel()
    
    mainPanel(
      id = "mainPanel",
      width = 8,
      
      fluidRow(
        column(width = 11, align = "left", offset = 1, htmlOutput(outputId = "figureTitle"))
      ), 
      
      fluidRow(
        column(width = 11, align = "left", offset = 1, htmlOutput(outputId = "figureSubtitle"))
      ),
      
      fluidRow(
        column(width = 11, align = "left", offset = 1, plotOutput(outputId = "figure"))
      ), 
      
      br(), br(),
      fluidRow(
        column(width = 11, align = "left", offset = 1, htmlOutput(outputId = "figureFooterHelpText"))
      ),
      
      fluidRow(
        column(width = 11, align = "left", offset = 1, htmlOutput(outputId = "figureFooter"))
      ),
      br()
    ) # mainPanel()
  ) # sidebarLayout()
) # htmltools::htmlTemplate()


# Server --------------------

server <- function(input, output, session) {
  
  # Reactive events -----
  
  # AZMet chill accumulation data
  dataAZMetDataMerge <- eventReactive(input$calculateCumulativeValues, {
    validate(
      need(expr = input$startDate <= input$endDate, message = FALSE)
    )
    
    idCalculatingCumulativeValues <- showNotification(
      ui = "Calculating cumulative values . . .", 
      action = NULL, 
      duration = NULL, 
      closeButton = FALSE,
      id = "idCalculatingCumulativeValues",
      type = "message"
    )
    
    on.exit(removeNotification(id = idCalculatingCumulativeValues), add = TRUE)
    
    # Calls 'fxnAZMetDataELT()' and 'fxnAZMetDataSumChill()'
    fxnAZMetDataMerge(
      azmetStation = input$azmetStation, 
      startDate = input$startDate, 
      endDate = input$endDate,
      chillVariable = input$chillVariable
    )
  })
  
  # Build figure
  figure <- eventReactive(dataAZMetDataMerge(), {
    fxnFigure(
      inData = dataAZMetDataMerge(), 
      azmetStation = input$azmetStation,
      startDate = input$startDate, 
      endDate = input$endDate,
      chillVariable = input$chillVariable
    )
  })
  
  # Build figure footer
  figureFooter <- eventReactive(dataAZMetDataMerge(), {
    fxnFigureFooter(
      azmetStation = input$azmetStation,
      startDate = input$startDate, 
      endDate = input$endDate,
      chillVariable = input$chillVariable, 
      timeStep = "Daily"
    )
  })
  
  # Build figure footer help text
  figureFooterHelpText <- eventReactive(dataAZMetDataMerge(), {
    fxnFigureFooterHelpText()
  })
  
  # Build figure subtitle
  figureSubtitle <- eventReactive(dataAZMetDataMerge(), {
    fxnFigureSubtitle(azmetStation = input$azmetStation, startDate = input$startDate, endDate = input$endDate)
  })
  
  # Build figure title
  figureTitle <- eventReactive(input$calculateCumulativeValues, {
    validate(
      need(
        expr = input$startDate <= input$endDate, 
        message = "Please select a 'Start Date' that is earlier than or the same as the 'End Date'."
      ),
      errorClass = "datepicker"
    )
    
    fxnFigureTitle(
      inData = dataAZMetDataMerge(), 
      endDate = input$endDate,
      chillVariable = input$chillVariable)
  })
  
  # Outputs -----
  
  output$figure <- renderPlot({
    figure()
  }, res = 96)
  
  output$figureFooter <- renderUI({
    figureFooter()
  })
  
  output$figureFooterHelpText <- renderUI({
    figureFooterHelpText()
  })
  
  output$figureSubtitle <- renderUI({
    figureSubtitle()
  })
  
  output$figureTitle <- renderUI({
    figureTitle()
  })
}

# Run --------------------

shinyApp(ui = ui, server = server)
