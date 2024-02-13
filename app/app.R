

# Calculate growing-season heat accumulation relative to cotton development

# Add code for the following
# 
# 'azmet-shiny-template.html': <!-- Google tag (gtag.js) -->
# 'azmet-shiny-template.html': <!-- CSS specific to this AZMet Shiny app -->

# Edit the following [in square brackets]:
# 
# 'azmet-shiny-template.html': <article role="article" about="[/application-areas]" class="node node--type-az-flexible-page node--view-mode-full clearfix">


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
          "Select an AZMet station, specify the chill-accumulation variable, and set dates for the start and end of the period of interest. Then, click or tap 'CALCULATE CHILL ACCUMULATION'."
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
          inputId = "calculateChillAccumulation", 
          label = "CALCULATE CHILL ACCUMULATION",
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
  
  # AZMet heat-unit accumulation data
  dataAZMetDataSumHUs <- eventReactive(input$calculateHeatAccumulation, {
    validate(
      need(expr = input$plantingDate <= input$endDate, message = FALSE)
    )
    
    idCalculatingHeatAccumulation <- showNotification(
      ui = "Calculating heat accumulation . . .", 
      action = NULL, 
      duration = NULL, 
      closeButton = FALSE,
      id = "idCalculatingHeatAccumulation",
      type = "message"
    )
    
    on.exit(removeNotification(id = idCalculatingHeatAccumulation), add = TRUE)
    
    # Calls 'fxnAZMetDataMerge()', which calls 'fxnAZMetDataELT()'
    fxnAZMetDataSumHUs(
      azmetStation = input$azmetStation, 
      startDate = input$plantingDate, 
      endDate = input$endDate)
  })
  
  # Build figure
  figure <- eventReactive(dataAZMetDataSumHUs(), {
    fxnFigure(
      inData = dataAZMetDataSumHUs(), 
      azmetStation = input$azmetStation,
      startDate = input$plantingDate, 
      endDate = input$endDate
    )
  })
  
  # Build figure footer
  figureFooter <- eventReactive(dataAZMetDataSumHUs(), {
    fxnFigureFooter(
      azmetStation = input$azmetStation,
      startDate = input$plantingDate, 
      endDate = input$endDate, 
      timeStep = "Daily"
    )
  })
  
  # Build table footer help text
  figureFooterHelpText <- eventReactive(dataAZMetDataSumHUs(), {
    fxnFigureFooterHelpText()
  })
  
  # Build figure subtitle
  figureSubtitle <- eventReactive(dataAZMetDataSumHUs(), {
    fxnFigureSubtitle(azmetStation = input$azmetStation, startDate = input$plantingDate, endDate = input$endDate)
  })
  
  # Build figure title
  figureTitle <- eventReactive(input$calculateHeatAccumulation, {
    validate(
      need(
        expr = input$plantingDate <= input$endDate, 
        message = "Please select a 'Planting Date' that is earlier than or the same as the 'End Date'."
      ),
      errorClass = "datepicker"
    )
    
    figureTitle <- fxnFigureTitle(inData = dataAZMetDataSumHUs(), endDate = input$endDate)
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
