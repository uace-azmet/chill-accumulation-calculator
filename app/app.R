# Calculate cumulative values of chill variables by station and date range


# UI --------------------


ui <- htmltools::htmlTemplate(
  
  filename = "azmet-shiny-template.html",
  
  pageChillAccumulationCalculator = 
    bslib::page(
      title = NULL,
      theme = theme, # `scr##_theme.R`
      
      bslib::layout_sidebar(
        sidebar = sidebar, # `scr##_sidebar.R`
        # shiny::htmlOutput(outputId = "navsetCardTabTitle"),
        # shiny::htmlOutput(outputId = "navsetCardTabSummary"),
        # shiny::uiOutput(outputId = "navsetCardTab")
        
        # shiny::htmlOutput(outputId = "figureTitle"),
        # shiny::htmlOutput(outputId = "figureSummary"),
        # plotly::plotlyOutput(outputId = "figure"),
        # shiny::htmlOutput(outputId = "figureCaption")
      ) |>
        htmltools::tagAppendAttributes(
          #https://getbootstrap.com/docs/5.0/utilities/api/
          class = "border-0 rounded-0 shadow-none"
        ),
      
      # shiny::htmlOutput(outputId = "downloadButtonsDiv"), # Common, regardless of card tab
      shiny::htmlOutput(outputId = "pageBottomText")
    )
  )


# Server --------------------


server <- 
  function(input, output, session) {
    
    shinyjs::useShinyjs(html = TRUE)
    # shinyjs::hideElement(id = "downloadButtonsDiv")
    # shinyjs::hideElement(id = "navsetCardTab")
    
    
    # Observables -----
    
    # shiny::observeEvent(chillTotal(), {
      # shinyjs::showElement(id = "downloadButtonsDiv")
      # shinyjs::showElement(id = "navsetCardTab")
      # showNavsetCardTab(TRUE)
      # showPageBottomText(TRUE)
    # })
    
    # To update available dates based on selected station
    shiny::observeEvent(input$azmetStation, {
      stationStartDate <-
        dplyr::filter(azmetStationMetadata, meta_station_name == input$azmetStation) %>% 
        dplyr::pull(start_date)
      
      stationStartDateMinimum <- stationStartDate
      stationEndDateMinimum <- stationStartDate
      
      # if (stationStartDate > Sys.Date() - lubridate::years(1)) {
      #   stationStartDateMinimum <- stationStartDate
      #   stationEndDateMinimum <- stationStartDate
      # } else {
      #   stationStartDateMinimum <- Sys.Date() - lubridate::years(1)
      #   stationEndDateMinimum <- Sys.Date() - lubridate::years(1)
      # }
  
      if (stationStartDate > input$startDate) {
        stationStartDateSelected <- stationStartDate
      } else {
        stationStartDateSelected <- input$startDate
      }
  
      if (stationStartDate > input$endDate) {
        stationEndDateSelected <- stationStartDate
      } else {
        stationEndDateSelected <- input$endDate
      }
  
      shiny::updateDateInput(
        inputId = "startDate",
        label = "Start Date",
        value = stationStartDateSelected,
        min = stationStartDateMinimum,
        max = Sys.Date() - 1
      )
  
      shiny::updateDateInput(
        inputId = "endDate",
        label = "End Date",
        value = stationEndDateSelected,
        min = stationEndDateMinimum,
        max = Sys.Date() - 1
      )
    })
    
    # Catch input errors before data download, show error modal
    shiny::observeEvent(input$calculateTotal, {
      if (input$startDate > input$endDate) {
        shiny::showModal(datepickerErrorModal) # `scr##_datepickerErrorModal.R`
      }

      if (
        input$azmetStation == "Yuma N.Gila" &
        lubridate::int_overlaps(
          int1 = yugNodataInterval,
          int2 = lubridate::interval(input$startDate, input$endDate)
        ) == TRUE
      ) {
        shiny::showModal(datepickerYumaNGilaErrorModal) # `scr##_datepickerYumaNGilaErrorModal.R`
      }
    })
    
    # To update icon in navsetCardTab title
    # shiny::observeEvent(input$navsetCardTab, {
    #   if (input$navsetCardTab == "barChart") {
    #     navsetCardTabTitleIcon("bar-chart-fill")
    #   } else if (input$navsetCardTab == "table") {
    #     navsetCardTabTitleIcon("table")
    #   } else if (input$navsetCardTab == "timeSeries") {
    #     navsetCardTabTitleIcon("graph-up")
    #   }
    # })
    
    
    # Reactives -----
    
    # figure <- shiny::eventReactive(seasonalTotals(), {
    #   fxn_figure(
    #     inData = seasonalTotals(),
    #     azmetStation = input$azmetStation,
    #     chillVariable = input$chillVariable
    #   )
    # })
    # 
    # figureCaption <- shiny::eventReactive(seasonalTotals(), {
    #   fxn_figureCaption(
    #     azmetStation = input$azmetStation,
    #     inData = seasonalTotals(),
    #     startDate = input$startDate,
    #     endDate = input$endDate,
    #     chillVariable = input$chillVariable
    #   )
    # })
    # 
    # figureSummary <- shiny::eventReactive(seasonalTotals(), {
    #   fxn_figureSummary(
    #     azmetStation = input$azmetStation,
    #     inData = seasonalTotals(),
    #     startDate = input$startDate,
    #     endDate = input$endDate,
    #     chillVariable = input$chillVariable
    #   )
    # })
    # 
    # figureTitle <- shiny::eventReactive(seasonalTotals(), {
    #   fxn_figureTitle(
    #     azmetStation = input$azmetStation
    #   )
    # })
    # 
    # pageBottomText <- shiny::eventReactive(seasonalTotals(), {
    #   fxn_pageBottomText()
    # })

    seasonalTotals <-
      shiny::eventReactive(input$calculateTotal, {
        
        # Catch input errors before data download
        shiny::validate(
          shiny::need(
            expr = input$startDate <= input$endDate,
            message = FALSE # Failing validation test
          ),
          
          shiny::need(
            expr =
              !(input$azmetStation == "Yuma N.Gila" &
                  lubridate::int_overlaps(
                    int1 = yugNodataInterval,
                    int2 = lubridate::interval(input$startDate, input$endDate)
                  )
              ),
            message = FALSE # Failing validation test
          )
        )

        idCalculateTotal <- shiny::showNotification(
          ui = "Calculating chill accumulation . . .",
          action = NULL,
          duration = NULL,
          closeButton = FALSE,
          id = "idCalculateTotal",
          type = "message"
        )

        on.exit(
          shiny::removeNotification(id = idCalculateTotal),
          add = TRUE
        )
        
        fxn_seasonalTotals(
          azmetStation = input$azmetStation,
          startDate = input$startDate,
          endDate = input$endDate,
          chillVariable = input$chillVariable
        )
      })
  
  
  # Outputs -----
  
    # output$downloadButtonsDiv <- 
    #   shiny::renderUI({
    #     fxn_downloadButtonsDiv()
    #   })
    # 
    # output$downloadCSV <- 
    #   shiny::downloadHandler(
    #     filename = function() {"AZMet-total-evaporation-calculator.csv"},
    #     content = function(file) {
    #       vroom::vroom_write(x = totalEvapotranspiration()[[1]], file = file, delim = ",")
    #     }
    #   )
    # 
    # output$downloadTSV <- 
    #   shiny::downloadHandler(
    #     filename = function() {"AZMet-total-evaporation-calculator.tsv"},
    #     content = function(file) {
    #       vroom::vroom_write(x = totalEvapotranspiration()[[1]], file = file, delim = "\t")
    #     }
    #   )
    # 
    # output$navsetCardBarChart <- 
    #   plotly::renderPlotly({
    #     navsetCardBarChart()
    #   })
    # 
    # output$navsetCardBarChartCaption <- 
    #   shiny::renderUI({
    #     navsetCardBarChartCaption()
    #   })
    # 
    # output$navsetCardTab <- 
    #   shiny::renderUI({
    #     shiny::req(showNavsetCardTab())
    #     navsetCardTab # `scr##_navsetCardTab.R`
    #   })
    # 
    # output$navsetCardTable <- 
    #   reactable::renderReactable({
    #     navsetCardTable()
    #   })
    # 
    # output$navsetCardTableCaption <- 
    #   shiny::renderUI({
    #     navsetCardTableCaption()
    #   })
    # 
    # output$navsetCardTabSummary <- 
    #   shiny::renderUI({
    #     navsetCardTabSummary()
    #   })
    # 
    # output$navsetCardTabTitle <- 
    #   shiny::renderUI({
    #     navsetCardTabTitle()
    #   })
    # 
    # output$navsetCardTabTooltip <- 
    #   shiny::renderUI({
    #     navsetCardTabTooltipText()
    #   })
    # 
    # output$navsetCardTimeSeries <- 
    #   plotly::renderPlotly({
    #     navsetCardTimeSeries()
    #   })
    # 
    # output$navsetCardTimeSeriesCaption <- 
    #   shiny::renderUI({
    #     navsetCardTimeSeriesCaption()
    #   })

    # output$pageBottomText <-
    #   shiny::renderUI({
    #     shiny::req(showPageBottomText())
    #     pageBottomText()
    #   })
}


# Run --------------------


shiny::shinyApp(ui = ui, server = server)
