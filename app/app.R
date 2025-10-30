# Calculate cumulative values of chill variables by station and date range


# UI --------------------


ui <- htmltools::htmlTemplate(
  
  filename = "azmet-shiny-template.html",
  
  pageFluid = bslib::page_fluid(
    title = NULL,
    theme = theme, # `scr##_theme.R`
    
    bslib::layout_sidebar(
      sidebar = sidebar, # `scr##_sidebar.R`
      
      # shiny::htmlOutput(outputId = "figureTitle"),
      # shiny::htmlOutput(outputId = "figureSummary"),
      # plotly::plotlyOutput(outputId = "figure"),
      # shiny::htmlOutput(outputId = "figureFooter")
    ) |>
      htmltools::tagAppendAttributes(
        #https://getbootstrap.com/docs/5.0/utilities/api/
        class = "border-0 rounded-0 shadow-none"
      ),
    
    # shiny::htmlOutput(outputId = "pageBottomText")
  )
)


# Server --------------------


server <- function(input, output, session) {
  # shinyjs::useShinyjs(html = TRUE) ###commented out
  # shinyjs::hideElement("pageBottomText") ###commented out
  
  
  # Observables -----
  
  # shiny::observeEvent(input$azmetStation, {
  #   stationStartDate <-
  #     dplyr::filter(
  #       azmetStationMetadata,
  #       meta_station_name == input$azmetStation
  #     )$start_date
  #   
  #   if (stationStartDate > Sys.Date() - lubridate::years(1)) {
  #     stationStartDateMinimum <- stationStartDate
  #     stationEndDateMinimum <- stationStartDate
  #   } else {
  #     stationStartDateMinimum <- Sys.Date() - lubridate::years(1)
  #     stationEndDateMinimum <- Sys.Date() - lubridate::years(1)
  #   }
  #   
  #   if (stationStartDate > input$startDate) {
  #     stationStartDateSelected <- stationStartDate
  #   } else {
  #     stationStartDateSelected <- input$startDate
  #   }
  #   
  #   if (stationStartDate > input$endDate) {
  #     stationEndDateSelected <- stationStartDate
  #   } else {
  #     stationEndDateSelected <- input$endDate
  #   }
  #   
  #   shiny::updateDateInput(
  #     inputId = "startDate",
  #     label = "Start Date",
  #     value = stationStartDateSelected,
  #     min = stationStartDateMinimum,
  #     max = Sys.Date() - 1
  #   )
  #   
  #   shiny::updateDateInput(
  #     inputId = "endDate",
  #     label = "End Date",
  #     value = stationEndDateSelected,
  #     min = stationEndDateMinimum,
  #     max = Sys.Date() - 1
  #   )
  # })
  
  # shiny::observeEvent(input$calculateTotal, {
  #   if (input$startDate > input$endDate) {
  #     shiny::showModal(datepickerErrorModal) # `scr##_datepickerErrorModal.R`
  #   }
  # })
  
  # shiny::observeEvent(seasonalTotals(), { ###commented out
  #   shinyjs::showElement("pageBottomText")
  # })
  
  
  # Reactives -----
  
  # figure <- shiny::eventReactive(seasonalTotals(), {
  #   fxn_figure(
  #     inData = seasonalTotals(),
  #     azmetStation = input$azmetStation
  #   )
  # })
  
  # figureFooter <- shiny::eventReactive(seasonalTotals(), {
  #   fxn_figureFooter(
  #     azmetStation = input$azmetStation,
  #     startDate = input$startDate,
  #     endDate = input$endDate
  #   )
  # })
  
  # figureSummary <- shiny::eventReactive(seasonalTotals(), {
  #   fxn_figureSummary(
  #     azmetStation = input$azmetStation,
  #     inData = seasonalTotals(),
  #     startDate = input$startDate,
  #     endDate = input$endDate
  #   )
  # })
  
  # figureTitle <- shiny::eventReactive(seasonalTotals(), {
  #   fxn_figureTitle(
  #     azmetStation = input$azmetStation
  #   )
  # })
  
  # pageBottomText <- shiny::eventReactive({#seasonalTotals(), {
  #   fxn_pageBottomText(
  #     startDate = input$startDate,
  #     endDate = input$endDate,
  #     chillVariable = input$chillVariable
  #   )
  # })
  
  # seasonalTotals <- shiny::eventReactive(input$calculateTotal, {
  #   shiny::validate(
  #     shiny::need(
  #       expr = input$startDate <= input$endDate,
  #       message = FALSE
  #     )
  #   )
  #   
  #   idCalculateTotal <- shiny::showNotification(
  #     ui = "Calculating total evapotranspiration . . .",
  #     action = NULL,
  #     duration = NULL,
  #     closeButton = FALSE,
  #     id = "idCalculateTotal",
  #     type = "message"
  #   )
  #   
  #   on.exit(
  #     shiny::removeNotification(id = idCalculateTotal),
  #     add = TRUE
  #   )
  #   
  #   fxn_seasonalTotals( # calls `fxn_dailyData.R` and `fxn_etTotal.R`
  #     azmetStation = input$azmetStation,
  #     startDate = input$startDate,
  #     endDate = input$endDate,
  #     etEquation = input$etEquation
  #   )
  # })
  
  
  # Outputs -----
  
  # output$figure <- plotly::renderPlotly({
  #   figure()
  # })
  
  # output$figureFooter <- shiny::renderUI({
  #   figureFooter()
  # })
  
  # output$figureSummary <- shiny::renderUI({
  #   figureSummary()
  # })
  
  # output$figureTitle <- shiny::renderUI({
  #   figureTitle()
  # })
  
  # output$pageBottomText <- shiny::renderUI({
  #   pageBottomText()
  # })
}


# Run --------------------


shiny::shinyApp(ui = ui, server = server)
