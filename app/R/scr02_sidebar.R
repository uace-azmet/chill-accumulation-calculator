sidebar <- 
  bslib::sidebar(
    width = 300,
    position = "left",
    open = list(desktop = "open", mobile = "always-above"),
    id = "sidebar",
    title = NULL,
    bg = "#FFFFFF",
    fg = "#191919",
    class = NULL,
    max_height_mobile = NULL,
    gap = NULL,
    padding = NULL,
    
    
    # Visible elements -----
    
    htmltools::p(
      bsicons::bs_icon("sliders", class = "bolder-icon"), 
      htmltools::HTML("&nbsp;<strong>DATA OPTIONS</strong>&nbsp;"),
      bslib::tooltip(
        bsicons::bs_icon("info-circle"),
        "Select an AZMet station, chill variable, and dates for the start and end of the period of interest. Then, click or tap 'CALCULATE TOTAL'.",
        id = "infoDataOptions",
        placement = "right"
      ),
      
      class = "data-options-title"
    ),
    
    shiny::selectInput(
      inputId = "azmetStation", 
      label = "AZMet Station",
      choices = sort(azmetStationMetadata$meta_station_name),
      selected = initialStation
    ),
    
    shiny::selectInput(
      inputId = "chillVariable",
      label = "Chill Variable",
      choices = chillVariables,
      selected = chillVariables[1]
    ),
    
    shiny::dateInput(
      inputId = "startDate",
      label = "Start Date",
      value = initialStartDate,
      min = initialStartDateMinimum,
      max = Sys.Date() - 1,
      format = "MM d, yyyy",
      startview = "month",
      weekstart = 0, # Sunday
      width = "100%",
      autoclose = TRUE
    ),
    
    shiny::dateInput(
      inputId = "endDate",
      label = "End Date",
      value = Sys.Date() - 1,
      min = initialStartDateMinimum,
      max = Sys.Date() - 1,
      format = "MM d, yyyy",
      startview = "month",
      weekstart = 0, # Sunday
      width = "100%",
      autoclose = TRUE
    ),
    
    shiny::actionButton(
      inputId = "calculateTotal", 
      label = "CALCULATE TOTAL",
      class = "btn btn-block btn-blue"
    )    
  ) # bslib::sidebar()
