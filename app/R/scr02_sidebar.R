sidebar <- bslib::sidebar(
  width = 300,
  position = "left",
  open = list(desktop = "always", mobile = "always-above"),
  id = "sidebar",
  title = NULL,
  bg = "#FFFFFF",
  fg = "#191919",
  class = NULL,
  max_height_mobile = NULL,
  gap = NULL,
  padding = NULL,
  
  htmltools::p(
    bsicons::bs_icon("sliders"), 
    htmltools::HTML("&nbsp;"), 
    "DATA OPTIONS",
    htmltools::HTML("&nbsp;&nbsp;&nbsp;&nbsp;"),
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
    choices = azmetStationMetadata[order(azmetStationMetadata$meta_station_name), ]$meta_station_name,
    selected = azmetStationMetadata[order(azmetStationMetadata$meta_station_name), ]$meta_station_name[1]
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
    min = Sys.Date() - lubridate::years(1),
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
