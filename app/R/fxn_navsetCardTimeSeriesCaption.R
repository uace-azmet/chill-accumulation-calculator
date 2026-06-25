#' `fxn_navsetCardTimeSeriesCaption.R` - Build caption for time series graph based on user input
#' 
#' @param azmetStation AZMet station selection by user
#' @param inData - Data table [[1]] from `fxn_chillAccumulation.R`
#' @param startDate - Start date of period of interest
#' @param endDate - End date of period of interest
#' @param chillVariable - Chill variable selected by user
#' @return `navsetCardTimeSeriesCaption` Caption for time series graph based on user input


fxn_navsetCardTimeSeriesCaption <- function(azmetStation, inData, startDate, endDate, chillVariable) {
  
  azmetStationStartDate <- 
    dplyr::filter(azmetStationMetadata, meta_station_name == azmetStation) %>% 
    dplyr::pull(start_date)
  
  if (chillVariable == "Chill Portions") {
    chillVariableText <- "chill portions"
  } else if (chillVariable == "Hours below 32 °F") {
    chillVariableText <- "hours below 32 °F"
  } else if (chillVariable == "Hours below 45 °F") {
    chillVariableText <- "hours below 45 °F"
  } else if (chillVariable == "Hours between 32 and 45 °F") {
    chillVariableText <- "hours between 32 and 45 °F"
  } else if (chillVariable == "Hours above 68 °F") {
    chillVariableText <- "hours above 68 °F"
  } else if (chillVariable == "Utah Model") {
    chillVariableText <- "Utah Model chill units"
  }
  
  if (chillVariable == "Chill Portions") {
    variableUnits <- "portions"
  } else if (chillVariable == "Utah Model") {
    variableUnits <- "units"
  } else { # "Hours below 32 °F", "Hours between 32 and 45 °F", "Hours below 45 °F", "Hours above 68 °F"
    variableUnits <- "hours"
  }
  
  if (length(unique(inData$date_year_label)) == 1) {
    standardText <- 
      paste0(
        "Chill accumulation (black line in graph) is based on the sum of daily totals during the period of interest as represented by ", chillVariableText, ". Line breaks denote no data for that day. Data for the ", azmetStation, " station in the new AZMet database currently go back to ", gsub(" 0", " ", format(azmetStationStartDate, "%B %d, %Y")), "."
      )
  } else {
    standardText <- 
      paste0(
        "Chill accumulation for the current year (black line in graph) is based on the sum of daily totals during the period of interest as represented by ", chillVariableText, ". Totals for past years (gray lines in graph) are based on the same start and end month and day, but during those respective years. Line breaks denote no data for that day. Data for the ", azmetStation, " station in the new AZMet database currently go back to ", gsub(" 0", " ", format(azmetStationStartDate, "%B %d, %Y")), "."
      )
  }
  
  # Generate caption text with `chillR` reference
  if (chillVariable == "Chill Portions") {
    standardText <- 
      paste0(
        standardText, " Chill portions are based on calculations in the", "<a href=", "https://eikeluedeling.r-universe.dev/chillR>", htmltools::tags$code("chillR", style = "color: #8B0015;"), "</a>", "R package."
      )
  } else if (chillVariable == "Utah Model") {
    standardText <- 
      paste0(
        standardText, " Utah Model chill units are based on calculations in the", "<a href=", "https://eikeluedeling.r-universe.dev/chillR>", htmltools::tags$code("chillR", style = "color: #8B0015;"), "</a>", "R package. Accumulation values reset daily to 0.0 when negative."
      )
  } else {
    standardText <- standardText
  }
  
  variableKeyText <- 
    paste0(
      "Variable key: <strong>Day<sub>period</sub></strong> day number of the period of interest; <strong>Chill<sub>cumulative</sub></strong> accumulation of daily chill values in ", variableUnits, " during the period of interest as represented by ", chillVariableText
    )
  
  # Account for multi-month absence of YUG data in 2021
  nonOperational <- 0
  
  if (azmetStation == "Yuma N.Gila") {
    while (startDate >= azmetStationStartDate) {
      userDateRange <- lubridate::interval(start = startDate, end = endDate)
      
      if (lubridate::int_overlaps(int1 = yugNodataInterval, int2 = userDateRange) == TRUE) {
        nonOperational <- 1
      }
      
      startDate <- min(seq(startDate, length = 2, by = "-1 year"))
      endDate <- min(seq(endDate, length = 2, by = "-1 year"))
    }
  }
  
  # Generate figure footer based on presence/absence of non-operational dates
  if (azmetStation == "Yuma N.Gila" & nonOperational == 1) {
    navsetCardTimeSeriesCaption <- 
      htmltools::p(
        htmltools::HTML(
          paste(
            standardText,
            "However, we do not show chill accumulation for dates during the period from June 16, 2021 through October 21, 2021, when the ", azmetStation, " station was not in operation.",
            variableKeyText,
            sep = " "
          )
        ),
        
        class = "navset-card-caption"
      )
  } else {
    navsetCardTimeSeriesCaption <- 
      htmltools::p(
        htmltools::HTML(
          paste(
            standardText,
            variableKeyText,
            sep = " "
          )
        ), 
        class = "navset-card-caption"
      )
  }
  
  return(navsetCardTimeSeriesCaption)
}
