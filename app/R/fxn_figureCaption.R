#' `fxn_figureCaption.R` - Build caption for figure based on user input
#' 
#' @param azmetStation AZMet station selection by user
#' @param startDate - Start date of period of interest
#' @param endDate - End date of period of interest
#' @param chillVariable - Chill variable selected by user
#' @return `figureCaption` Caption for figure based on selected station


fxn_figureCaption <- function(azmetStation, startDate, endDate, chillVariable) {
  
  azmetStationStartDate <- 
    dplyr::filter(
      azmetStationMetadata, 
      meta_station_name == azmetStation
    )$start_date
  
  if (chillVariable == "Hours below 32 °F") {
    chillVariableText <- "hours below 32 °F"
  } else if (chillVariable == "Hours below 45 °F") {
    chillVariableText <- "hours below 45 °F"
  } else if (chillVariable == "Hours above 68 °F") {
    chillVariableText <- "hours above 68 °F"
  }
  
  standardText <- 
    paste0(
      "Chill accumulation for the current year (black bar in graph) is based on the sum of daily totals of ", chillVariableText, " from ", gsub(" 0", " ", format(startDate, "%B %d, %Y")), " through ", gsub(" 0", " ", format(endDate, "%B %d, %Y")), ". Accumulations for past years (gray bars in graph) are based on the same start and end month and day, but during those respective years. Average chill accumulation is calculated from values of all individual years shown above. Temperature data for the ", azmetStation, " station in the new AZMet database currently go back to ", 
      gsub(" 0", " ", format(azmetStationStartDate, "%B %d, %Y")),
      "."
    )
  
  # Account for multi-month absence of YUG data in 2021
  nonOperational <- 0
  
  if (azmetStation == "Yuma N.Gila") {
    nodataDateRange <-
      lubridate::interval(
        start = lubridate::date("2021-06-16"),
        end = lubridate::date("2021-10-21")
      )

    while (startDate >= azmetStationStartDate) {
      userDateRange <- lubridate::interval(start = startDate, end = endDate)

      if (lubridate::int_overlaps(int1 = nodataDateRange, int2 = userDateRange) == TRUE) {
        nonOperational <- 1
      }

      startDate <- min(seq(startDate, length = 2, by = "-1 year"))
      endDate <- min(seq(endDate, length = 2, by = "-1 year"))
    }
  }
  
  # Generate figure footer based on presence/absence of non-operational dates
  if (azmetStation == "Yuma N.Gila" & nonOperational == 1) {
    figureCaption <- 
      htmltools::p(
        htmltools::HTML(
          paste(
            standardText,
            "However, we do not show chill accumulation for the year with a month-day period that overlaps the period from June 16, 2021 through October 10, 2021, when the ", azmetStation, " station was not in operation.",
            sep = " "
          )
        ),
        
        class = "figure-footer"
      )
  } else {
    figureCaption <- 
      htmltools::p(
        htmltools::HTML(standardText), 
        class = "figure-footer"
      )
  }
  
  return(figureCaption)
}
