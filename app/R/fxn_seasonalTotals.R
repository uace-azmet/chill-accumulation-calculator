#' `fxn_seasonalTotals` - Combines seasonal chill accumulation from individual years
#' 
#' @param azmetStation - AZMet station selected by user
#' @param startDate - Start date of period of interest
#' @param endDate - End date of period of interest
#' @param chillVariable - Chill variable selected by user
#' @return `seasonalTotals` - Data table of seasonal chill accumulation from individual years


fxn_seasonalTotals <- function(azmetStation, startDate, endDate, chillVariable) {
  
  azmetStationStartDate <- 
    dplyr::filter(azmetStationMetadata, meta_station_name == azmetStation)$start_date
    
  azDaily <- 
    fxn_azDaily(
      azmetStation = azmetStation,
      startDate = azmetStationStartDate, # To call API only once
      endDate = endDate
    )
  
  while (startDate >= azmetStationStartDate) {
    seasonalData <- 
      dplyr::filter(
        azDaily,
        datetime >= startDate & datetime <= endDate
      )
    
    # Calculate seasonal total from individual year and prepare data for graph
    chillTotal <- 
      fxn_chillTotal(
        inData = seasonalData,
        azmetStation = azmetStation, 
        startDate = startDate, 
        endDate = endDate,
        chillVariable = chillVariable
      )
    
    # Account for multi-month absence of YUG data in 2021
    if (azmetStation == "Yuma N.Gila") {
      nodataDateRange <-
        lubridate::interval(
          start = lubridate::date("2021-06-16"),
          end = lubridate::date("2021-10-21")
        )

      userDateRange <- lubridate::interval(start = startDate, end = endDate)

      if (lubridate::int_overlaps(int1 = nodataDateRange, int2 = userDateRange) == TRUE) {
        chillTotal$chillTotal <- 0
        chillTotal$chillTotalLabel <- "NA"
      }
    }

    if (exists("seasonalTotals") == FALSE) {
      seasonalTotals <- chillTotal
    } else {
      seasonalTotals <- rbind(seasonalTotals, chillTotal)
    }
    
    startDate <- min(seq(lubridate::date(startDate), length = 2, by = "-1 year"))
    endDate <- min(seq(lubridate::date(endDate), length = 2, by = "-1 year"))
  }
  
  # Account for multi-month absence of YUG data in 2021
  if (azmetStation == "Yuma N.Gila") {
    seasonalTotals <- seasonalTotals %>% 
      dplyr::filter(chillTotalLabel != "NA")
  }
  
  return(seasonalTotals)
}
