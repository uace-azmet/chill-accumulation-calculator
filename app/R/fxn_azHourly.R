#' `fxn_azHourly.R` Download AZMet hourly data from API-based database
#' 
#' @param azmetStation - AZMet station name
#' @param startDate - Start date of period of interest
#' @param endDate - End date of period of interest
#' @return `azHourly` - Table of downloaded, transformed hourly data


fxn_azHourly <- function(azmetStation, startDate, endDate) {
  startDateTime = paste(startDate, "01", sep = " ")
  endDateTime = paste(endDate, "24", sep = " ")
  
  azHourly <- azmetr::az_hourly(
    station_id = 
      dplyr::filter(azmetStationMetadata, meta_station_name == azmetStation)$meta_station_id,
    start_date = startDateTime, 
    end_date = endDateTime
  ) %>% 
    dplyr::select(
      dplyr::all_of(
        c(hourlyVarsID, hourlyVarsMeasured, hourlyVarsDerived)
      )
    ) # Defined in `_global.R`
  
  return(azHourly)
}
