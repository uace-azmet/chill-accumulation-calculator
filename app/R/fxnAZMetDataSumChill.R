#' fxnAZMetDataSumChill: calculates heat unit accumulation based on user input
#' 
#' @param inData - dataAZMetdataELT
#' @param azmetStation - AZMet station selection by user
#' @param startDate - Start date of period of interest
#' @param endDate - End date of period of interest
#' @param chillVariable - Chill variable selection by user
#' @return `dataAZMetDataSumChill` - Data table with cumulative heat units by year


fxnAZMetDataSumChill <- function(inData, azmetStation, startDate, endDate, chillVariable) {
  if (length(unique(inData$date_year)) == 1) { # For single calendar year in data
    dateYear <- as.character(unique(inData$date_year))
  } else { # For two calendars year in data
    dateYear <- paste(min(unique(inData$date_year)), max(unique(inData$date_year)), sep = "-")
  }
  
  if (chillVariable == "Hours below 32 °F") {
    dataAZMetDataSumChill <- inData %>%
      dplyr::group_by(meta_station_name) %>%
      dplyr::summarize(chill_hours_32F_cumulative = sum(chill_hours_32F, na.rm = TRUE)) %>%
      dplyr::rename(chillSum = chill_hours_32F_cumulative) %>%
      dplyr::mutate(chillSumLabel = format(round(chillSum, digits = 0), nsmall = 0)) %>%
      dplyr::mutate(dateYearLabel = dateYear)
  } else if (chillVariable == "Hours below 45 °F") {
    dataAZMetDataSumChill <- inData %>%
      dplyr::group_by(meta_station_name) %>%
      dplyr::summarize(chill_hours_45F_cumulative = sum(chill_hours_45F, na.rm = TRUE)) %>%
      dplyr::rename(chillSum = chill_hours_45F_cumulative) %>%
      dplyr::mutate(chillSumLabel = format(round(chillSum, digits = 0), nsmall = 0)) %>%
      dplyr::mutate(dateYearLabel = dateYear)
  } else if (chillVariable == "Hours above 68 °F") {
    dataAZMetDataSumChill <- inData %>%
      dplyr::group_by(meta_station_name) %>%
      dplyr::summarize(chill_hours_68F_cumulative = sum(chill_hours_68F, na.rm = TRUE)) %>%
      dplyr::rename(chillSum = chill_hours_68F_cumulative) %>%
      dplyr::mutate(chillSumLabel = format(round(chillSum, digits = 0), nsmall = 0)) %>%
      dplyr::mutate(dateYearLabel = dateYear)
  }
  
  return(dataAZMetDataSumChill)
}
