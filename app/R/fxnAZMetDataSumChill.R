#' fxnAZMetDataSumChill: calculates heat unit accumulation based on user input
#' 
#' @param azmetStation - AZMet station selection by user
#' @param startDate - Start date of period of interest
#' @param endDate - End date of period of interest
#' @param chillVariable - Chill variable selection by user
#' @return `dataAZMetDataSumChill` - Data table with cumulative heat units by year


fxnAZMetDataSumChill <- function(azmetStation, startDate, endDate, chillVariable) {
  dataAZMetDataMerge <- fxnAZMetDataMerge(
    azmetStation = azmetStation, startDate = startDate, endDate = endDate
  )
  
  # For case of missing data from Yuma North Gila
  if (azmetStation == "Yuma North Gila" && endDate >= lubridate::as_date(paste0(lubridate::year(endDate), "-06-16"))) {
    dataAZMetDataMerge <- dataAZMetDataMerge %>%
      dplyr::filter(date_year != 2021)
  }
  
  if (chillVariable == "Hours below 32 °F") {
    dataAZMetDataSumChill <- dataAZMetDataMerge %>%
      dplyr::group_by(date_year) %>%
      dplyr::summarize(chill_hours_32F_cumulative = sum(chill_hours_32F, na.rm = TRUE)) %>%
      dplyr::rename(chillSum = chill_hours_32F_cumulative) %>%
      dplyr::mutate(chillSumLabel = format(round(chillSum, digits = 0), nsmall = 0))
  } else if (chillVariable == "Hours below 45 °F") {
    dataAZMetDataSumChill <- dataAZMetDataMerge %>%
      dplyr::group_by(date_year) %>%
      dplyr::summarize(chill_hours_45F_cumulative = sum(chill_hours_45F, na.rm = TRUE)) %>%
      dplyr::rename(chillSum = chill_hours_45F_cumulative) %>%
      dplyr::mutate(chillSumLabel = format(round(chillSum, digits = 0), nsmall = 0))
  } else if (chillVariable == "Hours above 68 °F") {
    dataAZMetDataSumChill <- dataAZMetDataMerge %>%
      dplyr::group_by(date_year) %>%
      dplyr::summarize(chill_hours_68F_cumulative = sum(chill_hours_68F, na.rm = TRUE)) %>%
      dplyr::rename(chillSum = chill_hours_68F_cumulative) %>%
      dplyr::mutate(chillSumLabel = format(round(chillSum, digits = 0), nsmall = 0))
  }
  
  return(dataAZMetDataSumChill)
}
