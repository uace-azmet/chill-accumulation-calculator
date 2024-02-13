#' fxnAZMetDataSumHUs: calculates heat unit accumulation based on user input
#' 
#' @param azmetStation - AZMet station selection by user
#' @param startDate - Start date of period of interest
#' @param endDate - End date of period of interest
#' @return `dataAZMetDataSumHUs` - Data table with cumulative heat units by year


fxnAZMetDataSumHUs <- function(azmetStation, startDate, endDate) {
  dataAZMetDataMerge <- fxnAZMetDataMerge(
    azmetStation = azmetStation, startDate = startDate, endDate = endDate
  )
  
  # For case of missing data from Yuma North Gila
  if (azmetStation == "Yuma North Gila" && endDate >= lubridate::as_date(paste0(lubridate::year(endDate), "-06-16"))) {
    dataAZMetDataMerge <- dataAZMetDataMerge %>%
      dplyr::filter(date_year != 2021)
  }
  
  dataAZMetDataSumHUs <- dataAZMetDataMerge %>%
    dplyr::group_by(date_year) %>%
    dplyr::summarize(heat_units_55F_cumulative = sum(heat_units_55F, na.rm = TRUE)) %>%
    dplyr::mutate(labelHUs = format(round(heat_units_55F_cumulative, digits = 1), nsmall = 1))
  
  return(dataAZMetDataSumHUs)
}
