#' fxnAZMetDataMerge: downloads and merges individual-year data since API database start and based on user input
#' 
#' @param azmetStation - AZMet station selection by user
#' @param startDate - Start date of period of interest
#' @param endDate - End date of period of interest
#' @param chillVariable - Chill variable selection by user
#' @return `dataAZMetDataMerge` - merged data tables from individual years


fxnAZMetDataMerge <- function(azmetStation, startDate, endDate) {
  azmetStationStartDate <- lubridate::as_date("2021-01-01") # Placeholder for station start date
  
  while (startDate >= azmetStationStartDate) {
    
    dataAZMetDataELT <- fxnAZMetDataELT(
      azmetStation = azmetStation, timeStep = "Daily", startDate = startDate, endDate = endDate
    )
    
    # For case of empty data return
    if (nrow(dataAZMetDataELT) == 0) {
      startDate <- min(seq(startDate, length = 2, by = "-1 year"))
      endDate <- min(seq(endDate, length = 2, by = "-1 year"))
    } else {
      # Chill accumulation calculation
      dataAZMetDataSumChill <- fxnAZMetDataSumChill(
        inData = dataAZMetDataELT,
        azmetStation = azmetStation, 
        startDate = startDate, 
        endDate = endDate,
        chillVariable = chillVariable
      )
      
      if (exists("dataAZMetDataMerge") == FALSE) {
        dataAZMetDataMerge <- dataAZMetDataSumChill
      } else {
        dataAZMetDataMerge <- rbind(dataAZMetDataMerge, dataAZMetDataSumChill)
      }
      
      startDate <- min(seq(startDate, length = 2, by = "-1 year"))
      endDate <- min(seq(endDate, length = 2, by = "-1 year"))
    }
  }
  
  return(dataAZMetDataMerge)
}



# For case of missing data from Yuma North Gila
if (azmetStation == "Yuma North Gila" && endDate >= lubridate::as_date(paste0(lubridate::year(endDate), "-06-16"))) {
  dataAZMetDataMerge <- dataAZMetDataMerge %>%
    dplyr::filter(date_year != 2021)
}
