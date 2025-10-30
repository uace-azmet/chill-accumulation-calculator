#' fxnAZMetDataMerge: downloads and merges individual-year data since API database start and based on user input
#' 
#' @param azmetStation - AZMet station selection by user
#' @param startDate - Start date of period of interest
#' @param endDate - End date of period of interest
#' @param chillVariable - Chill variable selection by user
#' @return `dataAZMetDataMerge` - merged data tables from individual years


fxnAZMetDataMerge <- function(azmetStation, startDate, endDate, chillVariable) {
  azmetStationStartDate <- apiStartDate # Placeholder for station start date
  
  while (startDate >= azmetStationStartDate) {
    
    dataAZMetDataELT <- fxnAZMetDataELT(
      azmetStation = azmetStation, 
      timeStep = "Daily", 
      startDate = startDate, 
      endDate = endDate
    )
    
    dataAZMetDataChillSum <- fxnAZMetDataChillSum(
      inData = dataAZMetDataELT,
      azmetStation = azmetStation, 
      chillVariable = chillVariable,
      startDate = startDate, 
      endDate = endDate
    )
    
    if (azmetStation == "Yuma North Gila") {
      nodataDateRange <- 
        lubridate::interval(
          start = lubridate::date("2021-06-16"), 
          end = lubridate::date("2021-10-21")
        )
      
      userDateRange <- lubridate::interval(start = startDate, end = endDate)
      
      if (lubridate::int_overlaps(int1 = nodataDateRange, int2 = userDateRange) == TRUE) {
        dataAZMetDataChillSum$chillSum <- 0.00
        dataAZMetDataChillSum$chillSumLabel <- "NA" 
      }
    }
    
    if (exists("dataAZMetDataMerge") == FALSE) {
      dataAZMetDataMerge <- dataAZMetDataChillSum
    } else {
      dataAZMetDataMerge <- rbind(dataAZMetDataMerge, dataAZMetDataChillSum)
    }
    
    startDate <- min(seq(startDate, length = 2, by = "-1 year"))
    endDate <- min(seq(endDate, length = 2, by = "-1 year"))
  }
  
  return(dataAZMetDataMerge)
}
