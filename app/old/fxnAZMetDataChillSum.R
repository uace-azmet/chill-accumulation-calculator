#' fxnAZMetDataChillSum: calculates chill accumulation based on user input
#' 
#' @param inData - dataAZMetdataELT
#' @param azmetStation - AZMet station selection by user
#' @param startDate - Start date of period of interest
#' @param endDate - End date of period of interest
#' @param chillVariable - Chill variable selection by user
#' @return `dataAZMetDataChillSum` - Data table with cumulative chill hours by year


fxnAZMetDataChillSum <- function(inData, azmetStation, startDate, endDate, chillVariable) {
  # For x-axis labels and related text of comparison to previous years
  if (lubridate::year(startDate) == lubridate::year(endDate)) { # For data request spanning a single calendar year
    dateYear <- as.character(lubridate::year(startDate))
  } else { # For data request spanning two calendar years
    dateYear <- 
      paste(
        lubridate::year(startDate), 
        lubridate::year(endDate), 
        sep = "-"
      )
  }
  
  if (nrow(inData) == 0) { # For case of empty data return
    dataAZMetDataHeatSum <- data.frame(matrix(
      data = NA,
      nrow = 1, 
      ncol = length(c("meta_station_name", "chillSum", "chillSumLabel", "endDateYear", "dateYearLabel"))
    ))
    
    colnames(dataAZMetDataHeatSum) <- 
      c("meta_station_name", "chillSum", "chillSumLabel", "endDateYear", "dateYearLabel")
    
    dataAZMetDataChillSum <- dataAZMetDataHeatSum %>%
      dplyr::mutate(meta_station_name = azmetStation) %>%
      dplyr::mutate(chillSum = 0.0) %>%
      dplyr::mutate(chillSumLabel = "NA") %>%
      dplyr::mutate(endDateYear = lubridate::year(endDate)) %>%
      dplyr::mutate(dateYearLabel = dateYear)
  } else {
    if (chillVariable == "Hours below 32 °F") {
      dataAZMetDataChillSum <- inData %>%
        dplyr::group_by(meta_station_name) %>%
        dplyr::summarize(chill_hours_32F_cumulative = sum(chill_hours_32F, na.rm = TRUE)) %>%
        dplyr::rename(chillSum = chill_hours_32F_cumulative) %>%
        dplyr::mutate(chillSumLabel = format(round(chillSum, digits = 0), nsmall = 0)) %>%
        dplyr::mutate(endDateYear = lubridate::year(endDate)) %>%
        dplyr::mutate(dateYearLabel = dateYear)
    } else if (chillVariable == "Hours below 45 °F") {
      dataAZMetDataChillSum <- inData %>%
        dplyr::group_by(meta_station_name) %>%
        dplyr::summarize(chill_hours_45F_cumulative = sum(chill_hours_45F, na.rm = TRUE)) %>%
        dplyr::rename(chillSum = chill_hours_45F_cumulative) %>%
        dplyr::mutate(chillSumLabel = format(round(chillSum, digits = 0), nsmall = 0)) %>%
        dplyr::mutate(endDateYear = lubridate::year(endDate)) %>%
        dplyr::mutate(dateYearLabel = dateYear)
    } else if (chillVariable == "Hours above 68 °F") {
      dataAZMetDataChillSum <- inData %>%
        dplyr::group_by(meta_station_name) %>%
        dplyr::summarize(chill_hours_68F_cumulative = sum(chill_hours_68F, na.rm = TRUE)) %>%
        dplyr::rename(chillSum = chill_hours_68F_cumulative) %>%
        dplyr::mutate(chillSumLabel = format(round(chillSum, digits = 0), nsmall = 0)) %>%
        dplyr::mutate(endDateYear = lubridate::year(endDate)) %>%
        dplyr::mutate(dateYearLabel = dateYear)
    }
  }
  
  return(dataAZMetDataChillSum)
}
