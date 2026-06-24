#' `fxn_chillTotal` - calculates chill accumulation for an individual season
#' 
#' @param inData - Daily data from an individual season
#' @param azmetStation - AZMet station selection by user
#' @param startDate - Start date of period of interest
#' @param endDate - End date of period of interest
#' @param chillVariable - Chill variable selected by user
#' @return `chillTotal` - Data table with chill accumulation for an individual season


fxn_chillTotal <- function(inData, azmetStation, startDate, endDate, chillVariable) {
  
  # For x-axis labels and related text of comparison to previous years
  if (lubridate::year(startDate) == lubridate::year(endDate)) { # For data request spanning a single calendar year
    dateYearLabel <- as.character(lubridate::year(startDate))
  } else { # For data request spanning two calendar years
    dateYearLabel <- 
      paste(lubridate::year(startDate), lubridate::year(endDate), sep = "-")
  }
  
  if (nrow(inData) == 0) { # For case of empty data return
    chillTotal <- data.frame(matrix(
      data = NA,
      nrow = 1, 
      ncol = 
        length(
          c(
            "meta_station_name", 
            "chillTotal", 
            "chillTotalLabel", 
            "endDateYear", 
            "dateYearLabel"
          )
        )
    ))
    
    colnames(chillTotal) <- 
      c(
        "meta_station_name", 
        "chillTotal", 
        "chillTotalLabel", 
        "endDateYear", 
        "dateYearLabel"
      )
    
    chillTotal <- chillTotal %>%
      dplyr::mutate(meta_station_name = azmetStation) %>%
      dplyr::mutate(chillTotal = 0.00) %>%
      dplyr::mutate(chillTotalLabel = "NA") %>%
      dplyr::mutate(endDateYear = lubridate::year(endDate)) %>%
      dplyr::mutate(dateYearLabel = dateYearLabel)
  } else {
    if (chillVariable == "Chill Portions") {
      chillTotal <- inData %>%
        dplyr::summarize(chill_portions_total = sum(chill_portions, na.rm = TRUE)) %>%
        dplyr::rename(chillTotal = chill_portions_total) %>%
        dplyr::mutate(chillTotalLabel = format(round(chillTotal, digits = 1), nsmall = 1)) %>%
        dplyr::mutate(endDateYear = lubridate::year(endDate)) %>%
        dplyr::mutate(dateYearLabel = dateYearLabel)
    } else if (chillVariable == "Hours below 32 °F") {
      chillTotal <- inData %>%
        dplyr::summarize(chill_hours_32F_total = sum(chill_hours_32F, na.rm = TRUE)) %>%
        dplyr::rename(chillTotal = chill_hours_32F_total) %>%
        dplyr::mutate(chillTotalLabel = format(round(chillTotal, digits = 0), nsmall = 0)) %>%
        dplyr::mutate(endDateYear = lubridate::year(endDate)) %>%
        dplyr::mutate(dateYearLabel = dateYearLabel)
    } else if (chillVariable == "Hours below 45 °F") {
      chillTotal <- inData %>%
        dplyr::summarize(chill_hours_45F_total = sum(chill_hours_45F, na.rm = TRUE)) %>%
        dplyr::rename(chillTotal = chill_hours_45F_total) %>%
        dplyr::mutate(chillTotalLabel = format(round(chillTotal, digits = 0), nsmall = 0)) %>%
        dplyr::mutate(endDateYear = lubridate::year(endDate)) %>%
        dplyr::mutate(dateYearLabel = dateYearLabel)
    } else if (chillVariable == "Hours between 32 and 45 °F") {
      chillTotal <- inData %>%
        dplyr::summarize(chill_hours_3245F_total = sum(chill_hours_3245F, na.rm = TRUE)) %>%
        dplyr::rename(chillTotal = chill_hours_3245F_total) %>%
        dplyr::mutate(chillTotalLabel = format(round(chillTotal, digits = 0), nsmall = 0)) %>%
        dplyr::mutate(endDateYear = lubridate::year(endDate)) %>%
        dplyr::mutate(dateYearLabel = dateYearLabel)
    } else if (chillVariable == "Hours above 68 °F") {
      chillTotal <- inData %>%
        dplyr::summarize(chill_hours_68F_total = sum(chill_hours_68F, na.rm = TRUE)) %>%
        dplyr::rename(chillTotal = chill_hours_68F_total) %>%
        dplyr::mutate(chillTotalLabel = format(round(chillTotal, digits = 0), nsmall = 0)) %>%
        dplyr::mutate(endDateYear = lubridate::year(endDate)) %>%
        dplyr::mutate(dateYearLabel = dateYearLabel)
    } else if (chillVariable == "Utah Model") {
      inData <- inData %>% 
        dplyr::mutate(utah_model_accum = NA_real_)
      
      for (i in 1:length(inData$utah_model)) {
        if (i == 1) {
          if (inData$utah_model[i] < 0) {
            inData$utah_model_accum[i] <- 0
          } else {
            inData$utah_model_accum[i] <- inData$utah_model[i]
          }
        } else {
          if (inData$utah_model[i] + inData$utah_model_accum[i - 1] < 0) {
            inData$utah_model_accum[i] <- 0
          } else {
            inData$utah_model_accum[i] <- inData$utah_model[i] + inData$utah_model_accum[i - 1]
          }
        }
      }
      rm(i)
      
      chillTotal <- inData %>%
        dplyr::summarize(utah_model_total = dplyr::last(utah_model_accum, na_rm = TRUE)) %>%
        dplyr::rename(chillTotal = utah_model_total) %>%
        dplyr::mutate(chillTotalLabel = format(round(chillTotal, digits = 1), nsmall = 1)) %>%
        dplyr::mutate(endDateYear = lubridate::year(endDate)) %>%
        dplyr::mutate(dateYearLabel = dateYearLabel)
    }
  }
  
  return(chillTotal)
}
