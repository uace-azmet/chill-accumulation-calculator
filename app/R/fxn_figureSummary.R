#' `fxn_figureSummary.R` - Build summary of figure based on user input
#' 
#' @param azmetStation - AZMet station selection by user
#' @param inData - data table of seasonal total evapotranspiration by year
#' @param startDate - Start date of period of interest
#' @param endDate - End date of period of interest
#' @return `figureSummary` - Summary of figure based on user inputs


fxn_figureSummary <- function(azmetStation, inData, startDate, endDate) {
  
  currentYear <- lubridate::year(endDate)
  currentYearTotal <- dplyr::filter(inData, endDateYear == currentYear)$chillTotal
  
  # For stations with only one year of data
  if (nrow(inData) == 1) {
    figureSummary <- 
      htmltools::p(
        htmltools::HTML(
          paste0(
            "Chill accumulation at the AZMet ", azmetStation, " station from ", gsub(" 0", " ", format(startDate, "%B %d, %Y")), " through ", gsub(" 0", " ", format(endDate, "%B %d, %Y")), " is ", "<b>", format(round(currentYearTotal, digits = 0), nsmall = 0), " hours</b>."
          ),
        ),
        
        class = "figure-summary"
      )
  } else {
    averageTotal <- mean(inData$chillTotal, na.rm = TRUE)
    previousYear <- currentYear - 1
    previousYearText <- dplyr::filter(inData, endDateYear == previousYear)$dateYearLabel
    previousYearTotal <- dplyr::filter(inData, endDateYear == previousYear)$chillTotal
    
    differenceAverage <- currentYearTotal - averageTotal
    differencePreviousYear <- currentYearTotal - previousYearTotal
    
    if (round(differenceAverage, digits = 2) > 0) {
      differenceAverageText <- 
        paste0(
          format(abs(round(differenceAverage, digits = 2)), nsmall = 2), " hours more than"
        )
    } else if (round(differenceAverage, digits = 2) < 0) {
      differenceAverageText <- 
        paste0(
          format(abs(round(differenceAverage, digits = 2)), nsmall = 2), " hours less than"
        )
    } else { # if (differenceAverage = 0)
      differenceAverageText <- "equal to"
    }
    
    if (differencePreviousYear == 0) {
      differencePreviousYearText <- "the same as"
    } else if (differencePreviousYear > 0) {
      differencePreviousYearText <- 
        paste0(
          format(abs(round(differencePreviousYear, digits = 0)), nsmall = 0), " hours more than"
        )
    } else { # if (differencePreviousYear < 0)
      differencePreviousYearText <- 
        paste0(
          format(abs(round(differencePreviousYear, digits = 0)), nsmall = 0), " hours less than"
        )
    }
    
    figureSummary <- 
      htmltools::p(
        htmltools::HTML(
          paste0(
            "Chill accumulation at the AZMet ", azmetStation, " station from ", gsub(" 0", " ", format(startDate, "%B %d, %Y")), " through ", gsub(" 0", " ", format(endDate, "%B %d, %Y")), " is ", "<b>", format(round(currentYearTotal, digits = 0), nsmall = 0), " hours</b>. This is ", differencePreviousYearText, " the accumulation during this same month-day period in ", previousYearText, ", and ", differenceAverageText, " the station average."
          ),
        ),
        
        class = "figure-summary"
      )
  }
  
  return(figureSummary)
}
