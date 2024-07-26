#' `fxnFigureSubtitle.R` - Build subtitle for figure based on user input
#' 
#' @param azmetStation - AZMet station selection by user
#' @param inData - data table of seasonal chill accumulation values by year
#' @param startDate - Start date of period of interest
#' @param endDate - End date of period of interest
#' @return `figureSubtitle` - Subtitle for figure based on user specifications


fxnFigureSubtitle <- function(azmetStation, inData, startDate, endDate) {
  currentYear <- lubridate::year(endDate)
  previousYear <- currentYear - 1
  
  currentYearChillSum <- dplyr::filter(inData, endDateYear == currentYear)$chillSum
  previousYearChillSum <- dplyr::filter(inData, endDateYear == previousYear)$chillSum
  totalComparePreviousSum <- currentYearChillSum - previousYearChillSum
  
  previousYearText <- dplyr::filter(inData, endDateYear == previousYear)$dateYearLabel
  
  if (totalComparePreviousSum == 0) {
    compareTextPrevious <- "the same as"
  } else if (totalComparePreviousSum > 0) {
    compareTextPrevious <- 
      paste0(
        format(abs(round(totalComparePreviousSum, digits = 0)), nsmall = 0), " hours greater than"
      )
  } else { # if (totalComparePreviousSum < 0)
    compareTextPrevious <- 
      paste0(
        format(abs(round(totalComparePreviousSum, digits = 0)), nsmall = 0), " hours less than"
      )
  }
  
  # TODO: Add average information
  # TODO: if() for != MOH, WEL, YUE
  if (nrow(inData) == 1) {
    figureSubtitle <- 
      htmltools::p(
        htmltools::HTML(
          paste0(
            "Chill accumulations at the AZMet ", azmetStation, " station from ", gsub(" 0", " ", format(startDate, "%B %d, %Y")), " through ", gsub(" 0", " ", format(endDate, "%B %d, %Y")), " is ", "<b>", format(round(currentYearChillSum, digits = 0), nsmall = 0), " hours</b>."
          ),
        ),
        
        class = "figure-subtitle"
      )
  } else {
    figureSubtitle <- 
      htmltools::p(
        htmltools::HTML(
          paste0(
            "Chill accumulation at the AZMet ", azmetStation, " station from ", gsub(" 0", " ", format(startDate, "%B %d, %Y")), " through ", gsub(" 0", " ", format(endDate, "%B %d, %Y")), " is ", "<b>", format(round(currentYearChillSum, digits = 0), nsmall = 0), " hours</b>. This is ", compareTextPrevious, " the total during this same month-day period in ", previousYearText, "."
          ),
        ),
        
        class = "figure-subtitle"
      )
  }
  
  return(figureSubtitle)
}
