#' `fxnFigureTitle.R` - Build title for figure based on user input
#' 
#' @param inData - data table of seasonal chill accumulation values by year
#' @param endDate - End date of period of interest
#' @param chillVariable - Chill variable selection by user
#' @return `figureTitle` - Title for figure based on selected station


fxnFigureTitle <- function(inData, endDate, chillVariable) {
  currentYear <- lubridate::year(endDate)
  currentYearChill <- 
    inData$chillSum[which(inData$endDateYear == currentYear)]
  currentYearChillText <- inData$dateYearLabel[which(inData$endDateYear == currentYear)]
  
  previousYear <- currentYear - 1
  previousYearChill <- 
    inData$chillSum[which(inData$endDateYear == previousYear)]
  previousYearChillText <- inData$dateYearLabel[which(inData$endDateYear == previousYear)]
  
  if (nrow(inData) < 2) {
    figureTitle <- 
      htmltools::h4(
        htmltools::HTML(
          paste(
            "Total Number of", chillVariable, "in", currentYear,
            sep = " "
          ),
        ),
        
        class = "figure-title"
      )
  } else {
    if (currentYearChill > (previousYearChill + (0.1 * previousYearChill))) {
      comparisonText <- "Greater than"
    } else if (currentYearChill < (previousYearChill - (0.1 * previousYearChill))) {
      comparisonText <- "Less than"
    } else {
      comparisonText <- "Similar to"
    }
    
    figureTitle <- 
      htmltools::h4(
        htmltools::HTML(
          paste(
            "Total Number of", chillVariable, "in", currentYearChillText, comparisonText, "That in", previousYearChillText,
            sep = " "
          ),
        ),
        
        class = "figure-title"
      )
    
  }
  
  return(figureTitle)
}
