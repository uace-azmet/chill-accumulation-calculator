#' `fxnFigureTitle.R` - Build title for figure based on user input
#' 
#' @param inData - data table of seasonal chill accumulation values by year
#' @param endDate - End date of period of interest
#' @return `figureTitle` - Title for figure based on selected station


fxnFigureTitle <- function(inData, endDate) {
  currentYear <- lubridate::year(endDate)
  currentYearChill <- 
    inData$chillSum[which(inData$date_year == currentYear)]
  
  previousYear <- currentYear - 1
  previousYearChill <- 
    inData$chillSum[which(inData$date_year == previousYear)]
  
  if (nrow(inData) < 2) {
    figureTitle <- 
      htmltools::h4(
        htmltools::HTML(
          paste(
            "Chill Accumulation in", currentYear,
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
            "Chill Accumulation in", currentYear, comparisonText, "That in", previousYear,
            sep = " "
          ),
        ),
        
        class = "figure-title"
      )
    
  }
  
  return(figureTitle)
}
