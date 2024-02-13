#' `fxnFigureTitle.R` - Build title for figure based on user input
#' 
#' @param inData - data table of seasonal heat accumulation values by year
#' @param endDate - End date of period of interest
#' @return `figureTitle` - Title for figure based on selected station


fxnFigureTitle <- function(inData, endDate) {
  currentYear <- lubridate::year(endDate)
  currentYearHUs <- 
    inData$heat_units_55F_cumulative[which(inData$date_year == currentYear)]
  
  previousYear <- currentYear - 1
  previousYearHUs <- 
    inData$heat_units_55F_cumulative[which(inData$date_year == previousYear)]
  
  if (nrow(inData) < 2) {
    figureTitle <- 
      htmltools::h4(
        htmltools::HTML(
          paste(
            "Heat Accumulation in", currentYear,
            sep = " "
          ),
        ),
        
        class = "figure-title"
      )
  } else {
    if (currentYearHUs > (previousYearHUs + (0.1 * previousYearHUs))) {
      comparisonText <- "Greater than"
    } else if (currentYearHUs < (previousYearHUs - (0.1 * previousYearHUs))) {
      comparisonText <- "Less than"
    } else {
      comparisonText <- "Similar to"
    }
    
    figureTitle <- 
      htmltools::h4(
        htmltools::HTML(
          paste(
            "Heat Accumulation in", currentYear, comparisonText, "That in", previousYear,
            sep = " "
          ),
        ),
        
        class = "figure-title"
      )
    
  }
  
  return(figureTitle)
}
