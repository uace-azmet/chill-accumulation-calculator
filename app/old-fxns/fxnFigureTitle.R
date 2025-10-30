#' `fxnFigureTitle.R` - Build title for figure based on user input
#' 
#' @param inData - data table of seasonal chill accumulation values by year
#' @param endDate - End date of period of interest
#' @return `figureTitle` - Title for figure based on selected station


fxnFigureTitle <- function(inData, endDate) {
  chillSum <- dplyr::filter(inData, endDateYear == lubridate::year(endDate))$chillSum
  chillSum <- format(round(chillSum, digits = 0), nsmall = 0)
  
  figureTitle <- 
    htmltools::h4(
      htmltools::HTML(
        paste0(
          "<b>", chillSum, " hours", "</b>"
        ),
      ),
      
      class = "figure-title"
    )
  
  return(figureTitle)
}
