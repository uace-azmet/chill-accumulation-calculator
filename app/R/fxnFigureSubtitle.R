#' `fxnFigureSubtitle.R` - Build subtitle for figure based on user input
#' 
#' @param azmetStation - AZMet station selection by user
#' @param startDate - Planting date of period of interest
#' @param endDate - End date of period of interest
#' @return `figureSubtitle` - Subtitle for figure based on selected AZMet station


fxnFigureSubtitle <- function(azmetStation, startDate, endDate) {
  startMonth <- lubridate::month(startDate, label = TRUE, abbr = FALSE)
  startDay <- lubridate::mday(startDate)
  endMonth <- lubridate::month(endDate, label = TRUE, abbr = FALSE)
  endDay <- lubridate::mday(endDate)
  
  figureSubtitle <- 
    htmltools::p(
      htmltools::HTML(
        paste(
          "at the AZMet", azmetStation, "station from", startMonth, startDay, "through", endMonth, endDay,
          sep = " "
        ),
      ),
      
      class = "figure-subtitle"
    )
  
  return(figureSubtitle)
}
