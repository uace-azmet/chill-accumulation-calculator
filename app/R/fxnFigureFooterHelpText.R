#' `fxnFigureFooterHelpText.R` - Build help text for figure footer
#' 
#' @return `figureFooterHelpText` - Help text for figure footer


fxnFigureFooterHelpText <- function() {
  figureFooterHelpText <- 
    htmltools::p(
      htmltools::HTML(
        "Scroll down over the following text to view additional information. This feature appears with certain device and browser settings."
      ), 
      
      class = "figure-footer-help-text"
    )
  
  return(figureFooterHelpText)
}
