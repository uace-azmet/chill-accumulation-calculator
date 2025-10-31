#' `fxn_figureTitle.R` - Build title for figure
#' 
#' @param azmetStation - AZMet station selected by user
#' @return `figureTitle` - Title for figure based on selected station


fxn_figureTitle <- function(azmetStation) {
  figureTitle <- 
    htmltools::p(
      htmltools::HTML(
        paste0(
          bsicons::bs_icon("graph-up"), 
          htmltools::HTML("&nbsp;"),
          htmltools::HTML("&nbsp;"),
          toupper(
            paste0(
              "Chill Accumulation at the AZMet ", azmetStation, " Station"
            )
          ),
          htmltools::HTML("&nbsp;&nbsp;&nbsp;&nbsp;"),
          bslib::tooltip(
            bsicons::bs_icon("info-circle"),
            "Hover over bars for values of chill accumulation.",
            id = "infoFigureTitle",
            placement = "right"
          )
        ),
      ),
      
      class = "figure-title"
    )
  
  return(figureTitle)
}
