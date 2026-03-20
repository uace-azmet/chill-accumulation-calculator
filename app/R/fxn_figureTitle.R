#' `fxn_figureTitle.R` - Build title for figure
#' 
#' @param azmetStation - AZMet station selected by user
#' @return `figureTitle` - Title for figure based on selected station


fxn_figureTitle <- function(azmetStation) {
  figureTitle <- 
    htmltools::p(
      htmltools::HTML(
        paste0(
          bsicons::bs_icon("graph-up", class = "bolder-icon"), 
          htmltools::HTML("&nbsp;&nbsp;"),
          htmltools::HTML(
            toupper(
              paste0(
                "<strong>Chill Accumulation at the AZMet ", azmetStation, " Station</strong>"
              )
            )
          ),
          htmltools::HTML("&nbsp;"),
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
