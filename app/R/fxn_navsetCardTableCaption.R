#' `fxn_navsetCardTableCaption.R` - Build caption for table summary based on user input
#' 
#' @param chillVariable - Chill variable selected by user
#' @return `navsetCardTableCaption` Caption for table summary based on user input


fxn_navsetCardTableCaption <- function(chillVariable) {
  
  if (chillVariable == "Chill Portions") {
    variableUnits <- "portions"
  } else if (chillVariable == "Utah Model") {
    variableUnits <- "units"
  } else { # "Hours below 32 °F", "Hours between 32 and 45 °F", "Hours below 45 °F", "Hours above 68 °F"
    variableUnits <- "hours"
  }
  
  if (chillVariable == "Chill Portions") {
    chillVariableText <- "chill portions"
  } else if (chillVariable == "Hours below 32 °F") {
    chillVariableText <- "hours below 32 °F"
  } else if (chillVariable == "Hours below 45 °F") {
    chillVariableText <- "hours below 45 °F"
  } else if (chillVariable == "Hours between 32 and 45 °F") {
    chillVariableText <- "hours between 32 and 45 °F"
  } else if (chillVariable == "Hours above 68 °F") {
    chillVariableText <- "hours above 68 °F"
  } else if (chillVariable == "Utah Model") {
    chillVariableText <- "Utah Model chill units"
  }
  
  captionText <- 
    paste0(
      "Values of 'NA' denote no data. Variable key: <strong>Day<sub>period</sub></strong> day number of the period of interest; <strong>Chill</strong> daily chill values in ", variableUnits, " as represented by ", chillVariableText, "; <strong>Chill<sub>cumulative</sub></strong> accumulation of daily chill values in ", variableUnits, " during the period of interest as represented by ", chillVariableText, "."
    )
  
  # Generate caption text with `chillR` reference
  if (chillVariable == "Chill Portions") {
    captionText <- 
      paste0(
        captionText, " Chill portions are based on calculations in the", htmltools::tags$code("chillR", style = "color: #606060;"), "R package."
      )
  } else if (chillVariable == "Utah Model") {
    captionText <- 
      paste0(
        captionText, " Utah Model chill units are based on calculations in the", htmltools::tags$code("chillR", style = "color: #606060;"), "R package. Accumulation values reset daily to 0.0 when negative."
      )
  } else {
    captionText <- captionText
  }
  
  # Format caption text as HTML
  navsetCardTableCaption <- 
    htmltools::p(
      htmltools::HTML(captionText), 
      class = "navset-card-caption"
    )
  
  return(navsetCardTableCaption)
}
