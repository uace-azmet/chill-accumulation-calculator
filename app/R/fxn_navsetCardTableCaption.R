#' `fxn_navsetCardTableCaption.R` - Build caption for table summary based on user input
#' 
#' @param chillVariable - Chill variable selected by user
#' @return `navsetCardTableCaption` Caption for table summary based on user input


fxn_navsetCardTableCaption <- function(chillVariable) {
  
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
  
  if (chillVariable == "Chill Portions") {
    variableUnits <- "portions"
  } else if (chillVariable == "Utah Model") {
    variableUnits <- "units"
  } else { # "Hours below 32 °F", "Hours between 32 and 45 °F", "Hours below 45 °F", "Hours above 68 °F"
    variableUnits <- "hours"
  }
  
  standardText <- "Values of 'NA' denote no data."
  
  # Generate caption text with `chillR` reference
  if (chillVariable == "Chill Portions") {
    standardText <- 
      paste0(
        standardText, " Chill portions are based on calculations in the", "<a href=", "https://eikeluedeling.r-universe.dev/chillR>", htmltools::tags$code("chillR", style = "color: #8B0015;"), "</a>", "R package."
      )
  } else if (chillVariable == "Utah Model") {
    standardText <- 
      paste0(
        standardText, " Utah Model chill units are based on calculations in the", "<a href=", "https://eikeluedeling.r-universe.dev/chillR>", htmltools::tags$code("chillR", style = "color: #8B0015;"), "</a>", "R package. Accumulation values reset daily to 0.0 when negative."
      )
  } else {
    standardText <- standardText
  }
  
  variableKeyText <- 
    paste0(
      "Variable key: <strong>Day<sub>period</sub></strong> day number of the period of interest; <strong>Chill</strong> daily chill values in ", variableUnits, " as represented by ", chillVariableText, "; <strong>Chill<sub>cumulative</sub></strong> accumulation of daily chill values in ", variableUnits, " during the period of interest as represented by ", chillVariableText
    )
  
  # Format caption text as HTML
  navsetCardTableCaption <- 
    htmltools::p(
      htmltools::HTML(
        paste(
          standardText,
          variableKeyText,
          sep = " "
        )
      ), 
      class = "navset-card-caption"
    )
  
  return(navsetCardTableCaption)
}
