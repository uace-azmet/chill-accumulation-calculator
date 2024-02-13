#' `fxnFigureFooter.R` - Build footer for figure based on user input
#' 
#' @param azmetStation - AZMet station selection by user
#' @param startDate - Planting date of period of interest
#' @param endDate - End date of period of interest
#' @param timeStep - AZMet data time step
#' @return `figureFooter` - Footer for figure based on user input


fxnFigureFooter <- function(azmetStation, startDate, endDate, timeStep) {
  # Inputs
  apiURL <- a(
    "api.azmet.arizona.edu", 
    href="https://api.azmet.arizona.edu/v1/observations/daily", # Daily data
    target="_blank"
  )
  
  azmetrURL <- a(
    "azmetr", 
    href="https://uace-azmet.github.io/azmetr/",
    target="_blank"
  )
  
  bulletinURL <- a(
    "AZ1602 'Heat Units'",
    href="https://extension.arizona.edu/sites/extension.arizona.edu/files/pubs/az1602.pdf",
    target="_blank"
  )
  
  todayDate <- gsub(" 0", " ", format(lubridate::today(), "%B %d, %Y"))
  
  todayYear <- lubridate::year(lubridate::today())
  
  webpageCode <- a(
    "GitHub page", 
    href="https://github.com/uace-azmet/cotton-growth-stages-and-heat-units", 
    target="_blank"
  )
  
  webpageDataVariables <- a(
    "data variables", 
    href="https://staging.azmet.arizona.edu/about/data-variables", 
    target="_blank"
  )
  
  webpageNetworkMap <- a(
    "station locations", 
    href="https://staging.azmet.arizona.edu/about/network-map", 
    target="_blank"
  )
  
  webpageStationMetadata <- a(
    "station metadata", 
    href="https://staging.azmet.arizona.edu/about/station-metadata", 
    target="_blank"
  )
  
  webpageAZMet <- a(
    "AZMet website", 
    href="https://staging.azmet.arizona.edu/", 
    target="_blank"
  )
  
  # Footer text
  if (endDate > as.Date(paste0(lubridate::year(Sys.Date()), "-06-15")) & azmetStation == "Yuma North Gila") {
    figureFooter <- 
      htmltools::p(
        htmltools::HTML(
          paste0(
            "Heat units are based on the single sine curve method with upper and lower temperature thresholds of 86 and 55 °F, respectively, and represent accumulations of daily values for the current growing season (dark blue bar in graph) from", " ", gsub(" 0", " ", format(startDate, "%B %d, %Y")), " ", "through", " ", gsub(" 0", " ", format(endDate, "%B %d, %Y")), ".", " ", "Accumulation values of heat units for past growing seasons (gray bars in graph) are based on the same planting and end dates, but during those respective years. More information on this method, as well as the relationship between heat units and cotton growth stages, is in Extension bulletin", " ", bulletinURL, ".", " ", "Cumulative heat unit data for the Yuma North Gila station are unavailable in 2021 after June 15.",  
            br(), br(), 
            timeStep, " ", "AZMet data are from", " ", apiURL, " and accessed using the ", azmetrURL, " R package. Values from recent dates may be based on provisional data", ".", " ", "More information about", " ", webpageDataVariables, ",", " ", webpageNetworkMap, ",", " ", "and", " ", webpageStationMetadata, " ", "is available on the", " ", webpageAZMet, ".", " ", "Users of AZMet data and data applications assume all risks of its use", ".",
            br(), br(),
            "To cite the above AZMet data, please use: 'Arizona Meteorological Network (", todayYear, ") Arizona Meteorological Network (AZMet) Data. https://azmet.arizona.edu. Accessed", " ", todayDate, "'.",
            br(), br(),
            "For information on how this webpage is put together, please visit the", " ", webpageCode, " ", "for this tool."
          )
        )
      )
  } else {
    figureFooter <- 
      htmltools::p(
        htmltools::HTML(
          paste0(
            "Heat units are based on the single sine curve method with upper and lower temperature thresholds of 86 and 55 °F, respectively, and represent accumulations of daily values for the current growing season (dark blue bar in graph) from", " ", gsub(" 0", " ", format(startDate, "%B %d, %Y")), " ", "through", " ", gsub(" 0", " ", format(endDate, "%B %d, %Y")), ".", " ", "Accumulation values of heat units for past growing seasons (gray bars in graph) are based on the same planting and end dates, but during those respective years. More information on this method, as well as the relationship between heat units and cotton growth stages, is in Extension bulletin", " ", bulletinURL, ".", " ",  
            br(), br(), 
            timeStep, " ", "AZMet data are from", " ", apiURL, " and accessed using the ", azmetrURL, " R package. Values from recent dates may be based on provisional data", ".", " ", "More information about", " ", webpageDataVariables, ",", " ", webpageNetworkMap, ",", " ", "and", " ", webpageStationMetadata, " ", "is available on the", " ", webpageAZMet, ".", " ", "Users of AZMet data and data applications assume all risks of its use", ".",
            br(), br(),
            "To cite the above AZMet data, please use: 'Arizona Meteorological Network (", todayYear, ") Arizona Meteorological Network (AZMet) Data. https://azmet.arizona.edu. Accessed", " ", todayDate, "'.",
            br(), br(),
            "For information on how this webpage is put together, please visit the", " ", webpageCode, " ", "for this tool."
          )
        )
      )
  }
  
  return(figureFooter)
}
