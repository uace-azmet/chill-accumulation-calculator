#' `fxn_navsetCardTabSummary.R` - Build summary of chill accumulation value based on user input
#' 
#' @param azmetStation - AZMet station selection by user
#' @param inData - data table of seasonal chill accumulation by year
#' @param startDate - Start date of period of interest
#' @param endDate - End date of period of interest
#' @param chillVariable - Chill variable selected by user
#' @return `navsetCardTabSummary` - Summary of chill accumulation value based on user input


fxn_navsetCardTabSummary <- function(azmetStation, inData, startDate, endDate, chillVariable) {
  
  currentYear <- lubridate::year(endDate)
  currentYearTotal <- 
    dplyr::filter(inData, end_date_year == currentYear) %>% 
    dplyr::pull(chill_accumulation_seasonal)
  
  if (azmetStation == "Yuma N.Gila") {
    yugNodataOverlapPreviousYear <- 
      lubridate::int_overlaps(
        int1 = yugNodataInterval, 
        int2 = lubridate::interval(
          start = startDate - lubridate::years(1), 
          end = endDate - lubridate::years(1)
        )
      )
  } else {
    yugNodataOverlapPreviousYear <- FALSE
  }
  
  if (chillVariable == "Chill Portions") {
    variableUnits <- "portions"
  } else if (chillVariable == "Utah Model") {
    variableUnits <- "units"
  } else { # "Hours below 32 °F", "Hours between 32 and 45 °F", "Hours below 45 °F", "Hours above 68 °F"
    variableUnits <- "hours"
  }
  
  if (nrow(inData) == 1) { # For stations with only one year of data
    if (chillVariable %in% c("Chill Portions", "Utah Model")) {
      navsetCardTabSummary <- 
        htmltools::p(
          htmltools::HTML(
            paste0(
              "Chill accumulation at the AZMet ", azmetStation, " station from ", gsub(" 0", " ", format(startDate, "%B %d, %Y")), " through ", gsub(" 0", " ", format(endDate, "%B %d, %Y")), " is ", "<b>", format(round(currentYearTotal, digits = 1), nsmall = 1), " ", variableUnits, "</b>."
            ),
          ),
          
          class = "navset-card-tab-summary"
        )
    } else { # "Hours below 32 °F", "Hours between 32 and 45 °F", "Hours below 45 °F", "Hours above 68 °F"
      navsetCardTabSummary <- 
        htmltools::p(
          htmltools::HTML(
            paste0(
              "Chill accumulation at the AZMet ", azmetStation, " station from ", gsub(" 0", " ", format(startDate, "%B %d, %Y")), " through ", gsub(" 0", " ", format(endDate, "%B %d, %Y")), " is ", "<b>", format(round(currentYearTotal, digits = 0), nsmall = 0), " ", variableUnits, "</b>."
            ),
          ),
          
          class = "navset-card-tab-summary"
        )
    }
  } else { # For stations with more than one year of data
    averageTotal <- round(mean(inData$chill_accumulation_seasonal, na.rm = TRUE), digits = 1)
    
    previousYear <- currentYear - 1
    previousYearText <- 
      dplyr::filter(inData, end_date_year == previousYear) %>% 
      dplyr::pull(date_year_label)
    previousYearTotal <- 
      round(
        dplyr::filter(inData, end_date_year == previousYear) %>% 
          dplyr::pull(chill_accumulation_seasonal), 
        digits = 1
      )
    
    differenceAverage <- currentYearTotal - averageTotal
    differencePreviousYear <- currentYearTotal - previousYearTotal
    
    if (differenceAverage > 0) {
      differenceAverageText <- 
        paste0(
          format(abs(round(differenceAverage, digits = 1)), nsmall = 1), " ", variableUnits, " more than"
        )
    } else if (differenceAverage < 0) {
      differenceAverageText <- 
        paste0(
          format(abs(round(differenceAverage, digits = 1)), nsmall = 1), " ", variableUnits, " less than"
        )
    } else { # if (differenceAverage = 0)
      differenceAverageText <- "equal to"
    }
    
    if (differencePreviousYear == 0) {
      differencePreviousYearText <- "the same as"
    } else if (differencePreviousYear > 0) {
      if (chillVariable %in% c("Chill Portions", "Utah Model")) {
        differencePreviousYearText <- 
          paste0(
            format(abs(round(differencePreviousYear, digits = 1)), nsmall = 1), " ", variableUnits, " more than"
          )
      } else { # "Hours below 32 °F", "Hours between 32 and 45 °F", "Hours below 45 °F", "Hours above 68 °F"
        differencePreviousYearText <- 
          paste0(
            format(abs(round(differencePreviousYear, digits = 0)), nsmall = 0), " ", variableUnits, " more than"
          )
      }
    } else { # if (differencePreviousYear < 0)
      if (chillVariable %in% c("Chill Portions", "Utah Model")) {
        differencePreviousYearText <- 
          paste0(
            format(abs(round(differencePreviousYear, digits = 1)), nsmall = 1), " ", variableUnits, " less than"
          )
      } else { # "Hours below 32 °F", "Hours between 32 and 45 °F", "Hours below 45 °F", "Hours above 68 °F"
        differencePreviousYearText <- 
          paste0(
            format(abs(round(differencePreviousYear, digits = 0)), nsmall = 0), " ", variableUnits, " less than"
          )
      }
    }
    
    if (chillVariable %in% c("Chill Portions", "Utah Model")) {
      navsetCardTabSummary <- 
        htmltools::p(
          htmltools::HTML(
            paste0(
              "Chill accumulation at the AZMet ", azmetStation, " station from ", gsub(" 0", " ", format(startDate, "%B %d, %Y")), " through ", gsub(" 0", " ", format(endDate, "%B %d, %Y")), " is ", "<b>", format(round(currentYearTotal, digits = 1), nsmall = 1), " ", variableUnits, "</b>. This is ", differencePreviousYearText, " the accumulation during this same month-day period in ", previousYearText, ", and ", differenceAverageText, " the station average."
            ),
          ),
          
          class = "navset-card-tab-summary"
        )
    } else { # "Hours below 32 °F", "Hours between 32 and 45 °F", "Hours below 45 °F", "Hours above 68 °F"
      navsetCardTabSummary <- 
        htmltools::p(
          htmltools::HTML(
            paste0(
              "Chill accumulation at the AZMet ", azmetStation, " station from ", gsub(" 0", " ", format(startDate, "%B %d, %Y")), " through ", gsub(" 0", " ", format(endDate, "%B %d, %Y")), " is ", "<b>", format(round(currentYearTotal, digits = 0), nsmall = 0), " ", variableUnits, "</b>. This is ", differencePreviousYearText, " the accumulation during this same month-day period in ", previousYearText, ", and ", differenceAverageText, " the station average."
            ),
          ),
          
          class = "navset-card-tab-summary"
        )
    }
  }
  
  return(navsetCardTabSummary)
}
