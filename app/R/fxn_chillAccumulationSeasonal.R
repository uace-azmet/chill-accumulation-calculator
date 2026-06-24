#' `fxn_chillAccumulationSeasonal` - Calculates chill accumulation for an individual season
#' 
#' @param inData - Derived data table of daily values from `fxn_chillAccumulation.R`
#' @param azmetStation - AZMet station selection by user
#' @param startDate - Start date of period of interest
#' @param endDate - End date of period of interest
#' @param chillVariable - Chill variable selected by user
#' @param userDateRange - Date interval based on `startDate` and `endDate`
#' @return `chillAccumulationSeasonal` - Data table with chill accumulation for an individual season


fxn_chillAccumulationSeasonal <- 
  function(azmetStation, inData, startDate, endDate, chillVariable, userDateRange) {
    
    # For x-axis labels and related text of comparison to previous years
    if (lubridate::year(startDate) == lubridate::year(endDate)) { # For data request spanning a single calendar year
      dateYearLabel <- as.character(lubridate::year(startDate))
    } else { # For data request spanning two calendar years
      dateYearLabel <- 
        paste(lubridate::year(startDate), lubridate::year(endDate), sep = "-")
    }
    
    if (nrow(inData) == 0) { # For case of empty data return
      
      chillAccumulationSeasonal <- 
        data.frame(
          matrix(
            data = NA,
            nrow = 1, 
            ncol = 
              length(
                c(
                  "meta_station_name", 
                  "chill_accumulation_seasonal", 
                  "chill_accumulation_seasonal_label", 
                  "end_date_year", 
                  "date_year_label"
                )
              )
          )
        )
      
      colnames(chillAccumulationSeasonal) <- 
        c(
          "meta_station_name", 
          "chill_accumulation_seasonal", 
          "chill_accumulation_seasonal_label", 
          "end_date_year", 
          "date_year_label"
        )
      
      chillAccumulationSeasonal <- chillAccumulationSeasonal %>%
        dplyr::mutate(meta_station_name = azmetStation) %>%
        dplyr::mutate(chill_accumulation_seasonal = 0.00) %>%
        dplyr::mutate(chill_accumulation_seasonal_label = "NA") %>%
        dplyr::mutate(end_date_year = lubridate::year(endDate)) %>%
        dplyr::mutate(date_year_label = dateYearLabel)
      
    } else {
      if (chillVariable == "Chill Portions") {
        chillAccumulationSeasonal <- inData %>%
          dplyr::summarize(chill_accumulation_seasonal = sum(chill, na.rm = TRUE)) %>%
          # dplyr::rename(chill_accumulation_seasonal = chill_portions_total) %>%
          dplyr::mutate(chill_accumulation_seasonal_label = format(round(chill_accumulation_seasonal, digits = 1), nsmall = 1)) %>%
          dplyr::mutate(end_date_year = lubridate::year(endDate)) %>%
          dplyr::mutate(date_year_label = dateYearLabel)
      } else if (chillVariable == "Hours below 32 °F") {
        chillAccumulationSeasonal <- inData %>%
          dplyr::summarize(chill_accumulation_seasonal = sum(chill, na.rm = TRUE)) %>%
          # dplyr::rename(chill_accumulation_seasonal = chill_hours_32F_total) %>%
          dplyr::mutate(chill_accumulation_seasonal_label = format(round(chill_accumulation_seasonal, digits = 0), nsmall = 0)) %>%
          dplyr::mutate(end_date_year = lubridate::year(endDate)) %>%
          dplyr::mutate(date_year_label = dateYearLabel)
      } else if (chillVariable == "Hours below 45 °F") {
        chillAccumulationSeasonal <- inData %>%
          dplyr::summarize(chill_accumulation_seasonal = sum(chill, na.rm = TRUE)) %>%
          # dplyr::rename(chill_accumulation_seasonal = chill_hours_45F_total) %>%
          dplyr::mutate(chill_accumulation_seasonal_label = format(round(chill_accumulation_seasonal, digits = 0), nsmall = 0)) %>%
          dplyr::mutate(end_date_year = lubridate::year(endDate)) %>%
          dplyr::mutate(date_year_label = dateYearLabel)
      } else if (chillVariable == "Hours between 32 and 45 °F") {
        chillAccumulationSeasonal <- inData %>%
          dplyr::summarize(chill_accumulation_seasonal = sum(chill, na.rm = TRUE)) %>%
          # dplyr::rename(chill_accumulation_seasonal = chill_hours_3245F_total) %>%
          dplyr::mutate(chill_accumulation_seasonal_label = format(round(chill_accumulation_seasonal, digits = 0), nsmall = 0)) %>%
          dplyr::mutate(end_date_year = lubridate::year(endDate)) %>%
          dplyr::mutate(date_year_label = dateYearLabel)
      } else if (chillVariable == "Hours above 68 °F") {
        chillAccumulationSeasonal <- inData %>%
          dplyr::summarize(chill_accumulation_seasonal = sum(chill, na.rm = TRUE)) %>%
          # dplyr::rename(chill_accumulation_seasonal = chill_hours_68F_total) %>%
          dplyr::mutate(chill_accumulation_seasonal_label = format(round(chill_accumulation_seasonal, digits = 0), nsmall = 0)) %>%
          dplyr::mutate(end_date_year = lubridate::year(endDate)) %>%
          dplyr::mutate(date_year_label = dateYearLabel)
      } else if (chillVariable == "Utah Model") {
        inData <- inData %>% 
          dplyr::mutate(utah_model_accum = NA_real_)
        
        for (i in 1:length(inData$chill)) {
          if (i == 1) {
            if (inData$chill[i] < 0) {
              inData$utah_model_accum[i] <- 0
            } else {
              inData$utah_model_accum[i] <- inData$chill[i]
            }
          } else {
            if (inData$chill[i] + inData$utah_model_accum[i - 1] < 0) {
              inData$utah_model_accum[i] <- 0
            } else {
              inData$utah_model_accum[i] <- inData$chill[i] + inData$utah_model_accum[i - 1]
            }
          }
        }
        rm(i)
        
        chillAccumulationSeasonal <- inData %>%
          dplyr::summarize(utah_model_total = dplyr::last(utah_model_accum, na_rm = TRUE)) %>%
          dplyr::rename(chill_accumulation_seasonal = utah_model_total) %>%
          dplyr::mutate(chill_accumulation_seasonal_label = format(round(chill_accumulation_seasonal, digits = 1), nsmall = 1)) %>%
          dplyr::mutate(end_date_year = lubridate::year(endDate)) %>%
          dplyr::mutate(date_year_label = dateYearLabel)
      }
    }
    
    if (azmetStation == "Yuma N.Gila" & lubridate::int_overlaps(int1 = yugNodataInterval, int2 = userDateRange) == TRUE) {
      chillAccumulationSeasonal$chill_accumulation_seasonal <- NA_real_
      chillAccumulationSeasonal$chill_accumulation_seasonal_label <- "NA"
    }
    
    return(chillAccumulationSeasonal)
  }
