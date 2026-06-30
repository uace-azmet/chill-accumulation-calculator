#' `fxn_chillAccumulation` - Calculates chill accumulation by day and season for period of interest and individual years
#' 
#' @param azmetStation - AZMet station selection by user
#' @param startDate - Start date of period of interest
#' @param endDate - End date of period of interest
#' @param chillVariable - Chill variable selected by user
#' @return `chillAccumulation` - List of daily [[1]] and seasonal [[2]] data tables of values for individual years


fxn_chillAccumulation <- function(azmetStation, startDate, endDate, chillVariable) {
  
  azmetStationStartDate <- 
    dplyr::filter(azmetStationMetadata, meta_station_name == azmetStation) %>% 
    dplyr::pull(start_date)
    
  
  # Data download, hourly to daily transform -----
  
  startDateDownload <- startDate
  endDateDownload <- endDate
  
  while (startDateDownload >= azmetStationStartDate) {
    if (chillVariable %in% c("Chill Portions", "Utah Model")) {
      azHourly <-  
        fxn_azHourly(
          azmetStation = azmetStation,
          startDate = startDateDownload, # To call API by individual season
          endDate = endDateDownload
        )
      azDaily <- azHourly %>% 
        fxn_hourlyChillVarsToDaily(inData = ., azmetStation = azmetStation)
    } else { # chillVariable %in% c("Hours below 32 °F", "Hours between 32 and 45 °F", "Hours below 45 °F", "Hours above 68 °F")
      azDaily <- 
        fxn_azDaily(
          azmetStation = azmetStation,
          startDate = startDateDownload, # To call API by individual season
          endDate = endDateDownload
        )
    }
    
    if (exists("azDailySeasons") == FALSE) {
      azDailySeasons <- azDaily
    } else {
      azDailySeasons <- rbind(azDailySeasons, azDaily)
    }
    
    startDateDownload <- 
      min(seq(lubridate::date(startDateDownload), length = 2, by = "-1 year"))
    
    endDateDownload <- 
      min(seq(lubridate::date(endDateDownload), length = 2, by = "-1 year"))
  }
  
  
  # Data variable transform -----
  
  if (chillVariable == "Hours between 32 and 45 °F") {
    azDailySeasons <- azDailySeasons %>%
      dplyr::mutate(chill_hours_3245F = chill_hours_45F - chill_hours_32F)
  } else {
    azDailySeasons <- azDailySeasons
  }
  
  if (chillVariable == "Chill Portions") {
    azDailySeasons <- azDailySeasons %>% 
      dplyr::rename(chill = chill_portions)
  } else if (chillVariable == "Hours below 32 °F") {
    azDailySeasons <- azDailySeasons %>% 
      dplyr::rename(chill = chill_hours_32F)
  } else if (chillVariable == "Hours below 45 °F") {
    azDailySeasons <- azDailySeasons %>% 
      dplyr::rename(chill = chill_hours_45F)
  } else if (chillVariable == "Hours above 68 °F") {
    azDailySeasons <- azDailySeasons %>% 
      dplyr::rename(chill = chill_hours_68F)
  } else if (chillVariable == "Hours between 32 and 45 °F") {
    azDailySeasons <- azDailySeasons %>% 
      dplyr::rename(chill = chill_hours_3245F)
  } else if (chillVariable == "Utah Model") {
    azDailySeasons <- azDailySeasons %>% 
      dplyr::rename(chill = utah_model)
  }
  
  azDailySeasons <- azDailySeasons %>% 
    dplyr::select(dplyr::all_of(c("datetime", "meta_station_name", "chill")))
  
  # Calculate accumulation by individual year
  while (startDate >= azmetStationStartDate) {
    
    userDateRange <- lubridate::interval(start = startDate, end = endDate)
    
    if (azmetStation == "Yuma N.Gila" & startDate %within% yugNodataInterval & endDate %within% yugNodataInterval) {
      # Handle empty daily data table at YUG
      singleYearDaily <-
        tibble::tibble( 
          datetime = seq(lubridate::ymd(startDate), lubridate::ymd(endDate), by = "days"),
          meta_station_name = azmetStation,
          chill = NA_real_,
          chill_acc = NA_real_
        )
    } else {
      singleYearDaily <- 
        dplyr::filter(azDailySeasons, datetime >= startDate & datetime <= endDate)
    }
    
    if (chillVariable != "Utah Model") {
      singleYearDaily <- singleYearDaily %>% 
        dplyr::mutate(
          chill_acc = 
            dplyr::if_else(
              condition = is.na(chill),
              true = NA_real_,
              false = 
                round((cumsum(tidyr::replace_na(chill, 0))), digits = 1)
            )
        )
    } else if (chillVariable == "Utah Model") {
      singleYearDaily <- singleYearDaily %>% 
        dplyr::mutate(chill_acc = NA_real_)
      
      for (i in 1:nrow(singleYearDaily)) {
        if (i == 1) {
          if (singleYearDaily$chill[i] < 0) {
            singleYearDaily$chill_acc[i] <- 0
          } else {
            singleYearDaily$chill_acc[i] <- singleYearDaily$chill[i]
          }
        } else {
          if (singleYearDaily$chill[i] + singleYearDaily$chill_acc[i - 1] < 0) {
            singleYearDaily$chill_acc[i] <- 0
          } else {
            singleYearDaily$chill_acc[i] <-
              singleYearDaily$chill[i] + singleYearDaily$chill_acc[i - 1]
          }
        }
      }
    }
    
    singleYearDaily <- singleYearDaily %>%
      dplyr::mutate(
        date_year_label =
          dplyr::if_else(
            condition = lubridate::year(startDate) == lubridate::year(endDate),
            true = as.character(lubridate::year(startDate)),
            false = paste(lubridate::year(startDate), lubridate::year(endDate), sep = "-")
          ),
        day_of_period = dplyr::row_number()
      )
    
    if (azmetStation == "Yuma N.Gila" & lubridate::int_overlaps(int1 = yugNodataInterval, int2 = userDateRange) == TRUE) {
      # Handle partially empty or empty daily data table at YUG
      singleYearDaily <- singleYearDaily %>%
        dplyr::mutate(
          chill_acc =
            dplyr::if_else(
              condition = datetime < yugNodataStartDate,
              true = chill_acc,
              false = NA_real_
            )
        )
    }
    
    # With `singleYearDaily` transformed, calculate seasonal totals
    singleYearSeasonal <-
      fxn_chillAccumulationSeasonal(
        azmetStation = azmetStation,
        inData = singleYearDaily,
        startDate = startDate,
        endDate = endDate,
        chillVariable = chillVariable,
        userDateRange = userDateRange
      )
    
    # Build data tables for return
    if (exists("dailyAccumulations") == FALSE) {
      dailyAccumulations <- singleYearDaily
    } else {
      dailyAccumulations <- rbind(dailyAccumulations, singleYearDaily)
    }
    
    if (exists("seasonalAccumulations") == FALSE) {
      seasonalAccumulations <- singleYearSeasonal
    } else {
      seasonalAccumulations <- rbind(seasonalAccumulations, singleYearSeasonal)
    }
    
    # Setup for analysis of data from previous year
    startDate <- min(seq(lubridate::date(startDate), length = 2, by = "-1 year"))
    endDate <- min(seq(lubridate::date(endDate), length = 2, by = "-1 year"))
  }
  
  return(list(dailyAccumulations, seasonalAccumulations))
}
