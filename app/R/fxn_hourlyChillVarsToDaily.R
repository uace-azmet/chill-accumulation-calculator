#' `fxn_hourlyChillVarsToDaily.R` Compute chill values for variables dependent on hourly data
#' 
#' @param inData - returned output from `fxn_hourlyData.R`
#' @param azmetStation - user-specified AZMet station
#' @param startDate - Start date of period of interest
#' @param endDate - End date of period of interest
#' @return `hourlyChillVarsToDaily` - Tibble of daily values for chill variables dependent on hourly data


fxn_hourlyChillVarsToDaily <- function(inData, azmetStation, startDate, endDate) {
  
  azmetStationStartDate <- 
    dplyr::filter(
      azmetStationMetadata, 
      meta_station_name == azmetStation
    )$start_date
  
  # Account for multi-month absence of YUG data in 2021
  nonOperational <- 0
  
  if (azmetStation == "Yuma N.Gila") {
    nodataDateRange <-
      lubridate::interval(
        start = lubridate::date("2021-06-16"),
        end = lubridate::date("2021-10-21")
      )
    
    while (startDate >= azmetStationStartDate) {
      userDateRange <- lubridate::interval(start = startDate, end = endDate)
      
      if (lubridate::int_overlaps(int1 = nodataDateRange, int2 = userDateRange) == TRUE) {
        nonOperational <- 1
      }
      
      startDate <- min(seq(startDate, length = 2, by = "-1 year"))
      endDate <- min(seq(endDate, length = 2, by = "-1 year"))
    }
  }
  
  # Generate figure footer based on presence/absence of non-operational dates
  if (azmetStation == "Yuma N.Gila" & nonOperational == 1) {
    inData <- inData %>% 
      dplyr::filter(date_year != 2021)
  } else {
    inData <- inData
  }
  
  hourlyChillVarsToDaily <- as.data.frame(inData) %>% 
    dplyr::rename(
      Year = date_year,
      JDay = date_doy,
      Temp = temp_airC
    ) %>% 
    dplyr::mutate(
      Year = as.integer(Year),
      JDay = as.integer(JDay),
      Hour = lubridate::hour(date_datetime),
      Month = lubridate::month(date_datetime, label = FALSE),
      Day = as.numeric(lubridate::day(date_datetime))
    ) %>% 
    dplyr::select(Year, JDay, Hour, Temp, Month, Day) %>% 
    na.omit(object = .) %>%  # `chillR` does not handle NAs within next function
    chillR::daily_chill(
      hourtemps = .,
      running_mean = 0,
      models = list(Chill_Portions = Dynamic_Model),
      THourly = NULL
    ) %>% 
    # chillR::daily_chill(
    #   hourtemps = .,
    #   running_mean = 0,
    #   models = list(Chill_Portions = Dynamic_Model, Utah_Chill_Units = Utah_Model),
    #   THourly = NULL
    # ) %>% 
    magrittr::extract2("daily_chill") %>% 
    tibble::as_tibble() %>% 
    dplyr::rename(
      date_year = Year,
      chill_portions = Chill_Portions#,
      # utah_model = Utah_Chill_Units
    ) %>%
    # dplyr::rename(
    #   date_year = Year,
    #   chill_portions = Chill_Portions,
    #   utah_model = Utah_Chill_Units
    # ) %>%
    dplyr::mutate(
      datetime = as.Date(as.character(YYMMDD), "%Y%m%d"),
      date_year = lapply(date_year, as.character),
      date_doy = lubridate::yday(datetime),
      meta_station_name = azmetStation
    )
  
  return(hourlyChillVarsToDaily)
}
