#' `fxn_hourlyChillVarsToDaily.R` Compute chill values for variables dependent on hourly data
#' 
#' @param inData - returned output from `fxn_hourlyData.R`
#' @param azmetStation - user-specified AZMet station
#' @return `hourlyChillVarsToDaily` - Tibble of daily values for chill variables dependent on hourly data


fxn_hourlyChillVarsToDaily <- function(inData, azmetStation) {
  
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
      models = list(Chill_Portions = Dynamic_Model, Utah_Chill_Units = Utah_Model),
      THourly = NULL
    ) %>%
    magrittr::extract2("daily_chill") %>% 
    tibble::as_tibble() %>% 
    dplyr::rename(
      date_year = Year,
      chill_portions = Chill_Portions,
      utah_model = Utah_Chill_Units
    ) %>%
    dplyr::mutate(
      datetime = as.Date(as.character(YYMMDD), "%Y%m%d"),
      date_year = lapply(date_year, as.character),
      date_doy = lubridate::yday(datetime),
      meta_station_name = azmetStation
    )
  
  return(hourlyChillVarsToDaily)
}
