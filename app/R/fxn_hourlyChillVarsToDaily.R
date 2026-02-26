#' `fxn_hourlyChillVarsToDaily.R` Compute chill values for variables dependent on hourly data
#' 
#' @param inData - returned output from `fxn_hourlyData.R`
#' @return `hourlyChillVarsToDaily` - Tibble of daily values for chill variables dependent on hourly data


fxn_hourlyChillVarsToDaily <- function(inData) {
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
      date_doy = lubridate::yday(datetime)
    ) #%>% 
    # dplyr::group_by(winter) %>% 
    # dplyr::mutate(
    #   winter_day = dplyr::row_number(),
    #   chill_portions_sum = cumsum(chill_portions)
    # ) %>% 
    # dplyr::ungroup()
  
  return(hourlyChillVarsToDaily)
}
