#' `fxn_totalEvapotranspirationSeasonal` - Calculates seasonal total evapotranspiration for an individual year
#' 
#' @param azmetStation - AZMet station selection by user
#' @param inData - Derived data table of daily values from `fxn_totalEvapotranspiration.R`
#' @param startDate - Start date of period of interest
#' @param endDate - End date of period of interest
#' @param userDateRange - date interval based on `startDate` and `endDate`
#' @return `totalEvapotranspirationSeasonal` - Data table with total evapotranspiration for a single season of an individual year


fxn_chillAccumulationSeasonal <- function(azmetStation, inData, startDate, endDate, userDateRange) {
  
  chillAccumulationSeasonal <- inData %>%
    dplyr::group_by(meta_station_name) %>%
    dplyr::summarize(chill_accumulation_seasonal = sum(chill_variable, na.rm = TRUE))
  
  
  if (etEquation == "Original AZMet") {
    chillAccumulationSeasonal <- inData %>%
      dplyr::group_by(meta_station_name) %>%
      dplyr::summarize(eto_azmet_in_total = sum(eto_azmet_in, na.rm = TRUE)) %>%
      dplyr::rename(total_evapotranspiration_seasonal = eto_azmet_in_total)
  } else { # etEquation == "Penman-Monteith"
    totalEvapotranspirationSeasonal <- inData %>%
      dplyr::group_by(meta_station_name) %>%
      dplyr::summarize(eto_pen_mon_in_total = sum(eto_pen_mon_in, na.rm = TRUE)) %>%
      dplyr::rename(total_evapotranspiration_seasonal = eto_pen_mon_in_total)
  }
  
  totalEvapotranspirationSeasonal <- totalEvapotranspirationSeasonal %>% 
    dplyr::mutate(
      total_evapotranspiration_seasonal_label = 
        format(round(total_evapotranspiration_seasonal, digits = 2), nsmall = 2)
    ) %>% 
    dplyr::mutate(end_date_year = lubridate::year(endDate)) %>%
    dplyr::mutate(
      date_year_label = 
        dplyr::if_else(
          condition = lubridate::year(startDate) == lubridate::year(endDate),
          true = as.character(lubridate::year(startDate)),
          false = paste(lubridate::year(startDate), lubridate::year(endDate), sep = "-")
        )
    )
  
  if (azmetStation == "Yuma N.Gila" & lubridate::int_overlaps(int1 = yugNodataInterval, int2 = userDateRange) == TRUE) {
    chillAccumulationSeasonal$total_evapotranspiration_seasonal <- NA_real_
    chillAccumulationSeasonal$total_evapotranspiration_seasonal_label <- "NA"
  }
  
  return(chillAccumulationSeasonal)
}
