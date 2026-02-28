#' `fxn_chillTotal` - calculates chill accumulation for an individual season
#' 
#' @param inData - returned output from `fxn_dailyData.R`
#' @param azmetStation - AZMet station selection by user
#' @param startDate - Start date of period of interest
#' @param endDate - End date of period of interest
#' @param chillVariable - Chill variable selected by user
#' @return `chillTotal` - Data table with chill accumulation for an individual season


fxn_chillTotal <- function(inData, azmetStation, startDate, endDate, chillVariable) {
  
  # For x-axis labels and related text of comparison to previous years
  if (lubridate::year(startDate) == lubridate::year(endDate)) { # For data request spanning a single calendar year
    dateYearLabel <- as.character(lubridate::year(startDate))
  } else { # For data request spanning two calendar years
    dateYearLabel <- 
      paste(
        lubridate::year(startDate), 
        lubridate::year(endDate), 
        sep = "-"
      )
  }
  
  if (nrow(inData) == 0) { # For case of empty data return
    chillTotal <- data.frame(matrix(
      data = NA,
      nrow = 1, 
      ncol = length(c("meta_station_name", "chillTotal", "chillTotalLabel", "endDateYear", "dateYearLabel"))
    ))
    
    colnames(chillTotal) <- 
      c("meta_station_name", "chillTotal", "chillTotalLabel", "endDateYear", "dateYearLabel")
    
    chillTotal <- chillTotal %>%
      dplyr::mutate(meta_station_name = azmetStation) %>%
      dplyr::mutate(chillTotal = 0.00) %>%
      dplyr::mutate(chillTotalLabel = "NA") %>%
      dplyr::mutate(endDateYear = lubridate::year(endDate)) %>%
      dplyr::mutate(dateYearLabel = dateYearLabel)
  } else {
    if (chillVariable == "Chill Portions") {
      chillTotal <- inData %>%
        # dplyr::group_by(meta_station_name) %>%
        dplyr::summarize(chill_portions_total = sum(chill_portions, na.rm = TRUE)) %>%
        dplyr::rename(chillTotal = chill_portions_total) %>%
        dplyr::mutate(chillTotalLabel = format(round(chillTotal, digits = 1), nsmall = 1)) %>%
        dplyr::mutate(endDateYear = lubridate::year(endDate)) %>%
        dplyr::mutate(dateYearLabel = dateYearLabel)
    } else if (chillVariable == "Hours below 32 °F") {
      chillTotal <- inData %>%
        # dplyr::group_by(meta_station_name) %>%
        dplyr::summarize(chill_hours_32F_total = sum(chill_hours_32F, na.rm = TRUE)) %>%
        dplyr::rename(chillTotal = chill_hours_32F_total) %>%
        dplyr::mutate(chillTotalLabel = format(round(chillTotal, digits = 0), nsmall = 0)) %>%
        dplyr::mutate(endDateYear = lubridate::year(endDate)) %>%
        dplyr::mutate(dateYearLabel = dateYearLabel)
    } else if (chillVariable == "Hours below 45 °F") {
      chillTotal <- inData %>%
        # dplyr::group_by(meta_station_name) %>%
        dplyr::summarize(chill_hours_45F_total = sum(chill_hours_45F, na.rm = TRUE)) %>%
        dplyr::rename(chillTotal = chill_hours_45F_total) %>%
        dplyr::mutate(chillTotalLabel = format(round(chillTotal, digits = 0), nsmall = 0)) %>%
        dplyr::mutate(endDateYear = lubridate::year(endDate)) %>%
        dplyr::mutate(dateYearLabel = dateYearLabel)
    } else if (chillVariable == "Hours between 32 and 45 °F") {
      chillTotal <- inData %>%
        # dplyr::group_by(meta_station_name) %>%
        dplyr::summarize(chill_hours_3245F_total = sum(chill_hours_3245F, na.rm = TRUE)) %>%
        dplyr::rename(chillTotal = chill_hours_3245F_total) %>%
        dplyr::mutate(chillTotalLabel = format(round(chillTotal, digits = 0), nsmall = 0)) %>%
        dplyr::mutate(endDateYear = lubridate::year(endDate)) %>%
        dplyr::mutate(dateYearLabel = dateYearLabel)
    } else if (chillVariable == "Hours above 68 °F") {
      chillTotal <- inData %>%
        # dplyr::group_by(meta_station_name) %>%
        dplyr::summarize(chill_hours_68F_total = sum(chill_hours_68F, na.rm = TRUE)) %>%
        dplyr::rename(chillTotal = chill_hours_68F_total) %>%
        dplyr::mutate(chillTotalLabel = format(round(chillTotal, digits = 0), nsmall = 0)) %>%
        dplyr::mutate(endDateYear = lubridate::year(endDate)) %>%
        dplyr::mutate(dateYearLabel = dateYearLabel)
    } else if (chillVariable == "Utah Model") {
      chillTotal <- inData %>%
        # dplyr::group_by(meta_station_name) %>%
        dplyr::summarize(utah_model_total = sum(utah_model, na.rm = TRUE)) %>%
        dplyr::rename(chillTotal = utah_model_total) %>%
        dplyr::mutate(chillTotalLabel = format(round(chillTotal, digits = 1), nsmall = 1)) %>%
        dplyr::mutate(endDateYear = lubridate::year(endDate)) %>%
        dplyr::mutate(dateYearLabel = dateYearLabel)
    }
  }
  
  return(chillTotal)
}



my_tibble <- 
  tibble::tibble(
    utah_model = c(-3, -2, -1, 0, -1, -2, -3, 0, 1, 2, 1, 0, -1, -2, -3, -2, -1, 0, 1, 2, 3, 4, 5, 6)
  )

ind_UtahModelStart <- purrr::detect_index(my_tibble$utah_model, ~ . > 0)

my_tibble <- my_tibble %>% 
  dplyr::mutate(
    row_number = dplyr::row_number(),
    utah_model_transform1 = dplyr::if_else(
      row_number < ind_UtahModelStart,
      0,
      utah_model
    )
  ) %>% 
  dplyr::rowwise(
    dplyr::mutate(
      utah_model_transform2 = dplyr::if_else(
        dplyr::row_number() == 1,
        utah_model_transform1,
        dplyr::if_else(
          sum(c(1,2,3,4))
        )
      )
    )
  )




# utah_model_transform2 = dplyr::rowwise(
#   dplyr::if_else(
#     sum(utah_model_transform1) > 0,
#     sum(utah_model_transform1),
#     -9999.0
#   )
# )
# utah_model_transform2 = dplyr::rowwise(
#   dplyr::if_else(
#     utah_model_transform1 <= 0,
#     0,
#     cumsum(utah_model_transform1)
#   ) 
# )


# dplyr::rowwise(
#   
# )
# dplyr::mutate(
#   row_number = dplyr::row_number(),
#   utah_model_transform1 = dplyr::if_else(
#     row_number < ind_UtahModelStart,
#     0,
#     utah_model
#   ),
#   utah_model_transform2 = cumsum(utah_model_transform1),
#   utah_model_transform3 = dplyr::if_else(
#     utah_model_transform2 < 0,
#     0,
#     utah_model_transform2
#   ),
# utah_model_total = cumsum(utah_model_transform3) # wrong


