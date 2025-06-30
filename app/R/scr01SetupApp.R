# Load auxiliary files
azmetStations <- vroom::vroom(
  file = "aux-files/azmet-stations-api-db.csv", 
  delim = ",", 
  col_names = TRUE, 
  show_col_types = FALSE
)

# Omit for now, as previous years are not complete and conditional statements to handle this are not in place
azmetStations <- azmetStations |>
  dplyr::filter(stationName != "Mohave ETo") |>
  dplyr::filter(stationName != "Wellton ETo") |>
  dplyr::filter(stationName != "Yuma Valley ETo") |>
  dplyr::filter(stationName != "Chino Valley") |>
  dplyr::filter(stationName != "Elgin")

# Set auxiliary variables
apiStartDate <- as.Date("2021-01-01")

chillVariables <- c("Hours below 32 °F", "Hours below 45 °F", "Hours above 68 °F")

if (Sys.Date() <= as.Date(paste0(lubridate::year(Sys.Date()), "-09-01"))) {
  initialStartDate <- as.Date(paste0((lubridate::year(Sys.Date()) - 1), "-09-01"))
} else {
  initialStartDate <- as.Date(paste0(lubridate::year(Sys.Date()), "-09-01"))
}

initialEndDate <- Sys.Date() - 1
