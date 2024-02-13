# Load auxiliary files
stationNames <- vroom::vroom(
  file = "aux-files/azmet-stations-api-db.csv", 
  delim = ",", 
  col_names = TRUE, 
  show_col_types = FALSE
)

# Set auxiliary variables
chillVariables <- c("Hours below 32 °F", "Hours below 45 °F", "Hours above 68 °F")

if (Sys.Date() <= as.Date(paste0(lubridate::year(Sys.Date()), "-09-01"))) {
  initialStartDate <- as.Date(paste0((lubridate::year(Sys.Date()) - 1), "-09-01"))
} else {
  initialStartDate <- as.Date(paste0(lubridate::year(Sys.Date()), "-09-01"))
}

initialEndDate <- (Sys.Date() - 1)
