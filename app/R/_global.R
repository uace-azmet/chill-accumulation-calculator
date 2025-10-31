# Libraries --------------------


library(azmetr)
library(bsicons)
library(bslib)
library(dplyr)
library(htmltools)
library(lubridate)
library(plotly)
library(shiny)


# Files --------------------


# Functions. Loaded automatically at app start if in `R` folder
#source("./R/fxn_functionName.R", local = TRUE)

# Scripts. Loaded automatically at app start if in `R` folder
#source("./R/scr_scriptName.R", local = TRUE)


# Variables --------------------


azmetStationMetadata <- azmetr::station_info |>
  dplyr::mutate(end_date = NA) |> # Placeholder until inactive stations are in API and `azmetr`
  dplyr::mutate(
    end_date = dplyr::if_else(
      status == "active",
      lubridate::today(tzone = "America/Phoenix") - 1,
      end_date
    )
  ) |>
  dplyr::filter(!meta_station_name %in% c("Test"))

chillVariables <- 
  c(
    "Hours below 32 °F", 
    "Hours below 45 °F", 
    "Hours above 68 °F"
  )

# Derived (after data retrieved from station) variables
dailyVarsDerived <- 
  c(
    "chill_hours_0C",
    "chill_hours_20C",
    "chill_hours_32F",
    "chill_hours_45F",
    "chill_hours_68F",
    "chill_hours_7C"#,
    # "dwpt_mean", 
    # "dwpt_meanF", 
    # "eto_azmet",
    # "eto_azmet_in", 
    # "eto_pen_mon", 
    # "eto_pen_mon_in", 
    # "heat_units_10C", 
    # "heat_units_13C", 
    # "heat_units_3413C", 
    # "heat_units_45F", 
    # "heat_units_50F", 
    # "heat_units_55F", 
    # "heat_units_7C", 
    # "heat_units_9455F", 
    # "heatstress_cotton_meanC", 
    # "heatstress_cotton_meanF", 
    # "precip_total_in", 
    # "sol_rad_total_ly", 
    # "temp_air_maxF", 
    # "temp_air_meanF",
    # "temp_air_minF",
    # "temp_soil_10cm_maxF",
    # "temp_soil_10cm_meanF",
    # "temp_soil_10cm_minF",
    # "temp_soil_50cm_maxF",
    # "temp_soil_50cm_meanF",
    # "temp_soil_50cm_minF", 
    # "wind_2min_spd_max_mph", 
    # "wind_2min_spd_mean_mph", 
    # "wind_2min_timestamp", 
    # "wind_spd_max_mph", 
    # "wind_spd_mean_mph", 
    # "wind_vector_magnitude_mph"
  )

# Identification and date variables
dailyVarsID <- 
  c(
    "date_doy", 
    "date_year", 
    "datetime", 
    "meta_needs_review", 
    "meta_station_id", 
    "meta_station_name", 
    "meta_version"
  )

# Measured (or dervied at station datalogger) variables
dailyVarsMeasured <- 
  c(
    # "meta_bat_volt_max", 
    # "meta_bat_volt_mean", 
    # "meta_bat_volt_min", 
    # "precip_total_mm", 
    # "relative_humidity_max", 
    # "relative_humidity_mean", 
    # "relative_humidity_min", 
    # "sol_rad_total", 
    # "temp_air_maxC", 
    # "temp_air_meanC", 
    # "temp_air_minC", 
    # "temp_soil_10cm_maxC", 
    # "temp_soil_10cm_meanC",  
    # "temp_soil_10cm_minC", 
    # "temp_soil_50cm_maxC", 
    # "temp_soil_50cm_meanC", 
    # "temp_soil_50cm_minC", 
    # "vp_actual_max", 
    # "vp_actual_mean", 
    # "vp_actual_min", 
    # "vp_deficit_mean", 
    # "wind_2min_spd_max_mps", 
    # "wind_2min_spd_mean_mps", 
    # "wind_2min_timestamp", 
    # "wind_2min_vector_dir", 
    # "wind_spd_max_mps", 
    # "wind_spd_mean_mps", 
    # "wind_vector_dir", 
    # "wind_vector_dir_stand_dev", 
    # "wind_vector_magnitude"
  )

activeStations <-
  dplyr::filter(
    azmetStationMetadata,
    status == "active"
  )

if (Sys.Date() <= as.Date(paste0(lubridate::year(Sys.Date()), "-09-01"))) {
  initialStartDate <- as.Date(paste0((lubridate::year(Sys.Date()) - 1), "-09-01"))
} else {
  initialStartDate <- as.Date(paste0(lubridate::year(Sys.Date()), "-09-01"))
}

initialStation <-
  dplyr::filter(
    activeStations,
    meta_station_name == azmetStationMetadata[order(azmetStationMetadata$meta_station_name), ]$meta_station_name[1]
  )$meta_station_name

initialStationStartDate <- dplyr::filter(activeStations, meta_station_name == initialStation)$start_date

if (initialStationStartDate > Sys.Date() - lubridate::years(1)) {
  initialStartDateMinimum <- initialStationStartDate
} else {
  initialStartDateMinimum <- Sys.Date() - lubridate::years(1)
}
