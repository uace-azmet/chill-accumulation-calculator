#' `fxn_navsetCardTimeSeries.R` Generate time series graph with daily data based on user input
#' 
#' @param inData - Data table [[1]] from `fxn_chillAccumulation.R`
#' @param startDate - Start date of period of interest
#' @param endDate - End date of period of interest
#' @param chillVariable - Chill variable selected by user

#' @return `navsetCardTimeSeries` - Time series graph with daily data based on user input

# https://plotly-r.com/ 
# https://plotly.com/r/reference/ 
# https://plotly.github.io/schema-viewer/
# https://github.com/plotly/plotly.js/blob/c1ef6911da054f3b16a7abe8fb2d56019988ba14/src/components/fx/hover.js#L1596
# https://www.color-hex.com/color-palette/1041718


fxn_navsetCardTimeSeries <- function(inData, startDate, endDate, chillVariable) {
  
  inData <- inData |>
    dplyr::mutate(datetime = lubridate::ymd(datetime))
  
  dataPreviousYears <- inData %>% 
    dplyr::filter(datetime < startDate) %>% 
    dplyr::group_by(date_year_label)
  
  dataCurrentYear <- inData %>% 
    dplyr::filter(datetime >= startDate)
  
  if (chillVariable == "Chill Portions") {
    axisVarUnits <- "portions"
    hoverTextVarUnits <- "portions"
  } else if (chillVariable == "Utah Model") {
    axisVarUnits <- "units"
    hoverTextVarUnits <- "units"
  } else { # "Hours below 32 °F", "Hours between 32 and 45 °F", "Hours below 45 °F", "Hours above 68 °F"
    axisVarUnits <- "hours"
    hoverTextVarUnits <- "hours"
  }
  
  if (chillVariable %in% c("Chill Portions", "Utah Model")) {
    digits <- 1
  } else { # "Hours below 32 °F", "Hours between 32 and 45 °F", "Hours below 45 °F", "Hours above 68 °F"
    digits <- 0
  }
  
  layoutFontFamily <- "proxima-nova, calibri, -apple-system, BlinkMacSystemFont, \"Segoe UI\", Roboto, \"Helvetica Neue\", Arial, \"Noto Sans\", sans-serif, \"Apple Color Emoji\", \"Segoe UI Emoji\", \"Segoe UI Symbol\", \"Noto Color Emoji\""
  
  
  # Time Series -----
  
  navsetCardTimeSeries <- 
    plotly::plot_ly( # Lines and points for `dataPreviousYears`
      data = dataPreviousYears,
      x = ~day_of_period,
      y = ~chill_acc,
      type = "scatter",
      mode = "lines+markers",
      #color = "rgba(201, 201, 201, 1.0)",
      marker = list(color = "rgba(201, 201, 201, 1.0)", size = 3),
      line = list(color = "rgba(201, 201, 201, 1.0)", width = 1),
      name = "previous years",
      hoverinfo = "text",
      text = 
        ~paste0(
          "<br><b>AZMet Station:</b> ", meta_station_name,
          "<br><b>Date:</b> ", gsub(" 0", " ", format(datetime, "%b %d, %Y")),
          "<br><b>Chill<sub>cumulative</sub>:</b> ", format(chill_acc, nsmall = digits), " ", hoverTextVarUnits
        ),
      showlegend = TRUE,
      legendgroup = "dataPreviousYears",
      legendrank = 2
    ) %>% 
    
    plotly::add_trace( # Lines and points for `dataCurrentYear`
      inherit = FALSE,
      data = dataCurrentYear,
      x = ~day_of_period,
      y = ~chill_acc,
      type = "scatter",
      mode = "lines+markers",
      #color = "#191919",
      marker = list(color = "#191919", size = 3),
      line = list(color = "#191919", width = 1.5),
      name = ~date_year_label,
      hoverinfo = "text",
      text = 
          ~paste0(
          "<br><b>AZMet Station:</b> ", meta_station_name,
          "<br><b>Date:</b> ", gsub(" 0", " ", format(datetime, "%b %d, %Y")),
          "<br><b>Chill<sub>cumulative</sub>:</b> ", format(chill_acc, nsmall = digits), " ", hoverTextVarUnits
        ),
      showlegend = TRUE,
      legendgroup = "dataCurrentYear",
      legendrank = 1
    ) %>% 
    
    plotly::config(
      displaylogo = FALSE,
      displayModeBar = TRUE,
      modeBarButtonsToRemove = 
        c(
          "autoScale2d",
          "hoverClosestCartesian", 
          "hoverCompareCartesian", 
          "lasso2d",
          "select"
        ),
      scrollZoom = FALSE,
      toImageButtonOptions = 
        list(
          format = "png", # Either png, svg, jpeg, or webp
          filename = "AZMet-chill-accumulation-calculator",
          height = 400,
          width = 700,
          scale = 5
        )
    ) %>%
    
    plotly::layout(
      font = list(color = "#191919", family = layoutFontFamily, size = 13),
      hoverlabel = list(font = list(family = layoutFontFamily, size = 14)),
      legend = 
        list(
          groupclick = "toggleitem",
          orientation = "h",
          traceorder = "normal",
          x = 0.00,
          xanchor = "left",
          xref = "container",
          # y = 1.05,
          # yanchor = "bottom",
          y = 1.0,
          yanchor = "top",
          yref = "container"
        ),
      margin = 
        list(
          l = 0,
          r = 50, # For space between plot and modebar
          b = 0, # For space between x-axis title and caption or figure help text
          t = 50,
          pad = 0
        ),
      modebar = list(bgcolor = "#FFFFFF", orientation = "v"),
      xaxis = 
        list(
          range = list(~(min(day_of_period) - 0.5), ~(max(day_of_period) + 1.5)),
          title = list(
            font = list(size = 14),
            standoff = 25,
            text = "<b>Day<sub>period</sub></b>"
          ),
          zeroline = FALSE
        ),
      yaxis = 
        list(
          title = list(
            font = list(size = 14),
            standoff = 25,
            text = paste0("<b>Chill<sub>cumulative</sub> (", axisVarUnits, ")</b>")
          ),
          zeroline = FALSE
        )
    )
  
  return(navsetCardTimeSeries)
}
