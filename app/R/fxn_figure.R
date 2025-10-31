#' `fxn_figure` generates bar chart of chill accumulation of current and recent years
#' 
#' @param inData - data table of seasonal chill accumulation values by year
#' @param azmetStation - user-specified AZMet station
#' @return `figure` - plotly figure

# https://plotly-r.com/ 
# https://plotly.com/r/reference/ 
# https://plotly.github.io/schema-viewer/
# https://github.com/plotly/plotly.js/blob/c1ef6911da054f3b16a7abe8fb2d56019988ba14/src/components/fx/hover.js#L1596
# https://www.color-hex.com/color-palette/1041718


fxn_figure <- function(inData, azmetStation) {
  
  # Inputs -----
  
  averageTotal <- mean(inData$chillTotal, na.rm = TRUE)
  
  dataCurrentYear <- inData %>% 
    dplyr::filter(endDateYear == max(endDateYear)) %>%
    dplyr::mutate(endDateYear = as.factor(endDateYear))
  
  dataOtherYears <- inData %>% 
    dplyr::filter(endDateYear != max(endDateYear)) %>% 
    dplyr::mutate(endDateYear = as.factor(endDateYear))
  
  layoutFontFamily <- "proxima-nova, calibri, -apple-system, BlinkMacSystemFont, \"Segoe UI\", Roboto, \"Helvetica Neue\", Arial, \"Noto Sans\", sans-serif, \"Apple Color Emoji\", \"Segoe UI Emoji\", \"Segoe UI Symbol\", \"Noto Color Emoji\""
  
  ticktext <- inData$dateYearLabel
  tickvals <- inData$endDateYear
  
  
  # Figure -----
  
  # For stations with only one year of data
  if (nrow(inData) == 1) {
    figure <- 
      plotly::plot_ly( # Bars for `dataOtherYears`
        data = dataOtherYears,
        x = ~endDateYear,
        y = ~chillTotal,
        marker = list(color = "#bfbfbf"),
        name = "other years",
        showlegend = FALSE,
        hoverinfo = "text",
        hovertext = ~paste0(
          "<br><b>AZMet Station:</b> ", azmetStation,
          "<br><b>Year:</b> ", dateYearLabel,
          "<br><b>Accumulation:</b> ", chillTotalLabel, " hours"
        ),
        type = "bar"
      ) %>% 
      
      plotly::add_trace( # Bar for `dataCurrentYear`
        inherit = FALSE,
        data = dataCurrentYear,
        x = ~endDateYear,
        y = ~chillTotal,
        marker = list(color = "#191919"),
        name = "current year",
        showlegend = FALSE,
        hoverinfo = "text",
        hovertext = ~paste0(
          "<br><b>AZMet Station:</b> ", azmetStation,
          "<br><b>Year:</b> ", dateYearLabel,
          "<br><b>Accumulation:</b> ", chillTotalLabel, " hours"
        ),
        type = "bar"
      ) %>%
      
      plotly::config(
        displaylogo = FALSE,
        displayModeBar = FALSE,
        modeBarButtonsToRemove = c(
          "autoScale2d",
          "hoverClosestCartesian", 
          "hoverCompareCartesian", 
          "lasso2d",
          "select"
        ),
        scrollZoom = FALSE,
        toImageButtonOptions = list(
          format = "png", # Either png, svg, jpeg, or webp
          filename = "AZMet-chill-accumulation-calculator",
          height = 400,
          width = 700,
          scale = 5
        )
      ) %>%
      
      plotly::layout(
        # annotations = list(
        #   align = "left",
        #   font = 
        #     list(
        #       color = "#3b3b3b",
        #       family = layoutFontFamily,
        #       size = 14
        #     ),
        #   showarrow = FALSE,
        #   text = paste("<b>Average: ", format(abs(round(averageTotal, digits = 2)), nsmall = 2), " hours</b>"),
        #   x = 0,
        #   xanchor = "left",
        #   xref = "paper",
        #   xshift = 12,
        #   y = averageTotal,
        #   yanchor = "bottom",
        #   yref = "y",
        #   yshift = 0
        # ),
        font = list(
          color = "#191919",
          family = layoutFontFamily,
          size = 13
        ),
        hoverlabel = list(
          font = list(
            family = layoutFontFamily,
            size = 14
          )
        ),
        margin = list(
          l = 0,
          r = 0, # For space between plot and modebar
          b = 0,
          t = 0,
          pad = 3 # For space between gridlines and yaxis labels
        ),
        modebar = list(
          bgcolor = "#FFFFFF",
          orientation = "v"
        ),
        # shapes =
        #   list(
        #     type = "line",
        #     layer = "above",
        #     line = list(
        #       color = "#3b3b3b", 
        #       dash = "solid",
        #       width = 1
        #     ),
        #     x0 = 0,
        #     x1 = 1,
        #     xref = "paper",
        #     y0 = averageTotal,
        #     y1 = averageTotal,
        #     yref = "y"
        #   ),
        xaxis = list(
          fixedrange = TRUE,
          linewidth = 0,
          ticktext = ticktext,
          tickvals = tickvals,
          title = list(
            font = list(size = 14),
            standoff = 25,
            text = "Year"
          ),
          zeroline = FALSE
        ),
        yaxis = list(
          fixedrange = TRUE,
          gridcolor = "#c9c9c9",
          title = list(
            font = list(size = 14),
            standoff = 25,
            text = "hours"
          ),
          zeroline = TRUE,
          zerolinecolor = "#c9c9c9"
        )
      )
  } else {
    figure <- 
      plotly::plot_ly( # Bars for `dataOtherYears`
        data = dataOtherYears,
        x = ~endDateYear,
        y = ~chillTotal,
        marker = list(color = "#bfbfbf"),
        name = "other years",
        showlegend = FALSE,
        hoverinfo = "text",
        hovertext = ~paste0(
          "<br><b>AZMet Station:</b> ", azmetStation,
          "<br><b>Year:</b> ", dateYearLabel,
          "<br><b>Accumulation:</b> ", chillTotalLabel, " hours"
        ),
        type = "bar"
      ) %>% 
      
      plotly::add_trace( # Bar for `dataCurrentYear`
        inherit = FALSE,
        data = dataCurrentYear,
        x = ~endDateYear,
        y = ~chillTotal,
        marker = list(color = "#191919"),
        name = "current year",
        showlegend = FALSE,
        hoverinfo = "text",
        hovertext = ~paste0(
          "<br><b>AZMet Station:</b> ", azmetStation,
          "<br><b>Year:</b> ", dateYearLabel,
          "<br><b>Accumulation:</b> ", chillTotalLabel, " hours"
        ),
        type = "bar"
      ) %>%
      
      plotly::config(
        displaylogo = FALSE,
        displayModeBar = FALSE,
        modeBarButtonsToRemove = c(
          "autoScale2d",
          "hoverClosestCartesian", 
          "hoverCompareCartesian", 
          "lasso2d",
          "select"
        ),
        scrollZoom = FALSE,
        toImageButtonOptions = list(
          format = "png", # Either png, svg, jpeg, or webp
          filename = "AZMet-chill-accumulation-calculator",
          height = 400,
          width = 700,
          scale = 5
        )
      ) %>%
      
      plotly::layout(
        annotations = list(
          align = "left",
          font = 
            list(
              color = "#3b3b3b",
              family = layoutFontFamily,
              size = 14
            ),
          showarrow = FALSE,
          text = paste("<b>Average: ", format(abs(round(averageTotal, digits = 2)), nsmall = 2), " hours</b>"),
          x = 0,
          xanchor = "left",
          xref = "paper",
          xshift = 12,
          y = averageTotal,
          yanchor = "bottom",
          yref = "y",
          yshift = 0
        ),
        font = list(
          color = "#191919",
          family = layoutFontFamily,
          size = 13
        ),
        hoverlabel = list(
          font = list(
            family = layoutFontFamily,
            size = 14
          )
        ),
        margin = list(
          l = 0,
          r = 0, # For space between plot and modebar
          b = 0,
          t = 0,
          pad = 3 # For space between gridlines and yaxis labels
        ),
        modebar = list(
          bgcolor = "#FFFFFF",
          orientation = "v"
        ),
        shapes =
          list(
            type = "line",
            layer = "above",
            line = list(
              color = "#3b3b3b", 
              dash = "solid",
              width = 1
            ),
            x0 = 0,
            x1 = 1,
            xref = "paper",
            y0 = averageTotal,
            y1 = averageTotal,
            yref = "y"
          ),
        xaxis = list(
          fixedrange = TRUE,
          linewidth = 0,
          ticktext = ticktext,
          tickvals = tickvals,
          title = list(
            font = list(size = 14),
            standoff = 25,
            text = "Year"
          ),
          zeroline = FALSE
        ),
        yaxis = list(
          fixedrange = TRUE,
          gridcolor = "#c9c9c9",
          title = list(
            font = list(size = 14),
            standoff = 25,
            text = "Hours"
          ),
          zeroline = TRUE,
          zerolinecolor = "#c9c9c9"
        )
      )
  }
  
  return(figure)
}
