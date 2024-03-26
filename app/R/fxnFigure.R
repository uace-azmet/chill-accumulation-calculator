#' fxnFigure: generates bar chart of cumulative chill of current and recent years
#' 
#' @param inData - data table of seasonal chill accumulation values by year
#' @param azmetStation - AZMet station selection by user
#' @param startDate - Start date of period of interest
#' @param endDate - End date of period of interest
#' @param chillVariable - Chill variable selection by user
#' @return `figure` - png of figure


fxnFigure <- function(inData, azmetStation, startDate, endDate, chillVariable) {
  figure <- ggplot2::ggplot(
    data = inData, 
    mapping = aes(x = as.factor(.data$dateYearLabel), y = .data$chillSum)
  ) +
    
    geom_col( # Previous growing season
      data = dplyr::filter(inData, inData$dateYearLabel < max(inData$dateYearLabel)), 
      mapping = aes(x = as.factor(.data$dateYearLabel), y = .data$chillSum), 
      alpha = 1.0, fill = "#999999"
    ) +
    
    geom_col( # Current growing season
      data = dplyr::filter(inData, inData$dateYearLabel == max(inData$dateYearLabel)), 
      mapping = aes(x = as.factor(.data$dateYearLabel), y = .data$chillSum), 
      alpha = 1.0, fill = "#001C48"
    ) +
    
    geom_label( # Previous growing season
      data = dplyr::filter(inData, inData$dateYearLabel < max(inData$dateYearLabel)), 
      mapping = aes(label = .data$chillSumLabel, fontface = "bold"), 
      color = "#999999", fill = NA, label.size = NA, size = 3, vjust = 0.0
    ) +
    
    geom_label( # Current growing season
      data = dplyr::filter(inData, inData$dateYearLabel == max(inData$dateYearLabel)), 
      mapping = aes(label = .data$chillSumLabel, fontface = "bold"), 
      color = "#001C48", fill = NA, label.size = NA, size = 3, vjust = 0.0
    ) + 
    
    labs(x = "\nYear", y = paste0(chillVariable,"\n")) +
    
    scale_y_continuous(expand = expansion(mult = c(0.01, 0.05))) +
    
    theme_minimal() +
    
    theme( # https://ggplot2.tidyverse.org/reference/theme.html
      #line,
      #rect,
      text = element_text(family = "sans"),
      #title,
      #aspect.ratio,
      axis.title = element_text(
        color = "#343a40", face = "plain", size = 10, hjust = 0.0, 
        margin = margin(t = 0.2, r = 0, b = 0, l = 0, unit = "cm")
      ),
      #axis.title.x,
      #axis.title.x.top,
      #axis.title.x.bottom,
      #axis.title.y,
      #axis.title.y.left,
      #axis.title.y.right,
      axis.text = element_text(color = "#343a40", face = "plain", size = 10),
      #axis.text.x,
      #axis.text.x.top,
      #axis.text.x.bottom,
      #axis.text.y,
      #axis.text.y.left,
      #axis.text.y.right,
      #axis.ticks,
      #axis.ticks.x,
      #axis.ticks.x.top,
      #axis.ticks.x.bottom,
      #axis.ticks.y,
      #axis.ticks.y.left,
      #axis.ticks.y.right,
      #axis.ticks.length,
      #axis.ticks.length.x,
      #axis.ticks.length.x.top,
      #axis.ticks.length.x.bottom,
      #axis.ticks.length.y,
      #axis.ticks.length.y.left,
      #axis.ticks.length.y.right,
      #axis.line,
      #axis.line.x,
      #axis.line.x.top,
      #axis.line.x.bottom,
      #axis.line.y,
      #axis.line.y.left,
      #axis.line.y.right,
      #legend.background,
      #legend.margin,
      #legend.spacing,
      #legend.spacing.x,
      #legend.spacing.y,
      #legend.key,
      #legend.key.size,
      #legend.key.height,
      #legend.key.width,
      #legend.text,
      #legend.text.align,
      #legend.title,
      #legend.title.align,
      #legend.position,
      #legend.direction,
      #legend.justification,
      #legend.box,
      #legend.box.just,
      #legend.box.margin,
      #legend.box.background,
      #legend.box.spacing,
      #panel.background,
      #panel.border,
      #panel.spacing,
      #panel.spacing.x,
      #panel.spacing.y,
      #panel.grid,
      #panel.grid.major,
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = "#CCCCCC", linetype = "solid", linewidth = 0.3),
      #panel.grid.minor.x,
      #panel.grid.minor.y,
      #panel.ontop,
      #plot.background,
      #plot.title
      #plot.title.position
      #plot.subtitle
      #plot.caption,
      #plot.caption.position,
      #plot.tag,
      #plot.tag.position,
      #plot.margin,
      #strip.background,
      #strip.background.x,
      #strip.background.y,
      #strip.clip,
      #strip.placement,
      #strip.text,
      #strip.text.x,
      #strip.text.y,
      #strip.switch.pad.grid,
      #strip.switch.pad.wrap,
      #...,
      #complete = FALSE,
      #validate = TRUE
    )
  
  return(figure)
}
