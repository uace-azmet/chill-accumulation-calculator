#' fxnFigure: generates bar chart of cumulative heat units of current and recent years with cotton growth stage labels
#' 
#' @param inData - data table of seasonal heat accumulation values by year
#' @param azmetStation - AZMet station selection by user
#' @param startDate - Planting date of period of interest
#' @param endDate - End date of period of interest
#' @return `figure` - png of figure
#' 


fxnFigure <- function(inData, azmetStation, startDate, endDate) {
  figure <- ggplot2::ggplot(
    data = inData, 
    mapping = aes(x = as.factor(.data$date_year), y = .data$heat_units_55F_cumulative)
  ) +
    
    # Cotton growth stages as cumulative heat units (https://www.color-hex.com/color/CCCCCC)
    geom_rect(xmin = -Inf, xmax = Inf, ymin = 1800, ymax = 2200, alpha = 0.1, fill = "#CCCCCC") + # Peak Bloom
    geom_rect(xmin = -Inf, xmax = Inf, ymin = 2400, ymax = 2800, alpha = 0.1, fill = "#CCCCCC") + # Cutout
    geom_rect(xmin = -Inf, xmax = Inf, ymin = 3000, ymax = 3400, alpha = 0.1, fill = "#CCCCCC") + # Terminate
    
    geom_col( # Previous growing season
      data = dplyr::filter(inData, inData$date_year < max(inData$date_year)), 
      mapping = aes(x = as.factor(.data$date_year), y = .data$heat_units_55F_cumulative), 
      alpha = 1.0, fill = "#999999"
    ) +
    
    geom_col( # Current growing season
      data = dplyr::filter(inData, inData$date_year == max(inData$date_year)), 
      mapping = aes(x = as.factor(.data$date_year), y = .data$heat_units_55F_cumulative), 
      alpha = 1.0, fill = "#001C48"
    ) +
    
    geom_hline( # Cotton growth stages as cumulative heat units
      data = dataCottonGrowthStages, 
      mapping = aes(yintercept = huapValue), 
      alpha = 1.0, color = "#CCCCCC", linetype = "solid", linewidth = 0.3
    ) +
    
    geom_label( # Previous growing season
      data = dplyr::filter(inData, inData$date_year < max(inData$date_year)), 
      mapping = aes(label = .data$labelHUs, fontface = "bold"), 
      color = "#999999", fill = NA, label.size = NA, size = 3, vjust = 0.0
    ) +
    
    geom_label( # Current growing season
      data = dplyr::filter(inData, inData$date_year == max(inData$date_year)), 
      mapping = aes(label = .data$labelHUs, fontface = "bold"), 
      color = "#001C48", fill = NA, label.size = NA, size = 3, vjust = 0.0
    ) + 
    
    labs(x = "\nYear", y = "Cumulative Heat Units\n") +
    
    scale_y_continuous(
      breaks = dataCottonGrowthStages$huapValue, 
      labels = dataCottonGrowthStages$huapValue,
      expand = expansion(mult = c(0.01, 0.05)),
      sec.axis = (dup_axis(labels = dataCottonGrowthStages$growthStage))
    ) +
    
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
      axis.title.y.right = element_blank(),
      #axis.text,
      axis.text.x = element_text(color = "#343a40", face = "plain", size = 10),
      #axis.text.x.top,
      #axis.text.x.bottom,
      axis.text.y = element_text(color = "#343a40", face = "plain", size = 10),
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
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
      #panel.grid.major.x,
      #panel.grid.major.y,
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
