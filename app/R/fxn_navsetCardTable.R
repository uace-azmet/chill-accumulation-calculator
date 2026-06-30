#' `fxn_navsetCardTable.R` - Build summary table with daily data
#' 
#' @param inData - Data table [[1]] from `fxn_chillAccumulation.R`
#' @param startDate - Start date of period of interest
#' @param endDate - End date of period of interest
#' @param chillVariable - Chill variable selected by user
#' @return `navsetCardTable` - Summary table of daily data, reactable format


fxn_navsetCardTable <- function(inData, startDate, endDate, chillVariable) {
  
  if (chillVariable %in% c("Chill Portions", "Utah Model")) {
    digits <- 1
  } else { # "Hours below 32 °F", "Hours between 32 and 45 °F", "Hours below 45 °F", "Hours above 68 °F"
    digits <- 0
  }
  
  inData <- inData %>% 
    dplyr::filter(datetime >= startDate & datetime <= endDate) %>% 
    dplyr::select(
      dplyr::all_of(
        c(
          "meta_station_name",
          "datetime",
          "day_of_period",
          "chill", 
          "chill_acc"
        )
      )
    )
  
  if (chillVariable == "Chill Portions") {
    variableUnits <- "portions"
  } else if (chillVariable == "Utah Model") {
    variableUnits <- "units"
  } else { # "Hours below 32 °F", "Hours between 32 and 45 °F", "Hours below 45 °F", "Hours above 68 °F"
    variableUnits <- "hours"
  }
  
  
  # Table -----
  
  navsetCardTable <- inData |>
    reactable::reactable(
      columns = list(
        meta_station_name = 
          reactable::colDef(
            name = "Station",
            #aggregate = NULL,
            #sortable = NULL,
            #resizable = NULL,
            #filterable = NULL,
            #searchable = NULL,
            #filterMethod = NULL,
            #show = TRUE,
            #defaultSortOrder = NULL,
            #sortNALast = FALSE,
            #format = NULL,
            #cell = NULL,
            #grouped = NULL,
            #aggregated = NULL,
            #header = NULL,
            #footer = NULL,
            #details = NULL,
            #filterInput = NULL,
            html = TRUE,
            na = "NA",
            rowHeader = FALSE,
            minWidth = 150,
            #maxWidth = NULL,
            #width = NULL,
            #align = NULL,
            #vAlign = NULL,
            #headerVAlign = NULL,
            # sticky = "left",
            # class = "table-reactable-column-station",
            # style = list(
            #   borderRight = "1px solid #989898",
            #   boxShadow = "1px 0px 0px 0px #e3e3e3"
            # ),
            #headerClass = NULL,
            # headerStyle = list(
            #   borderRight = "1px solid #989898",
            #   boxShadow = "1px 1px 0px 0px #e3e3e3"
            # ),
            #footerClass = NULL,
            #footerStyle = NULL
          ), 
        datetime = 
          reactable::colDef(
            name = "Date",
            html = TRUE,
            # minWidth = 100,
            na = "NA",
            rowHeader = TRUE
          ),
        day_of_period = 
          reactable::colDef(
            name = htmltools::HTML("Day<sub>period</sub><br>"),
            html = TRUE,
            # minWidth = 100,
            na = "NA",
            rowHeader = TRUE
          ),
        chill = 
          reactable::colDef(
            name = 
              htmltools::HTML(
                paste0(
                  "Chill<br>", tags$span(style = "font-weight: normal; font-size: 0.8rem", paste0("(", variableUnits, ")"))
                )
              ),
            format = reactable::colFormat(digits = digits),
            html = TRUE,
            na = "NA",
            rowHeader = TRUE
          ),
        chill_acc = 
          reactable::colDef(
            name = 
              htmltools::HTML(
                paste0(
                  "Chill<sub>cumulative</sub><br>",
                  tags$span(style = "font-weight: normal; font-size: 0.8rem", paste0("(", variableUnits, ")"))
                )
              ),
            format = reactable::colFormat(digits = digits),
            html = TRUE,
            na = "NA",
            rowHeader = TRUE
          )
        ),
        #columnGroups = NULL,
        rownames = FALSE,
        #groupBy = NULL,
        sortable = FALSE,
        resizable = FALSE,
        filterable = FALSE,
        searchable = FALSE,
        searchMethod = NULL,
        #defaultColDef = NULL,
        #defaultColGroup = NULL,
        #defaultSortOrder = "asc",
        #defaultSorted = NULL,
        pagination = FALSE,
        #defaultPageSize = 10,
        showPageSizeOptions = FALSE,
        #pageSizeOptions = c(10, 25, 50, 100),
        #paginationType = "numbers",
        showPagination = NULL,
        showPageInfo = FALSE,
        #minRows = 1,
        #paginateSubRows = FALSE,
        #details = NULL,
        defaultExpanded = TRUE,
        selection = NULL,
        defaultSelected = NULL,
        onClick = NULL,
        highlight = TRUE,
        outlined = FALSE,
        bordered = FALSE,
        borderless = TRUE,
        striped = TRUE,
        compact = TRUE,
        #wrap = TRUE,
        #showSortIcon = TRUE,
        #showSortable = FALSE,
        class = "navset-card-table",
        #style = NULL,
        #rowClass = NULL,
        #rowStyle = NULL,
        fullWidth = TRUE,
        width = "auto",
        # height = 400,
        theme = 
          reactable::reactableTheme(
            color = NULL,
            backgroundColor = "#FFFFFF",
            borderColor = "#dee2e6",
            borderWidth = "2px",
            stripedColor = NULL,
            highlightColor = NULL,
            cellPadding = NULL,
            style = NULL,
            tableStyle = NULL,
            headerStyle = 
              list(
                color = "#191919", 
                fontFamily = "monospace", 
                fontSize = "0.8rem",
                borderBottomColor = rgb(180/255, 180/255, 180/255, 1.0),
                borderBottomWidth = "1px",
                boxShadow = "0px 1px 0px 0px #e3e3e3"
              ),
            groupHeaderStyle = NULL,
            tableBodyStyle = NULL,
            rowGroupStyle = NULL,
            rowStyle = NULL,
            rowStripedStyle = NULL,
            rowHighlightStyle = NULL,
            rowSelectedStyle = NULL,
            cellStyle = 
              list(
                color = "#191919", 
                fontFamily = "monospace", 
                fontSize = "0.8rem"
              ),
            footerStyle = NULL,
            inputStyle = NULL,
            filterInputStyle = NULL,
            searchInputStyle = NULL,
            selectStyle = NULL,
            paginationStyle = NULL,
            pageButtonStyle = NULL,
            pageButtonHoverStyle = NULL,
            pageButtonActiveStyle = NULL,
            pageButtonCurrentStyle = NULL
          ),
        #language = getOption("reactable.language"),
        #meta = NULL,
        #elementId = NULL,
        #static = getOption("reactable.static", FALSE),
        #selectionId = NULL
    )
  
  return(navsetCardTable)
}
