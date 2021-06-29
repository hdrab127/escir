#' Custom datatable for GWRC reporting
#'
#' \code{escir_datatable} is like the normal \code{DT::datatable} with some
#' defaults set and the options to hide/left align and not sort some columns.
#'
#' @param d data.frame or tibble.
#' @param elementId id of htmlwidget.
#' @param shared if TRUE, this will be linked via crosstalk to other widgets
#'   (e.g. click and highlight), otherwise FALSE renders as simple interactive
#'   table
#' @param height max height (px) before scrollbar used.
#' @param container manually specify html table layout for dataframe
#' @param cols_to_hide character vector of column names to suppress from output.
#' @param cols_to_left_align character vector of column names to left align
#' @param cols_to_fix_order parse character column to numeric for table sorting
#'   (i.e. when a column has detection limits or states appended)
#' @param cols_nonsortable character vector of column names to disable sorting
#'   buttons
#' @param col_widths named list or vector of column name = width, i.e.
#'   list('Date' = '82px', 'Site' = '120px'). Use this if you need to stop a
#'   column from text wrapping
#' @param col_colours named list or vector of column name = colour for
#'   background shading, i.e. list('Good' = 'green', 'Bad' = 'red').
#' @param escape whether to escape HTML in the data (safe to leave as FALSE)
#'
#' @section Notes:
#'
#' These columns are hidden by default:
#' \itemize{
#'   \item{key}
#'   \item{lng}
#'   \item{lat}
#'   \item{legend_colour}
#'   \item{fill_colour}
#'   \item{all_labels}
#'   \item{all_colours}
#'   \item{p_labs}
#'   \item{colour}
#'   \item{weight}
#'   \item{radius}
#'   \item{p_colours}
#'   \item{site_code}
#'   \item{map_label}
#'   \item{map_popup}
#'   \item{below_det}
#'   \item{survey_group}
#' }
#'
#' These columns are left aligned by default:
#' \itemize{
#'   \item{Whaitua}
#'   \item{Site code}
#'   \item{Site}
#'   \item{Catchment}
#'   \item{Area}
#'   \item{Event}
#'   \item{Duration}
#'   \item{Site name}
#'   \item{Date}
#'   \item{Rainfall station}
#'   \item{Season}
#'   \item{Flows}
#'   \item{Land use}
#'   \item{Soil order}
#'   \item{Soil type}
#'   \item{Lake}
#'   \item{Type}
#' }
#'
#' @importFrom magrittr `%>%`
#'
#' @export
escir_datatable <- function(d,
                            elementId = NULL,
                            shared = FALSE,
                            height = 360,
                            container,
                            cols_to_hide = c(),
                            cols_to_left_align = c(),
                            cols_to_fix_order = c(),
                            cols_nonsortable = c(),
                            col_widths = list(),
                            col_colours = list(),
                            escape = FALSE) {
  # get raw data from shared object for col indexing
  if (shared) {
    d_raw <- d$data()
  } else {
    d_raw <- d
  }
  # columns not rendered
  # TODO: trim this to necessary
  hide_cols <- which(names(d_raw) %in%
                       c(cols_to_hide,
                         'key',
                         'lng',
                         'lat',
                         'legend_colour',
                         'fill_colour',
                         'all_labels',
                         'all_colours',
                         'p_labs',
                         'colour',
                         'weight',
                         'radius',
                         'p_colours',
                         'site_code',
                         'map_label',
                         'map_popup',
                         'below_det',
                         'survey_group'))

  # columns left aligned
  left_cols <- which(names(d_raw) %in%
                       c(cols_to_left_align,
                         'Whaitua',
                         'Site code',
                         'Site\ncode',
                         'Site',
                         'Catchment',
                         'Area',
                         'Event',
                         'Duration',
                         'Site name',
                         'Date',
                         'Rainfall station',
                         'Season',
                         'Flows',
                         'Land use',
                         'Soil order',
                         'Soil type',
                         'Lake',
                         'Type'))

  # centred columns
  centre_cols <- seq_along(d_raw)[-c(hide_cols, left_cols)]

  # datatable options
  dt_options <-
    list(dom = 't',
         deferRender = TRUE,
         # container width - vert scrollbar width
         scrollX = 1170 - 17,
         scrollCollapse = TRUE,
         paging = FALSE,
         scroller = TRUE,
         columnDefs = list(list(visible = FALSE, targets = hide_cols - 1),
                           list(className = 'dt-center', targets = centre_cols - 1)))

  # add scroll height if specified
  if (!is.null(height)) {
    dt_options$scrollY <- paste0(height - 40, 'px')
    height_px <- paste0(height, 'px')
  } else {
    height_px <- NULL
  }

  # add non-sortables (if any)
  if (length(cols_nonsortable) > 0) {
    dt_options$columnDefs <-
      c(dt_options$columnDefs,
        list(list(orderable = FALSE,
                  targets = which(names(d_raw) %in% cols_nonsortable) - 1)))
  }

  # add widths (if any)
  if (length(col_widths) > 0) {
    for (column_name in names(col_widths)) {
      dt_options$columnDefs <-
        c(dt_options$columnDefs,
          list(list(width = col_widths[[column_name]],
                    targets = which(names(d_raw) == column_name) - 1)))
    }
  }

  # add colours (if any) {
  if (length(col_colours) > 0) {
    # COLOUR HEADERS (required DT api)
    # build list of dt colour calls
    colour_queries <-
      lapply(names(col_colours),
             function(column_name) {
               paste0('$(this.api().table().column(', which(names(d_raw) == column_name) - 1, ')',
                      '.header()).css({"background-color": "', col_colours[[column_name]], '"});')
             })
    # and bundle into one callback for after dt initialised
    dt_options$initComplete <-
      DT::JS(paste0('function(settings, json) {',
                    paste0(colour_queries, collapse = '\n'),
                    '}'))
    # COLOUR CELLS (can use R)
    colour_cells <- function(dt) {
      for (column_name in names(col_colours)) {
        dt <- DT::formatStyle(dt,
                              columns = column_name,
                              backgroundColor = col_colours[[column_name]])
      }
      dt
    }
  } else {
    # just returns table if no colours to assign
    colour_cells <- identity
  }

  # fix sorting on string value columns if any
  if (length(cols_to_fix_order) > 0) {
    col_list <- list()
    for (column_name in names(d_raw)) {
      if (column_name %in% cols_to_fix_order) {
        col_list <-
          c(col_list,
            list(list(title = column_name,
                      type = 'fix-order')))
      } else if (column_name == 'Plan target') {
        col_list <-
          c(col_list,
            list(list(title = column_name,
                      type = 'plan-order')))
      } else {
        col_list <-
          c(col_list,
            list(list(title = column_name)))
      }
    }
    dt_options$columns <- col_list
  }

  # output using original input data
  if (missing(container)) {
    DT::datatable(d,
                  elementId = elementId,
                  style = 'bootstrap',
                  class = 'compact hover',
                  # changing this doesn't seem to do anything?
                  width = '100%',
                  height = height_px,
                  escape = escape,
                  rownames = FALSE,
                  options = dt_options) %>%
      colour_cells()
  } else {
    DT::datatable(d,
                  container = container,
                  elementId = elementId,
                  style = 'bootstrap',
                  class = 'compact hover',
                  # changing this doesn't seem to do anything?
                  width = '100%',
                  height = height_px,
                  escape = escape,
                  rownames = FALSE,
                  options = dt_options) %>%
      colour_cells()

  }
}
