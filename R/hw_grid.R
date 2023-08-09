#' Lays out highchart widgets into a "grid", similar to
#' \code{grid.arrange} from \code{gridExtra}.
#'
#' @param ... either individual `highchart` objects or a mixture of
#'   individual `highchart` objects and `list`s of `highchart` objects.
#' @param ncol how many columns in the grid
#' @param rowheight Height in px.
#' @param add_htmlgrid_css A logical value to add or not `htmlgrid.css` as
#'   dependency.
#' @param browsable Logical value indicating if the returned object is converted
#'   to an HTML object browsable using \code{htmltools::browsable}.
#'
#' This is a hack to fix the bootstrap issues with highcharter::hw_grid
#'
#' @export
hw_grid_2 <- function(...,
                      ncol = NULL,
                      rowheight = NULL,
                      add_htmlgrid_css = TRUE,
                      browsable = TRUE) {
  input_list <- as.list(substitute(list(...)))[-1L]

  params <- list()

  for (i in seq_len(length(input_list))) {
    x <- eval.parent(input_list[[i]])
    if (any(class(x) == 'list')) {
      for (j in seq_len(length(x))) {
        y <- eval(x[[j]])
        params[[length(params) + 1]] <- y
      }
    } else {
      params[[length(params) + 1]] <- x
    }
  }

  if (!all(sapply(params, function(x) inherits(x, 'htmlwidget')))) {
    stop('All parameters must be htmlwidget objects')
  }


  if (is.null(ncol)) {
    ncol <- grDevices::n2mfrow(length(params))[1]
  }

  if (ncol > 12) {
    ncol <- 12
  }

  ncolm <- floor(ncol / 2)

  # adding htmlwdgtgrid.css dependencies
  dep <- htmltools::htmlDependency(
    name = 'htmlwdgtgrid',
    version = '0.0.9',
    src = c(file = system.file('css', package = 'escir')),
    stylesheet = 'htmlwdgtgrid-2.css',
  )

  divs <- lapply(params, function(x) {
    if (add_htmlgrid_css) {
      x$dependencies <- c(x$dependencies, list(dep))
    }

    x$width <- '100%'

    if (!is.null(rowheight)) {
      x$height <- rowheight
    }

    htmltools::tags$div(class = sprintf('col-1-%s mobile-col-1-%s',
                                        ncol,
                                        ncolm),
                        x)
  })

  divgrid <- htmltools::tags$div(class = 'hw-grid hw-grid-pad',
                                 divs,
                                 htmltools::tags$div(style = 'clear: both'))

  class(divgrid) <- c(class(divgrid), 'htmlwdwtgrid')

  if (browsable) {
    return(htmltools::browsable(divgrid))
  }

  divgrid
}
