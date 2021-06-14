#' Cut numeric column into colours bins for maps
#'
#' \code{assign_heatmap_bins} takes a numeric column and breaks it into n equal
#' bins that are mapped to a colour palette and tidy labels.
#'
#' @param d data.frame or tibble.
#' @param by unquote numeric column name to split into bins.
#' @param n_bins the rough number of bins to create. Base R's pretty will give
#'   or take to get 'pretty' break points.
#' @param colours character vector of colours for \code{colorRamp}, low values
#'   of \code{by} will get assigned to the first values of colours while high
#'   values get assigned to the tail of colours.
#' @param sep text separator between from and to values.
#'
#' @return A \code{tbl_df} with added columns fill_colour and label, plus list
#'   columns containing all_colours and all_labels used that can populate the
#'   map legend
#'
#' @importFrom rlang `!!` .data
#' @importFrom grDevices colorRamp
#' @importFrom stats runif
#' @importFrom magrittr `%>%`
#'
#' @export
assign_heatmap_bins <- function(d, by, n_bins = 4, colours, sep = ' - ') {
  # get reference to column in table
  by_col <- dplyr::enquo(by)

  # get pretty breakpoints
  breaks <-
    range(dplyr::pull(d, !!by_col), na.rm = TRUE) %>%
    pretty(n = n_bins)
  # and pretty labels for these
  labels <-
    chr_intervals(breaks, sep)

  # cut the by column into these break/labels
  out <-
    dplyr::mutate(d,
                  legend_label = cut(x = !!by_col,
                                     breaks = breaks,
                                     labels = labels,
                                     right = FALSE,
                                     include.lowest = TRUE))

  # then fix colours to each bracket
  legend_palette <-
    leaflet::colorFactor(palette = colorRamp(colours),
                         levels = levels(out$legend_label))

  # and assign these to each bin
  dplyr::mutate(out,
                fill_colour = legend_palette(.data$legend_label),
                # (also saving each possible label/colours for legend args)
                all_colours = list(legend_palette(labels)),
                all_labels = list(labels),
                legend_label = as.character(.data$legend_label))
}

#' Convert breakpoints into nice from - to interval labels
#'
#' @param xs vector of break points
#' @param sep text separator between from and to values.
#'
#' @return A character vector of intervals
#'
#' @export
chr_intervals <- function(xs, sep) {
  single_interval <- function(i) {
    # left pad so all inline
    n_zeros_pad <- max_nums - nchar(gsub('^-', '', xs_chr[i-1]))
    needs_minus_pad_lhs <- has_negs & (xs[i-1] >= 0)
    needs_minus_pad_rhs <- has_negs & (xs[i] >= 0)
    if ((n_zeros_pad + needs_minus_pad_lhs) > 0) {
      paste0('<span style="visibility: hidden">',
             dplyr::if_else(needs_minus_pad_lhs, '-', ''),
             paste0(rep('0', n_zeros_pad), collapse = ''),
             '</span>',
             xs_chr[i-1],
             sep,
             dplyr::if_else(needs_minus_pad_rhs,
                            '<span style="visibility: hidden">-</span>',
                            ''),
             xs_chr[i])
    } else {
      paste0(xs_chr[i-1], sep, xs_chr[i])
    }
  }
  max_nums <- max(nchar(abs(xs))[-length(xs)])
  has_negs <- any(xs < 0)
  xs_chr <- format(xs, trim = TRUE)
  sapply(seq_along(xs)[-1], single_interval)
}


#' Insert title above leafelt layer controls box
#'
#' @param map leaflet htmlwidget object with layer control.
#' @param title character string to put above layer options.
#'
#' @return htmlwidget
#'
#' @export
addLayersControlTitle <- function(map, title) {
  id <- map$elementId
  if (is.null(id)) {
    stop('addLayersControlTitle requires specifying a map id in the base leaflet(elementId = "...") constructor.')
  }
  css_sel <-
    paste0('#', id, '>',
           'div.leaflet-control-container>div>div.leaflet-control-layers')
  js_cb <-
    paste0('var title = document.createElement("div");',
           # 'title.style.marginLeft = "4px";',
           'title.style.fontSize = "1.1em";',
           # 'title.style.color = "#555";',
           'title.innerHTML = "<b>', title, '</b>";',
           'var control = document.querySelector("', css_sel, '");',
           'control && control.insertBefore(title, control.firstChild);')
  htmlwidgets::appendContent(map,
                             htmlwidgets::onStaticRenderComplete(htmlwidgets::JS(js_cb)))
}
