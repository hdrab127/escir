#' hazen_quantile ----
#'
#' adapted from https://www.mfe.govt.nz/publications/fresh-water/bathewatch-user-guide/hazen-percentile-calculator
#'
#' @param x numeric vector with below detection limit values (x_i = '< lbd')
#'  converted to half the detection limit, and above detection limit values
#'  (x_i = '> ubd') converted to the detection limit plus one.
#' @param p double percent / 100
#' @param na.rm remove missing values
#' @param strict boolean, if true throw error on too few samples, otherwise yield NA
#'
#' @return Hazen percentile value
#'
#' @export
hazen_percentile <- function(x, p = 0.95, na.rm = FALSE, strict = TRUE) {
  if (na.rm) {
    x <- stats::na.omit(x)
  }
  y <- sort(x)
  n <- length(x)

  # minimum data size
  if (p >= 0.5) {
    min_n <- 1 / (2 * (1 - p))
  } else {
    min_n <- 1 / (2 * p)
  }

  if (n < min_n) {
    if (strict) {
      stop('Too few samples to calculate Hazen percentile')
    } else {
      return(NA_real_)
    }
  }

  # Hazen rank
  r_H <- 0.5 + (p * n)

  # integer part of r_H
  r_i <- floor(r_H)

  # fractional part of r_H
  r_f <- r_H - r_i

  # Hazen pth percentile
  # use sum na.rm as r_i = n => r_f * y[r_i + 1] = 0 * NA = NA
  sum(c((1 - r_f) * y[r_i], r_f * y[r_i + 1]), na.rm = TRUE)
}
