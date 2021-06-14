#' Handle above and below detection values
#'
#' \code{add_num_value} takes the raw value column and halves value below
#' detection ('< x') set values above detection ('> x') at their limit or plus
#' one. And converts missing values ('*') to NA_real_
#'
#' @param d data.frame with raw Hilltop data.
#' @param below_det method to handle values below detection, currently only
#'   'halve' supported.
#' @param above_det method to handle values above detection, valid choices are
#'   'at' (default). or 'at + 1'.
#' @param var_name name of the raw values column, usually 'value' but sometimes
#'   'i1'.
#'
#' @return A \code{tbl_df} with new numeric column 'num_value' with the adjusted
#'   values and two logical columns flagging whether original value was over or
#'   under (useful if counting number of above or below detection values)
#'
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#' add_num_value(data.frame(value = c('< 0.1', '> 2', '*', 1, 0.3, 0.5)))
#' #> value num_value below_det above_det
#' #> < 0.1      0.05      TRUE     FALSE
#' #>   > 2      2.00     FALSE      TRUE
#' #>     *        NA     FALSE     FALSE
#' #>     1      1.00     FALSE     FALSE
#' #>   0.3      0.30     FALSE     FALSE
#' #>   0.5      0.50     FALSE     FALSE
#' }
#' @export
add_num_value <- function(d,
                          below_det = 'halve',
                          above_det = 'at',
                          var_name = 'value') {
  # grab numeric value col (copies, not mutating)
  d$value <- as.character(dplyr::pull(d, var_name))

  if (length(above_det) > 1) {
    stop('please choose \'at\' detection limit, or \'at + 1\'')
  }
  # logical becomes 0 if false, 1 if true
  add_one <- as.integer(above_det == 'at + 1')
  dplyr::mutate(d,
                # handle missing
                num_value = dplyr::if_else(grepl('\\*', .data$value), NA_character_, .data$value),
                # handle below
                below_det = grepl('^<', .data$num_value),
                num_value = dplyr::if_else(.data$below_det,
                                           as.character(as.double(gsub('^[<>\\*]',
                                                                       '',
                                                                       .data$num_value)) / 2),
                                           .data$num_value),
                # handle above
                above_det = grepl('^>', .data$num_value),
                num_value = dplyr::if_else(.data$above_det,
                                           as.character(as.double(gsub('^[><\\*]',
                                                                       '',
                                                                       .data$num_value)) + add_one),
                                           .data$num_value),
                num_value = as.double(.data$num_value))
}

#' Round a value as you think it would be rounded
#'
#' \code{safe_round} rounds numeric vectors to the digits specified. This is
#' recommended over base round as for rounding off a 5, the IEC 60559 standard
#' (see also ‘IEEE 754’) is expected to be used, ‘go to the even digit’.
#' Therefore round(0.5) is 0 and round(-1.5) is -2. However, this is dependent
#' on OS services and on representation error (since e.g. 0.15 is not
#' represented exactly, the rounding rule applies to the represented number and
#' not to the printed number, and so round(0.15, 1) could be either 0.1 or 0.2).
#' So add smallest possible number to x to ensure 0.0005 => 0.001 and similar.
#'
#' @param x numeric vector to be rounded.
#' @param digits number of decimal places to round to.
#'
#' @return rounded vector.
#'
#' @examples
#' \dontrun{
#' x <- c(0.049999, 0.05, 0.050001)
#' # base rounds 0.05 down to 0
#' round(x, 1)
#' #> 0.0 0.0 0.1
#' # safe_round goes to 0.1
#' safe_round(x, 1)
#' #> 0.0 0.1 0.1
#' }
#' @export
safe_round <- function(x, digits) {
  round(x + 0.0000000001, digits = digits)
}

#' Format numbers with safe rounding
#'
#' \code{safe_format} uses base R format with safer rounding (see \code{?safe_round}). Useful for rounding a numeric column for outputs that want trailing zeroes kept for aligment. It doesn't use scientific representation on large numbers.
#'
#' @param x numeric vector
#' @param digits number of decimal places, if 0 this will add commas between thousands.
#'
#' @return rounded and formatted vector
#'
#' @examples
#' \dontrun{
#' x <- c(0.049999, 0.05, 0.050001)
#' # base
#' format(x, digits = 1)
#' #> "0.05" "0.05" "0.05"
#' safe_format(x, digits = 1)
#' #> "0.0" "0.1" "0.1"
#'
#' x <- c(1500.499, 1500.5, 1500.5001, 15000.5)
#' # base
#' format(x, digits = 0)
#' #> "1.500499e+03" "1.500500e+03" "1.500500e+03" "1.500050e+04"
#' # safe
#' safe_format(x, digits = 0)
#' #> "1,500" "1,500" "1,501" "15,001"
#' }
#' @export
safe_format <- function(x, digits) {
  out <- safe_round(x, digits)
  if (digits == 0) {
    out <- format(out, big.mark = ',')
  } else {
    out <- format(out, nsmall = digits, scientific = FALSE)
  }
  out <- trimws(out)
  out[grepl('^(NA|-?Inf)$', out)] <- NA_character_
  out
}

#' Use pH to adjust ammoniacal nitrogen
#'
#' \code{pH_adjusted_NH3_N} estimates NH4-N, the nitrogen concentration of the
#' ammonium ion based on a pH of 8 and water temperature of 20degC. GWRC method.
#'
#' @param pH vector or column of pH values
#' @param NH3_N vector or column of ammoniacal nitrogen values
#'
#' @return vector of NH4-N values
#'
#' @examples
#' pH_adjusted_NH3_N(pH = rnorm(10, 7, 1), NH3_N = rep(0.01))
#' @export
pH_adjusted_NH3_N <- function(pH, NH3_N) {
  # low pH
  adjustment_factor <- rep(2.86, length(pH))

  # mid pH
  mid_idx <- which(pH >= 6 & pH <= 9)
  adjustment_factor[mid_idx] <-
    0.2509 * pH[mid_idx] ^ 3 -
    5.767 * pH[mid_idx] ^ 2 +
    42.7364 * pH[mid_idx] -
    100.2489

  # high pH
  high_idx <- which(pH > 9)
  adjustment_factor[high_idx] <- 0.2

  # apply
  NH3_N / adjustment_factor
}
