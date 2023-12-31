% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{add_num_value}
\alias{add_num_value}
\title{Handle above and below detection values}
\usage{
add_num_value(d, below_det = "halve", above_det = "at", var_name = "value")
}
\arguments{
\item{d}{data.frame with raw Hilltop data.}

\item{below_det}{method to handle values below detection, currently only
'halve' supported.}

\item{above_det}{method to handle values above detection, valid choices are
'at' (default). or 'at + 1'.}

\item{var_name}{name of the raw values column, usually 'value' but sometimes
'i1'.}
}
\value{
A \code{tbl_df} with new numeric column 'num_value' with the adjusted
  values and two logical columns flagging whether original value was over or
  under (useful if counting number of above or below detection values)
}
\description{
\code{add_num_value} takes the raw value column and halves value below
detection ('< x') set values above detection ('> x') at their limit or plus
one. And converts missing values ('*') to NA_real_
}
\examples{
\dontrun{
add_num_value(data.frame(value = c('< 0.1', '> 2', '*', 1, 0.3, 0.5)))
#> value num_value below_det above_det
#> < 0.1      0.05      TRUE     FALSE
#>   > 2      2.00     FALSE      TRUE
#>     *        NA     FALSE     FALSE
#>     1      1.00     FALSE     FALSE
#>   0.3      0.30     FALSE     FALSE
#>   0.5      0.50     FALSE     FALSE
}
}
