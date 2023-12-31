% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_hilltop.R
\name{htp_site_info}
\alias{htp_site_info}
\title{Get information for a single site}
\usage{
htp_site_info(url, Site, verbose = FALSE)
}
\arguments{
\item{url}{Hilltop webservice url, this points to the desired .hts file.}

\item{Site}{Hilltop site name to query.}

\item{verbose}{boolean, whether to print the url query.}
}
\value{
A \code{tbl_df} containing the site metadata.
}
\description{
\code{htp_site_info} fetches metadata for a single Hilltop site. See
\code{?htp_site_list} for multiple sites.
}
\examples{
\dontrun{
# get hilltop web service url to desired data file
htp_url <- 'http://hilltop.{COUNCIL_ABBR}.govt.nz/{DATA_FILE}.hts?Service=Hilltop'

# check available sites using htp_site_list
htp_site_info(htp_url, 'SITE NAME')
}
}
