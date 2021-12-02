htp_get_observation <- function(url,
                                Site,
                                Measurement,
                                TemporalFilter = NULL,
                                verbose = FALSE) {
  query <- paste0(url, '?service=SOS&Request=GetObservation')

  # if no dates specified hilltop returns all or default
  if (length(TemporalFilter) == 0) {
    readline('No date range specified, this will request all data and can be slow.\nHit [Enter] to continue, or Ctrl-c to abort')
  } else if (length(TemporalFilter) > 1) {
    stop('Multiple values are not allowed for From or To')
  } else {
    query <- paste0(query, '&TemporalFilter=om:phenomenonTime,', TemporalFilter)
  }

  # run for each measure-site
  site_data <- dplyr::bind_rows(lapply(Measurement,
                                       fetch_property,
                                       s = Site,
                                       htp_q = query,
                                       v = verbose))

  # return clean table
  cat('\n')
  if (nrow(site_data) == 0) {
    return(tibble::tibble())
  } else {
    site_data <- dplyr::rename_with(site_data, ~ tolower(gsub(' |\\.', '_', .x)))
    site_data <- dplyr::rename(site_data, timestamp = .data$`t`)
    site_data <- dplyr::relocate(site_data, .data$site, .before = dplyr::everything())
    site_data
  }
}

# encode and log read_xml urls
read_xml_url <- function(url_query, verbose) {
  # encode as url query
  url_query <- utils::URLencode(url_query)

  # downloard and parse
  if (verbose) {
    print(paste0('Fetching: ', url_query))
  }
  # catch service down HTTP error 503 codes
  tryCatch(xml2::read_xml(url_query),
           error = function(e) {
             print(e)
             stop('Might be a connection issue - check you have internet and try again in a minute. If this error persists please contact hayden@saltecology.co.nz')
           })
}
# so we can run for multiple measures/sites
fetch_property <- function(m, s, htp_q, v) {
  dplyr::bind_rows(lapply(s,
                          fetch_feature,
                          m = m,
                          htp_q = htp_q,
                          v = v))
}
# so we can run for multiple measures/sites
fetch_property <- function(m, s, htp_q, v) {
  # finish query and fetch data
  cat('\n', m, ' - ', s, crayon::yellow(' [fetching from server]'), sep = '')
  htp_q <- paste0(htp_q, '&observedProperty=', m)
  htp_q <- paste0(htp_q, '&featureOfInterest=', s)
  res <- read_xml_url(htp_q, verbose = v)

  # check xml errors
  res_error <- xml2::xml_find_all(res, './/Error')
  if (length(res_error) > 0) {
    cat('\r', m, ' - ', s, crayon::red(' [no data found]       '), sep = '')
    return(tibble::tibble())
  } else {
    cat('\r', m, ' - ', s, crayon::yellow(' [processing into R]   '), sep = '')
  }
  # convert to table with parameters/unknown value names
  ts <- xml_text(xml_find_all(res, '//wml2:observationMember/om:OM_Observation/om:result/wml2:MeasurementTimeseries/wml2:point/wml2:MeasurementTVP/wml2:time'))
  xs <- xml_text(xml_find_all(res, '//wml2:observationMember/om:OM_Observation/om:result/wml2:MeasurementTimeseries/wml2:point/wml2:MeasurementTVP/wml2:value'))
  if (length(ts) == 0) {
    cat('\r', m, ' - ', s, crayon::red(' [no data found]       '), sep = '')
    return(tibble::tibble())
  }
  res_data <- tibble::tibble(t = ts, xs = xs, site = s, .rows = length(ts))

  cat('\r', m, ' - ', s, crayon::green(' [complete]             '), sep = '')
  res_data
}
