#' Get site lists
#'
#' \code{htp_site_list} fetches site names and metadata from Hilltop
#'
#' @param url Hilltop webservice url, this points to the desired .hts file.
#' @param Location 'Yes' returns NZTM easting and northing. 'LatLong' latitude
#'   and longitude. NULL returns no location columns.
#' @param Measurement Filter to only sites with this measurement.
#' @param verbose boolean, whether to print the url query.
#'
#' @return A \code{tbl_df} containing the site names and locations/measurements
#'   if requested
#'
#' @importFrom rlang .data
#' @importFrom stats setNames
#'
#' @examples
#' \dontrun{
#' # get hilltop web service url to desired data file
#' htp_url <- 'http://hilltop.{COUNCIL_ABBR}.govt.nz/{DATA_FILE}.hts?Service=Hilltop'
#'
#' htp_site_list(htp_url)
#' htp_site_list(htp_url, Location = 'Yes')
#' htp_site_list(htp_url, Location = 'LatLong')
#'
#' htp_site_list(htp_url, Measurement = c('E-Coli'))
#' htp_site_list(htp_url, Location = 'Yes', Measurement = c('E-Coli'))
#' htp_site_list(htp_url, Location = 'LatLong', Measurement = c('E-Coli'))
#'
#' htp_site_list(htp_url, Measurement = c('E-Coli', 'Rainfall'))
#' htp_site_list(htp_url, Location = 'Yes', Measurement = c('E-Coli', 'Rainfall'))
#' htp_site_list(htp_url, Location = 'LatLong', Measurement = c('E-Coli', 'Rainfall'))
#' }
#' @export
htp_site_list <- function(url,
                          Location = NULL,
                          Measurement = NULL,
                          verbose = FALSE) {
  # prep query and test params
  query <- paste0(url, '&Request=SiteList')
  loc_nms <- c('site')
  if (length(Location) > 1) {
    stop('Location if specified must be one of \'Yes\' or \'LatLong\'')
  }
  if (!is.null(Location)) {
    query <- paste0(query, '&Location=', Location)
    if (Location == 'Yes') loc_nms <- c(loc_nms, 'nztm_e', 'nztm_n')
    if (Location == 'LatLong') loc_nms <- c(loc_nms, 'lat', 'lng')
  }

  # for each measurement
  site_list_internal <- function(m) {
    # finish query and fetch xml
    if (!is.null(m)) {
      htp_q <- paste0(query, '&Measurement=', m)
    } else {
      htp_q <- paste0(query)
    }
    res <- read_xml_url(htp_q, verbose)

    # only keep site (drop agency and version)
    res <- xml2::xml_find_all(res, '/HilltopServer/Site')
    res_data <- lapply(seq_along(res), function(i) {
      node <- res[i]
      if (xml2::xml_length(node) > 0) {
        node_data <- xml2::xml_children(node)
        tibble::as_tibble(setNames(object = as.list(c(xml2::xml_attr(node, 'Name'),
                                                      xml2::xml_text(node_data))),
                                   nm = loc_nms))
      } else {
        tibble::tibble(site = xml2::xml_attr(node, 'Name'))
      }
    })
    res_data <- dplyr::bind_rows(res_data)
    res_data <- dplyr::mutate(res_data, dplyr::across(-.data$site, as.double))
    res_data$requestas <- m
    res_data
  }
  if (length(Measurement) > 0) {
    # run for each measurement
    dplyr::bind_rows(lapply(Measurement, site_list_internal))
  } else {
    # run just once
    site_list_internal(m = NULL)
  }
}

#' Get information for a single site
#'
#' \code{htp_site_info} fetches metadata for a single Hilltop site. See
#' \code{?htp_site_list} for multiple sites.
#'
#' @param url Hilltop webservice url, this points to the desired .hts file.
#' @param Site Hilltop site name to query.
#' @param verbose boolean, whether to print the url query.
#'
#' @return A \code{tbl_df} containing the site metadata.
#'
#' @examples
#' \dontrun{
#' # get hilltop web service url to desired data file
#' htp_url <- 'http://hilltop.{COUNCIL_ABBR}.govt.nz/{DATA_FILE}.hts?Service=Hilltop'
#'
#' # check available sites using htp_site_list
#' htp_site_info(htp_url, 'SITE NAME')
#' }
#' @export
htp_site_info <- function(url,
                          Site,
                          verbose = FALSE) {
  # prep query and fetch data
  query <- paste0(url,
                  '&Request=SiteInfo',
                  '&Site=', Site)
  res <- read_xml_url(query, verbose)

  # parse site data into tbl
  res <- xml2::xml_find_all(res, '/HilltopServer/Site')
  res_data <- xml2::xml_children(res)
  tibble::as_tibble(setNames(as.list(c(xml2::xml_attr(res, 'Name'),
                                       xml2::xml_text(res_data))),
                             c('site', xml2::xml_name(res_data))))
}

#' Get measurement information for sites
#'
#' \code{htp_measurement_list} fetches available measurements for the sites
#' requested.
#'
#' @param url Hilltop webservice url, this points to the desired .hts file.
#' @param Site Hilltop site name/s to query.
#' @param verbose boolean, whether to print the url query.
#'
#' @return A \code{tbl_df} containing the sites and measurement metadata.
#'
#' @importFrom stats setNames
#'
#' @examples
#' \dontrun{
#' # get hilltop web service url to desired data file
#' htp_url <- 'http://hilltop.{COUNCIL_ABBR}.govt.nz/{DATA_FILE}.hts?Service=Hilltop'
#'
#' htp_measurement_list(htp_url)
#' htp_measurement_list(htp_url, 'SITEA')
#' htp_measurement_list(htp_url, c('SITEA', 'SITEB'))
#' }
#' @export
htp_measurement_list <- function(url,
                                 Site = NULL,
                                 verbose = FALSE) {
  # prep query
  query <- paste0(url, '&Request=MeasurementList')
  if (length(Site) > 0) {
    # run for each site
    res <- lapply(Site, function(s) {
      # finish query and fetch data
      cat(s, crayon::yellow(' [fetching from server]'), sep = '')
      htp_q <- paste0(query, '&Site=', s)
      res_site <- read_xml_url(htp_q, verbose)
      cat('\r', s, crayon::yellow(' [processing into R]   '), sep = '')
      res_site <- xml2::xml_find_all(res_site, '/HilltopServer/DataSource')

      # parse each node
      site_data <- lapply(seq_along(res_site), function(i) {
        node <- res_site[i]
        node_data <- xml2::xml_children(node)
        node_nms <- xml2::xml_name(node_data)
        msr_nodes <- xml2::xml_find_all(node, 'Measurement')
        msr_idx <- node_nms == 'Measurement'
        node_data <- tibble::as_tibble(setNames(object = as.list(c(s,
                                                                   xml2::xml_text(node_data[!msr_idx]))),
                                                nm = c('site',
                                                       node_nms[!msr_idx])))

        # parse each measure
        msr_data <- lapply(seq_along(msr_nodes), function(j) {
          msr_node <- xml2::xml_children(msr_nodes[j])
          tibble::as_tibble(setNames(object = as.list(xml2::xml_text(msr_node)),
                                     nm = c(xml2::xml_name(msr_node))))
        })

        # combine
        dplyr::bind_cols(node_data, dplyr::bind_rows(msr_data))
      })
      site_data <- dplyr::bind_rows(site_data)
      cat('\r', s, crayon::green(' [complete]             '), '\n', sep = '')
      site_data
    })
    res <- dplyr::bind_rows(res)
    res <- dplyr::rename_with(res, ~ tolower(gsub(' |\\.', '_', .x)))

  } else {
    # run once
    s <- 'All sites (this may take awhile)'
    cat(s, crayon::yellow(' [fetching from server]'), sep = '')
    res <- read_xml_url(query, verbose)
    cat('\r', s, crayon::yellow(' [processing into R]   '), sep = '')
    res <- xml2::xml_find_all(res, '/HilltopServer/Measurement')
    res <- tibble::tibble(requestas = xml2::xml_attr(res, 'Name'))
    cat('\r', s, crayon::green(' [complete]             '), '\n', sep = '')
  }
  res
}

# get time range
# NOTE all From, To or Range are interpreted as NZST
#htp_time_range <- function(url,
#                          Site,
#                          Measurement = NULL) {
# query <-
#   paste0(url, '&Request=TimeRange')
# print('Not implemented')
#}

#' Get data for site/s - measurement/s for a date window
#'
#' \code{htp_get_data} fetches data for the sites-measurements requested from
#' \code{From} to \code{To}.
#'
#' @param url Hilltop webservice url, this points to the desired .hts file.
#' @param Site site name/s to query.
#' @param Measurement measurements to query.
#' @param From date or datetime of first data point can be date, POSIXct, or
#'   character string in the form YYYY-MM-DD or YYYY-MM-DD HH:MM:SS, see
#'   examples.
#' @param To time of last measurement to get, see From format.
#' @param tstype if StdQualSeries required.
#' @param verbose boolean, whether to print the url query.
#' @param avg_by Hilltop averaging period, i.e. '1 hour', '1 day', will be
#'   deprecated in future. Better to include this with the measurement name
#'   i.e, Measurement = 'Flow&Method=Average&Interval=1 day$Align=1 day etc.
#'   See https://www.hbrc.govt.nz/assets/Document-Library/Council-Data/20170426-HilltopServerTrimmed.pdf
#'   for info on averaging methods like extrema and total.
#' @param just_i1 faster parsing for continuous series
#' @param metadata whether to return metadata like units, series type etc.
#'
#' @return A \code{tbl_df} containing the data for the sites-measurements-times
#'   requested.
#'
#' @importFrom rlang .data
#' @importFrom tidyr pivot_wider
#' @importFrom methods is
#' @importFrom stats setNames
#'
#' @examples
#' \dontrun{
#' # get hilltop web service url to desired data file
#' htp_url <- 'http://hilltop.{COUNCIL_ABBR}.govt.nz/{DATA_FILE}.hts?Service=Hilltop'
#'
#' # discrete data
#' htp_get_data(htp_url,
#'              Site = 'SITEA',
#'              From = '2019-03-01',
#'              To = '2019-07-01',
#'              Measurement = 'Enterococci Bacteria')
#'
#' # add just_i1 for continuous data for much faster results
#' # metadata false is already know units etc.
#' htp_get_data(htp_url,
#'              Site = 'SITEB',
#'              From = '2019-03-01',
#'              To = '2019-07-01',
#'              Measurement = 'Conductivity (TC)',
#'              just_i1 = TRUE,
#'              metadata = FALSE)
#' }
#' @export
htp_get_data <- function(url,
                         Site,
                         Measurement,
                         From = NULL,
                         To = NULL,
                         verbose = FALSE,
                         tstype = NULL,
                         avg_by = NULL,
                         just_i1 = FALSE,
                         metadata = TRUE) {
  query <- paste0(url, '&Request=GetData')

  # if no dates specified hilltop returns all or default
  if (length(From) == 0 & length(To) == 0) {
    readline('No date range specified, this will request all data and can be slow.\nHit [Enter] to continue, or Ctrl-c to abort')
  } else if (length(From) > 1 & length(To) > 1) {
    stop('Multiple values are not allowed for From or To')
  }
  # test if inputs are valid strings if not dates.
  valid_date_re <- '^(-?(?:[1-9][0-9]*)?[0-9]{4})-(1[0-2]|0[1-9])-(3[01]|0[1-9]|[12][0-9])$'
  valid_datetime_re <- paste0(valid_date_re,
                              '(T| ){1}',
                              '(2[0-3]|[01][0-9]):([0-5][0-9]):([0-5][0-9])(.[0-9]+)?(Z)?')
  if (length(From) == 1) {
    if (is(From, 'Date') |
        inherits(From, 'POSIXct') |
        grepl(valid_date_re, From) |
        grepl(valid_datetime_re, From)) {
      # all good
      query <- paste0(query, '&From=', From)
    } else {
      stop('From must be a Date or POSIXct object,
               or a string in the form yyyy-mm-dd or yyyy-mm-ddTHH:MM:SS respectively')
    }
  }
  if (length(To) == 1) {
    if (is(To, 'Date') |
        inherits(To, 'POSIXct') |
        grepl(valid_date_re, To) |
        grepl(valid_datetime_re, To)) {
      # all good
    } else {
      stop('To must be a date or datetime object,
                 or a string in the form yyyy-mm-dd or yyyy-mm-ddTHH:MM:SS respectively')
    }
  } else {
    # i.e. only from specified
    if (verbose) {
      print('Found only \'From\' datetime, setting \'To\' to now()')
    }
    To <- Sys.time()
  }
  query <- paste0(query, '&To=', To)

  # if averaging (can remove and just add to measurement user side?)
  if (!is.null(avg_by)) {
    query <- paste0(query, '&Method=Average&Interval=', avg_by)

  }
  if (length(tstype) == 1) {
    query <- paste0(query, '&tstype=', tstype)
  }

  # run for each measure-site
  site_data <- dplyr::bind_rows(lapply(Measurement,
                                       fetch_measure,
                                       s = Site,
                                       htp_q = query,
                                       v = verbose,
                                       meta = metadata,
                                       i1 = just_i1))

  # return clean table
  cat('\n')
  if (nrow(site_data) == 0) {
    return(tibble::tibble())
  } else {
    site_data <- dplyr::rename(site_data, timestamp = `t`)
    site_data <- dplyr::relocate(site_data, .data$site, .before = dplyr::everything())
    site_data
  }
}
# so we can run for multiple measures/sites
fetch_measure <- function(m, s, htp_q, v, meta, i1) {
  dplyr::bind_rows(lapply(s,
                          fetch_site,
                          m = m,
                          htp_q = htp_q,
                          v = v,
                          meta = meta,
                          i1 = i1))
}
# so we can run for multiple measures/sites
fetch_site <- function(m, s, htp_q, v, meta, i1) {
  # finish query and fetch data
  cat('\n', m, ' - ', s, crayon::yellow(' [fetching from server]'), sep = '')
  htp_q <- paste0(htp_q, '&Measurement=', m)
  htp_q <- paste0(htp_q, '&Site=', s)
  res <- read_xml_url(htp_q, verbose = v)

  # check xml errors
  res_error <- xml2::xml_find_all(res, './/Error')
  if (length(res_error) > 0) {
    cat('\r', m, ' - ', s, crayon::red(' [no data found]       '), sep = '')
    return(tibble::tibble())
  } else {
    cat('\r', m, ' - ', s, crayon::yellow(' [processing into R]   '), sep = '')
  }
  if (i1) {
    # convert to table if just continuous i1 data
    ts <- xml2::xml_text(xml2::xml_find_all(res, '/Hilltop/Measurement/Data/E/T'))
    i1 <- xml2::xml_text(xml2::xml_find_all(res, '/Hilltop/Measurement/Data/E/I1'))
    if (length(i1) == 0) {
      cat('\r', m, ' - ', s, crayon::red(' [no data found]       '), sep = '')
      return(tibble::tibble())
    }
    # TODO: if diff length ts and i1, fallback to safer method below
    res_data <- tibble::tibble(t = ts, i1 = i1, site = s, .rows = length(ts))

  } else {
    # convert to table with parameters/unknown value names
    res_nodes <- xml2::xml_find_all(res, '/Hilltop/Measurement/Data/E')
    if (length(res_nodes) == 0) {
      cat('\r', m, ' - ', s, crayon::red(' [no data found]       '), sep = '')
      return(tibble::tibble())
    }
    res_data <- lapply(seq_along(res_nodes), data_node_to_tbl, nodes = res_nodes)
    res_data <- dplyr::bind_rows(res_data)
    res_data$site <- s
  }
  if (meta) {
    # add datasource info
    res_meta <- xml2::xml_contents(xml2::xml_find_all(res, '/Hilltop/Measurement/DataSource'))
    meta_nms <- xml2::xml_name(res_meta)
    info_idx <- meta_nms == 'ItemInfo'
    meta_nms <- c(meta_nms[!info_idx],
                  xml2::xml_name(xml2::xml_contents(res_meta[info_idx])))
    meta_nms <- tolower(gsub(' |\\.', '_', meta_nms))
    res_meta <- c(xml2::xml_text(res_meta[!info_idx]),
                  xml2::xml_text(xml2::xml_contents(res_meta[info_idx])))
    res_meta <- tibble::as_tibble(setNames(as.list(res_meta), meta_nms),
                                  .name_repair = suffix_duped_names)
    res_data <- dplyr::bind_cols(res_data, res_meta)
  }
  cat('\r', m, ' - ', s, crayon::green(' [complete]             '), sep = '')
  res_data
}
# helpers to turn xml node to tbl_df
data_node_to_tbl <- function(i, nodes) {
  node <- xml2::xml_children(nodes[[i]])
  node_nms <- xml2::xml_name(node)
  par_idx <- node_nms == 'Parameter'
  d <- as.list(c(xml2::xml_text(node[!par_idx]),
                 xml2::xml_attr(node[par_idx], attr = 'Value')))
  nms <- c(node_nms[!par_idx],
           xml2::xml_attr(node[par_idx], attr = 'Name'))
  nms <- tolower(gsub(' |\\.', '_', nms))
  tibble::as_tibble(setNames(d, nms), .name_repair = suffix_duped_names)
}
suffix_duped_names <- function(nms) {
  check <- table(nms)
  repeats <- check[check > 1]
  for (nm in names(repeats)) {
    idx <- which(nms == nm)
    nms[idx] <- paste0(nms[idx], '_', seq_along(idx))
  }
  nms
}
# encode and log read_xml urls
read_xml_url <- function(url_query, verbose) {
  # encode as url query
  url_query <- utils::URLencode(url_query)

  # downloard and parse
  if (verbose) {
    print(paste0('Fetching: ', url_query))
  }
  # catch service down HTTP error 503 codes, allow one retry
  res <- try(xml2::read_xml(url_query), silent = TRUE)
  if (inherits(res, 'try-error')) {
    res <- try(xml2::read_xml(url_query), silent = TRUE)
  }
  if (inherits(res, 'try-error')) {
    stop(res[[1]], '\nMight be a connection issue - check you have internet and try again in a minute. If this error persists please contact hayden@saltecology.co.nz')
  }
  res
}
