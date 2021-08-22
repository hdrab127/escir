#' Get site lists
#'
#' \code{htp_site_list} fetches site names and metadata from Hilltop
#'
#' @param url Hilltop webservice url, this points to the desired .hts file.
#' @param Location 'Yes' returns NZTM easting and northing. 'LatLong' latitude
#'   and longitude. NULL returns no location columns.
#' @param Measurement Filter to only sites with this measurement.
#' @param verbose boolean, whether to print the url query.
#' @param ... Currently unused.
#'
#' @return A \code{tbl_df} containing the site names and metadata.
#'
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#' htp_site_list('http://hilltop.gw.govt.nz/data.hts?Service=Hilltop',
#'               Location = 'LatLong')
#' #> # A tibble: 3,099 x 3
#' #>  site                                     lat   lng
#' #>  <chr>                                  <dbl> <dbl>
#' #>  292611                                  NA     NA
#' #>  29818                                   NA     NA
#' #>  Abbots Creek at D/S Donalds Crk Conf   -41.2  175.
#' #>  Abbots Creek at Featherston            -41.1  175.
#' #>  Abbots Creek at Lake Shore             -41.2  175.
#' #>  Abbots Creek at SH2 Bridge             -41.1  175.
#' #>  ...
#' }
#' @export
htp_site_list <- function(url,
                          Location = c('Yes', 'LatLong'),
                          Measurement = NULL,
                          verbose = FALSE,
                          ...) {
  query <-
    paste0(url, '&Request=SiteList')

  if (length(Location) == 1) {
    if (Location %in% c('Yes', 'LatLong')) {
      query <- paste0(query, '&Location=', Location)
    } else {
      stop('Location if specified must be one of c(\'Yes\', \'LatLong\')')
    }
  } else if (length(Location) > 1) {
    stop('Location if specified must be ONE of c(\'Yes\', \'LatLong\')')
  }

  # so we can run for multiple measurements
  site_list_internal <- function(m) {
    if (length(m) == 1) {
      htp_q <- paste0(query, '&Measurement=', m)
    }

    # get data
    res <-
      read_xml_url(query, verbose)

    # only keep site (drop agency and version)
    res <-
      xml2::xml_find_all(res, './/Site')

    # build tidy dataframe depending on locations
    if (length(Location) == 1) {
      if (Location == 'Yes') {
        htp_site_list_to_tbl <- function(node) {
          x <- xml2::as_list(node)
          tibble::tibble(site = attr(x, 'Name'),
                        nztm_e = if (is.null(x$Easting[[1]])) NA_character_ else x$Easting[[1]],
                        nztm_n = if (is.null(x$Northing[[1]])) NA_character_ else x$Northing[[1]])
        }
      } else if (Location == 'LatLong') {
        htp_site_list_to_tbl <- function(node) {
          x <- xml2::as_list(node)
          tibble::tibble(site = attr(x, 'Name'),
                        lat = if (is.null(x$Latitude[[1]])) NA_character_ else x$Latitude[[1]],
                        lng = if (is.null(x$Longitude[[1]])) NA_character_ else x$Longitude[[1]])
        }
      }
    } else {
      htp_site_list_to_tbl <- function(node) {
        tibble::tibble(site = xml2::xml_attr(node, 'Name'))
      }
    }
    # apply tidy df function to each node
    lapply(res, htp_site_list_to_tbl) %>%
      dplyr::bind_rows() %>%
      # convert location vars (if there) to numeric
      dplyr::mutate_at(dplyr::vars(-1L), as.double)
  }
  if (length(Measurement) > 0) {
    # run for each measurement
    lapply(Measurement,
           site_list_internal) %>%
      stats::setNames(nm = Measurement) %>%
      dplyr::bind_rows(.id = 'requestas')

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
#' @param ... Currently unused.
#'
#' @return A \code{tbl_df} containing the site metadata.
#'
#' @examples
#' \dontrun{
#' htp_site_info('http://hilltop.gw.govt.nz/data.hts?Service=Hilltop',
#'               Site = 'Abbots Creek at Featherston')
#' #> # A tibble: 1 x 7
#' #> inactive northing firstsynonym  recordingaut~ comment          metnumber easting
#' #> <chr>    <chr>    <chr>         <chr>         <chr>            <chr>     <chr>
#' #> 0        5445581  Abbots Creek~ GW-Eastern    "Site named \"F~ D15131    1795280
#' }
#' @export
htp_site_info <- function(url,
                          Site,
                          verbose = FALSE,
                          ...) {
  query <-
    paste0(url, '&Request=SiteInfo')
  query <-
    paste0(query, '&Site=', Site)

  # get data
  res <-
    read_xml_url(query, verbose)

  # only keep site
  res <-
    xml2::xml_find_all(res, './/Site')

  # build tidy dataframe
  # flatten then arrange
  single_node_to_tbl(res)
}

#' Get measurement information for sites
#'
#' \code{htp_measurement_list} fetches available measurements for the sites
#' requested.
#'
#' @param url Hilltop webservice url, this points to the desired .hts file.
#' @param Site Hilltop site name/s to query.
#' @param verbose boolean, whether to print the url query.
#' @param ... Currently unused.
#'
#' @return A \code{tbl_df} containing the sites and measurement metadata.
#'
#' @importFrom magrittr `%>%`
#'
#' @examples
#' \dontrun{
#' htp_measurement_list('http://hilltop.gw.govt.nz/data.hts?Service=Hilltop',
#'                      Site = 'Abbots Creek at Featherston')
#' #> # A tibble: 1 x 7
#' #> inactive northing firstsynonym   recordingauthor~ comment     metnumber easting
#' #> <chr>    <chr>    <chr>          <chr>            <chr>       <chr>     <chr>
#' #> 0        5445581  Abbots Creek ~ GW-Eastern       "Site name~ D15131    1795280
#' }
#' @export
htp_measurement_list <- function(url,
                                 Site = NULL,
                                 verbose = FALSE,
                                 ...) {
  query <-
    paste0(url, '&Request=MeasurementList')

  # so we can have multiple sites
  measurement_list_internal <- function(s) {
    if (length(s) == 1) {
      # TODO add valid site check
      htp_q <- paste0(query, '&Site=', s)
    } else {
      htp_q <- query
    }

    res <-
      read_xml_url(htp_q, verbose)

    if (length(s) == 0) {
      # only keep Measurements
      res <-
        xml2::xml_find_all(res, './/Measurement')

      # and output as sorted vector
      sapply(res, function(node) {
        xml2::xml_attr(node, 'Name')
      }, simplify = 'vector') %>%
        sort()

    } else {
      # only keep data sources
      res <-
        xml2::xml_find_all(res, './/DataSource')

      # tidy each node to dataframe
      lapply(res, function(node) {
        # each node can have multiple measurement nodes
        measures <-
          xml2::xml_find_all(node, './/Measurement') %>%
          lapply(single_node_to_tbl) %>%
          dplyr::bind_rows()

        # get rest of data
        x <- xml2::as_list(node)
        x <- x[-which(names(x) == 'Measurement')]

        # add site and contents
        x <-
          list(site = xml2::xml_attr(node, 'Site'), x) %>%
          unlist() %>%
          as.list() %>%
          dplyr::as_tibble()

        if (nrow(measures) > 0) {
          # rep for each row of measurements and bind cols
          dplyr::as_tibble(cbind(x, measures))
        } else {
          x
        }
      }) %>%
        dplyr::bind_rows() %>%
        dplyr::select_all(tolower)
    }
  }

  if (length(Site) > 0) {
    # run for each site
    # TODO parallelise for Windows
    lapply(Site, measurement_list_internal) %>%
      dplyr::bind_rows()

  } else {
    # run once
    measurement_list_internal(s = NULL)
  }
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
#' @param suppress_warnings TRUE (default) hides warnings for when there is no
#'   data for the parameters requested.
#' @param verbose boolean, whether to print the url query.
#' @param avg_by Hilltop averaging period, i.e. '1 hour', '1 day'
#' @param just_i1 faster parsing for continuous series
#' @param metadata whether to return metadata like units, series type etc.
#' @param ... Currently unused.
#'
#' @return A \code{tbl_df} containing the data for the sites-measurements-times
#'   requested.
#'
#' @importFrom rlang .data
#' @importFrom tidyr pivot_wider
#' @importFrom methods is
#' @importFrom stats setNames
#' @importFrom magrittr `%>%`
#'
#' @examples
#' \dontrun{
#' htp_get_data('http://hilltop.gw.govt.nz/data.hts?Service=Hilltop',
#'              Site = 'Owhiro Bay',
#'              From = '2019-03-01',
#'              To = '2019-07-01',
#'              Measurement = 'Enterococci Bacteria')
#' #> [1] "Enterococci Bacteria - Owhiro Bay"
#' #> # A tibble: 4 x 22
#' #> site  timestamp           value lab sample num~ method gwrc programme
#' #> <chr> <dttm>              <chr> <chr>            <chr>  <chr>
#' #> Owhi~ 2019-03-04 11:20:00 12    19/10788-21      Enter~ Rec WQ
#' #> Owhi~ 2019-03-11 11:15:00 4     19/11079-21      Enter~ Rec WQ
#' #> Owhi~ 2019-03-18 10:34:00 4     19/12378-21      Enter~ Rec WQ
#' #> Owhi~ 2019-03-25 11:02:00 24    19/13658-21      Enter~ Rec WQ
#' #> # ... with 16 more variables
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
                         suppress_warnings = TRUE,
                         just_i1 = FALSE,
                         metadata = TRUE,
                         ...) {
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
    dplyr::rename_with(site_data, ~ tolower(gsub(' |\\.', '_', .x))) %>%
      dplyr::rename(timestamp = .data$`t`) %>%
      dplyr::relocate(.data$site, .before = dplyr::everything())
  }
}

# encode and log read_xml urls
read_xml_url <- function(url_query, verbose) {
  # encode as url query
  url_query <-
    utils::URLencode(url_query)

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
    res_meta <- setNames(object = as.list(c(xml2::xml_text(res_meta[!info_idx]),
                                            xml2::xml_text(xml2::xml_contents(res_meta[info_idx])))),
                         nm = c(meta_nms[!info_idx],
                                xml2::xml_name(xml2::xml_contents(res_meta[info_idx]))))
    res_data <- dplyr::bind_cols(res_data, tibble::as_tibble(res_meta))
  }
  cat('\r', m, ' - ', s, crayon::green(' [complete]             '), sep = '')
  res_data
}
# helpers to turn xml node to tbl_df
single_node_to_tbl <- function(node) {
  xml2::as_list(node) %>%
    unlist() %>%
    as.list() %>%
    dplyr::as_tibble() %>%
    dplyr::select_all(tolower)
}
# FASTER helpers to turn xml node to tbl_df
data_node_to_tbl <- function(i, nodes) {
  node <- xml2::xml_children(nodes[[i]])
  node_nms <- xml2::xml_name(node)
  par_idx <- node_nms == 'Parameter'
  tibble::as_tibble(setNames(object = as.list(c(xml2::xml_text(node[!par_idx]),
                                                xml2::xml_attr(node[par_idx], attr = 'Value'))),
                             nm = c(node_nms[!par_idx],
                                    xml2::xml_attr(node[par_idx], attr = 'Name'))))
}
