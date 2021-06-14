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
      q <- paste0(query, '&Measurement=', m)
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
          dplyr::tibble(site = attr(x, 'Name'),
                        nztm_e = if (is.null(x$Easting[[1]])) NA_character_ else x$Easting[[1]],
                        nztm_n = if (is.null(x$Northing[[1]])) NA_character_ else x$Northing[[1]])
        }
      } else if (Location == 'LatLong') {
        htp_site_list_to_tbl <- function(node) {
          x <- xml2::as_list(node)
          dplyr::tibble(site = attr(x, 'Name'),
                        lat = if (is.null(x$Latitude[[1]])) NA_character_ else x$Latitude[[1]],
                        lng = if (is.null(x$Longitude[[1]])) NA_character_ else x$Longitude[[1]])
        }
      }
    } else {
      htp_site_list_to_tbl <- function(node) {
        dplyr::tibble(site = xml2::xml_attr(node, 'Name'))
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
      q <- paste0(query, '&Site=', s)
    } else {
      q <- query
    }

    res <-
      read_xml_url(q, verbose)

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
                         ...) {
  query <-
    paste0(url, '&Request=GetData')


  if (length(tstype) == 1) {
    query <-
      paste0(query, '&tstype=', tstype)
  }

  # so we can run for multiple measures/sites
  # ... consumes mc.cores arg if not parallel
  get_data_internal <- function(m, s, ...) {
    cat('\n', m, ' - ', s, crayon::yellow(' [fetching from server]'), sep = '')
    q <- paste0(query, '&Measurement=', m)
    q <- paste0(q, '&Site=', s)

    # if no dates specified hilltop returns all or default
    if (length(From) == 0 & length(To) == 0) {
      readline('No date range specified, this will request all data and can be slow.\nHit [Enter] to continue, or Ctrl-c to abort')
    } else if (length(From) > 1 & length(To) > 1) {
      stop('Multiple values are not allowed for From or To')
    }
    # else if (length(From) == 0) {
    #   stop('If To is specified, From must be also.')
    # }

    # test if inputs are valid strings if not dates.
    valid_date_re <- '^(-?(?:[1-9][0-9]*)?[0-9]{4})-(1[0-2]|0[1-9])-(3[01]|0[1-9]|[12][0-9])$'
    valid_datetime_re <-
      paste0(valid_date_re,
             '(T| ){1}',
             '(2[0-3]|[01][0-9]):([0-5][0-9]):([0-5][0-9])(.[0-9]+)?(Z)?')

    if (length(From) == 1) {
      if (is(From, 'Date') |
          inherits(From, 'POSIXct') |
          grepl(valid_date_re, From) |
          grepl(valid_datetime_re, From)) {
        # all good
        q <-
          paste0(q,
                 '&From=', From)
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
    q <-
      paste0(q,
             '&To=', To)

    # if average, add after
    if (!is.null(avg_by)) {
      q <-
        paste0(q, '&Method=Average&Interval=', avg_by)

    }

    # fetch data
    res <-
      read_xml_url(q, verbose)

    # check xml errors
    res_error <-
      xml2::xml_find_all(res, './/Error')
    if (length(res_error) > 0) {
      cat('\r', m, ' - ', s, crayon::red(' [no data found]       '), sep = '')
      return(dplyr::tibble())
    }

    # get tidy datasource table
    res_datasource <-
      xml2::xml_find_all(res, './/DataSource') %>%
      single_node_to_tbl()
    if ('iteminfo.itemname' %in% names(res_datasource)) {
      res_datasource <-
        res_datasource %>%
        dplyr::rename(key = .data$iteminfo.itemname)
    }

    # get data nodes
    res_data <-
      xml2::xml_find_all(res, './/Data') %>%
      xml2::xml_children()

    # check has data
    res_length <- length(res_data)
    if (res_length == 0) {
      cat('\r', m, ' - ', s, crayon::red(' [no data found]       '), sep = '')
      return(dplyr::tibble())
    }

    # xml2::as_list is the slow part...
    cat('\r', m, ' - ', s, crayon::yellow(' [processing into R]   '), sep = '')
    res_data <-
      res_data %>%
      xml2::as_list() %>%
      setNames(nm = paste0('row-', seq_len(res_length))) %>%
      lapply(tibble::enframe) %>%
      Filter(f = function(x) nrow(x) > 0) %>%
      dplyr::bind_rows(.id = 'id')
    res_values <-
      dplyr::filter(res_data, .data$name != 'Parameter') %>%
      tidyr::unnest(.data$value) %>%
      tidyr::unnest(.data$value) %>%
      tidyr::pivot_wider()
    res_pars <-
      dplyr::filter(res_data, .data$name == 'Parameter')
    if (nrow(res_pars) > 0) {
      res_pars <-
        res_pars %>%
        dplyr::mutate(value = purrr::map(.data$value, attributes),
                      name = tolower(purrr::map(.data$value, 'Name')),
                      name = gsub(' ', '_', .data$name),
                      value = purrr::map(.data$value, 'Value')) %>%
        tidyr::unnest(c(.data$name, .data$value)) %>%
        dplyr::distinct() %>%
        dplyr::group_by(.data$id, .data$name) %>%
        dplyr::summarise(value = paste(.data$value, collapse = ';')) %>%
        dplyr::ungroup() %>%
        tidyr::pivot_wider()
      res_data <-
        dplyr::full_join(res_values, res_pars, by = 'id')
    } else {
      res_data <-
        res_values
    }
    res_data <-
      res_data %>%
      dplyr::select_all(function(x) tolower(gsub(' ', '_', x))) %>%
      dplyr::rename(timestamp = .data$`t`)
    res_data$site <- s

    # combine data with metadata
    if (nrow(res_datasource) > 0) {
      # rep for each row and bind cols
      res_datasource <-
        dplyr::select_all(res_datasource, function(x) tolower(gsub(' |\\.', '_', x)))
      res_data <-
        dplyr::bind_cols(res_data, res_datasource)
    }
    cat('\r', m, ' - ', s, crayon::green(' [complete]             '), sep = '')
    res_data
  }

  # run for each site
  get_data_measure <- function(m) {
    dplyr::bind_rows(lapply(Site, get_data_internal, m = m))
  }
  site_data <-
    lapply(Measurement, get_data_measure) %>%
    dplyr::bind_rows()
  cat('\n')

  if (nrow(site_data) == 0) {
    return(dplyr::tibble())
  }
  site_data %>%
    dplyr::select(.data$site, dplyr::everything())
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
             stop('GW Hilltop web service might be down - check your connection and try again in a minute. If this error persists please contact hayden.rabel@gw.govt.nz')
           })
}
# helper to turn xml node to tbl_df
single_node_to_tbl <- function(node) {
  xml2::as_list(node) %>%
    unlist() %>%
    as.list() %>%
    dplyr::as_tibble() %>%
    dplyr::select_all(tolower)
}
