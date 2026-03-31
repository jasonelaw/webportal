# Web Portal Routes ------------------------------------------------------------
#'Web Portal API
#'
#' See the web portal API documentation for arguments.
#' @param ... pass query arguments to route. Please see Web Portal API
#' documenation for available arguments.
#' @param .perform if `TRUE`, perform the request, otherwise return a
#' `httr2::request` object
#' @param .format if `TRUE`, return a `data.frame`, otherwise return a
#' `httr2::response` object. Ignored if `.perform = FALSE`

#' @name webportal-routes
NULL


# Version ------------------------------
#' @describeIn webportal-routes Get version of API
#' @export
version <- function(.perform = TRUE, .format = TRUE) {
  webportal(
    .path = "version", .class = "version",
    .perform = .perform, .format = .format
  )
}

## Filters --------------------------------------------------------------------
#' @describeIn webportal-routes Get filters for map endpoints
#' @export
get_filters <- function(.perform = TRUE, .format = TRUE) {

  webportal(
    .path    = "filters",
    .class   = "filter",
    .perform = .perform,
    .format  = .format
  )
}
## Locations -------------------------------------------------------------------

#' @describeIn webportal-routes Get a location
#' @export
get_location <- function(location, ..., .format = TRUE, .perform = TRUE) {

  rlang::check_required(location)
  location <- URLencode(location)

  webportal(
    ...,
    .path = "locations",
    .template = "{location}",
    .template_vars = rlang::list2(
      location = location
    ),
    .class = "wplocation",
    .perform = .perform,
    .format = .format
  )
}

#' @describeIn webportal-routes Get locations
#' @export
get_locations <- function(
  location = NULL,
  active = NULL,
  .format = TRUE,
  .perform = TRUE
) {

  webportal(
    location = location,
    active   = active,
    .path    = "locations",
    .class   = "wplocations",
    .perform = .perform,
    .format  = .format
  )
}

## Dataset ---------------------------------------------------------------------
#' @describeIn webportal-routes Get datasets
#' @export
get_dataset <- function(dataset, .perform = TRUE, .format = TRUE) {
  rlang::check_required(dataset)
  .fun <- function(dataset, .perform, .format) {
    args <- list(.path = "data-set", .class = "dataset", .perform = FALSE)
    eval(rlang::call2(webportal, dataset = dataset, !!!args))
  }

  vector_call(.fun, .perform, .format, dataset = dataset)
}

## Latest Statistic Definitions ------------------------------------------------

#' @describeIn webportal-routes Get Latest Statistics
#' @export
get_latest_stat <- function(
  parameter = NULL,
  statistic = NULL,
  active = NULL,
  .perform = TRUE,
  .format = TRUE
) {

  webportal(
    parameter = parameter,
    statistic = statistic,
    active = active,
    .path = c("statistics", "latest"),
    .class = "lateststatdef",
    .perform = .perform,
    .format = .format
  )
}

## Latest Statistics Values ----------------------------------------------------

#' @describeIn webportal-routes Get Latest Statistics
#' @export
get_latest_stat_values <- function(
  parameter = NULL,
  statistic = NULL,
  location = NULL,
  ...,
  .format = TRUE,
  .perform = TRUE
) {

  webportal(
    parameter = parameter,
    statistic = statistic,
    location  = location,
    ...,
    .path     = "statistic-values/latest",
    .class    = "lateststatistic",
    .perform  = .perform,
    .format   = .format
  )
}

## Maps -----------------------------------------------------------------------

#' @describeIn webportal-routes List all locations
#' @export
get_map_locations <- function(..., .perform = TRUE, .format = TRUE) {

  webportal(
    ...,
    .path    = c("map", "locations"),
    .class   = "geojson",
    .perform = .perform,
    .format  = .format
  )
}

#' @describeIn webportal-routes List timeseries by parameter; return sf object
#' @export
get_map_datasets <- function(parameter, ..., .perform = TRUE, .format = TRUE) {

  rlang::check_required(parameter)

  parameter <- URLencode(parameter)

  webportal(
    ...,
    .path     = "map/datasets",
    .template = "{parameter}",
    .template_vars = list(
      parameter = parameter
    ),
    .class    = "geojson",
    .perform  = .perform,
    .format  = .format
  )
}

#' @describeIn webportal-routes Get latest statistics; return sf object
#' @export
get_map_latest_stat <- function(
  parameter,
  statistic,
  ...,
  .perform = TRUE,
  .format = TRUE
) {

  rlang::check_required(parameter)
  rlang::check_required(statistic)

  parameter <- URLencode(parameter)
  statistic <- URLencode(statistic)

  webportal(
    ...,
    .path = "map/statistics/latest",
    .template = "{parameter}/{statistic}",
    .template_vars = list(
      parameter = parameter,
      statistic = statistic
    ),
    .class = "geojson",
    .perform = .perform,
    .format = .format
  )
}

#' @describeIn webportal-routes Get periodic statistics; return sf object
#' @export
get_map_periodic_stat <- function(
  parameter,
  statistic,
  interval,
  date, ...,
  .perform = TRUE,
  .format = TRUE
) {
  .fun <- function(parameter, statistic, interval, date, ...) {
    webportal(
      ...,
      .path = "/map/statistics/periodic",
      .template = "/{parameter}/{statistic}/{interval}/{date}",
      .template_vars = list(
        parameter = parameter,
        statistic = statistic,
        interval = interval,
        date = date
      ),
      .class = "geojson",
      .perform = FALSE
    )
  }

  parameter <- rlang::check_required(parameter)
  statistic <- rlang::check_required(statistic)
  interval  <- rlang::check_required(interval)
  date      <- rlang::check_required(date)

  parameter <- URLencode(parameter)
  statistic <- URLencode(statistic)
  interval  <- URLencode(interval)
  date      <- URLencode(date)

  vector_call(
    parameter = parameter,
    statistic = statistic,
    interval = interval,
    date = date,
    ...,
    .f = .fun,
    .perform = .perform,
    .format = .format
  )
}

## Export ----------------------------------------------------------------------

#' @describeIn webportal-routes Export a data set
#' @export
export_dataset <- function(dataset, ..., .perform = TRUE, .format = TRUE) {
  rlang::check_required(dataset)

  .fun <- function(dataset, ...) {
    args <- list(
      .path    = c("export", "data-set"),
      .class   = "export",
      .perform = FALSE
    )
    call <- rlang::call2(webportal, dataset = dataset, ..., !!!args)
    eval(call)
  }

  vector_call(.fun, .perform, .format, dataset = dataset, ...)
}

#' @describeIn webportal-routes Export a periodic statistic
#' @export
export_periodic_stat <- function(
  dataset,
  statistic,
  calendar,
  interval,
  ...,
  .perform = TRUE,
  .format = TRUE
) {

  rlang::check_required(dataset)
  rlang::check_required(statistic)
  rlang::check_required(calendar)
  rlang::check_required(interval)

  .fun <- function(dataset, statistic, calendar, interval, ...) {
    args <- list(
      .path = c("export", "data-set"), .class = "export", .perform = FALSE
    )

    cll <- rlang::call2(
      .fn = webportal, dataset = dataset, statistic = statistic,
      calendar = calendar, interval = interval, ..., !!!args
    )
    eval(cll)
  }

  vector_call(
    .fun,
    .perform,
    .format,
    dataset = dataset,
    statistic = statistic,
    calendar = calendar,
    interval = interval,
    ...,
  )

}



#' @describeIn webportal-routes Export seasonal statistics
#' @export
export_seasonal_stat <- function(
  dataset,
  interval,
  statistic,
  referenceperiod = referenceperiod,
  ...,
  .perform = TRUE,
  .format = TRUE
) {

  rlang::check_required(dataset)
  rlang::check_required(interval)
  rlang::check_required(statistic)
  rlang::check_required(referenceperiod)

  .fun <- function(dataset, interval, statistic, referenceperiod, ...) {
    args <- list(
      .path = c("export", "seasonal-statistic"), .class = "export",
      .perform = FALSE
    )

    call <- rlang::call2(
      .fn = webportal,
      dataset = dataset,
      interval = interval,
      statistic = statistic,
      referenceperiod = referenceperiod,
      ...,
      !!!args
    )
    eval(call)
  }

  vector_call(
    .fun,
    .perform,
    .format,
    dataset = dataset,
    interval = interval,
    statistic = statistic,
    referenceperiod = referenceperiod,
    ...
  )
}

check_bulk_args <- function(interval, daterange, call = rlang::caller_env()) {

  rlang::check_required(datasets, call = call)
  rlang::check_exclusive(daterange, starttime, .frame = call)
  rlang::check_exclusive(daterange, starttime, .frame = call)

  rlang::arg_match0(
    arg = interval,
    values = c(
      "PointsAsRecorded", "Minutely", "Hourly",
      "Daily", "Monthly", "Yearly"
    ),
    error_call = call
  )
  if (!is.null(daterange)) {
    rlang::arg_match0(
      arg = daterange,
      values = c("Today", "Days7", "Days30", "Months6", "Years1"),
      error_call = call,

    )
  }
}

#' @describeIn webportal-routes Export several time-aligned data sets.
#' @export
export_time_aligned <- function(
  datasets,
  starttime = NULL,
  endtime = NULL,
  daterange = NULL,
  interval = "PointsAsRecorded",
  step = 1,
  includeGradeCodes = TRUE,
  includeQualifers = TRUE,
  includeApprovalLevels = TRUE,
  ...,
  .perform = TRUE,
  .format = TRUE
) {

  check_bulk_args(interval, daterange)

  args <- rlang::list2(...,
    datasets = datasets,
    starttime = starttime,
    endtime = endtime,
    daterange = NULL,
    interval = interval,
    step = step,
    includeGradeCodes = includeGradeCodes,
    includeQualifers = includeQualifers,
    includeApprovalLevels = includeApprovalLevels
  )

  webportal(
    .body    = args,
    .path    = c("export", "time-aligned"),
    .class   = "aligned",
    .perform = .perform,
    .format  = .format
  )
}

#' @describeIn webportal-routes Export datasets in bulk; optionally summarize
#' @export
export_bulk <- function(
  datasets,
  starttime,
  endtime,
  daterange = NULL,
  interval = "PointsAsRecorded",
  step = 1,
  includeGradeCodes = TRUE,
  includeQualifers = TRUE,
  includeApprovalLevels = TRUE,
  ...,
  .perform = TRUE,
  .format = TRUE
) {

  check_bulk_args(interval, daterange)

  args <- rlang::list2(...,
    datasets  = datasets,
    starttime = starttime,
    endtime = endtime,
    daterange = daterange,
    interval  = interval,
    step      = step,
    includeGradeCodes     = includeGradeCodes,
    includeQualifers      = includeQualifers,
    IncludeApprovalLevels = includeApprovalLevels
  )

  ret <- webportal(
    .path = c("export", "bulk"),
    .body = args,
    .class = "bulk",
    .perform = .perform,
    .format = .format
  )
  ret
}

#'Returns an object suitable for the Datasets argument of the bulk export
#'endpoints: `export_bulk` and `export_time_aligned`.
#'@export
Datasets <- function(Identifier, Calculation = NULL, Unit = NULL){
  if (!is.null(Calculation)){
    choices <- c(
      "Instantaneous", "Maximum", "Minimum", "Aggregate",
      "Integrate", "Differentiate"
    )
    stopifnot(Calculation %in% choices)
  }
  tibble::tibble(Identifier, Calculation, Unit)
}
