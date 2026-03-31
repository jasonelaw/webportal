

# Formatting helpers ----------------------------------------------------------
drop_status <- function(x) {
  if (rlang::has_name(x, "ResponseStatus")) {
    x["ResponseStatus"] <- NULL
  }
  if (rlang::has_name(x, "disclaimers")) {
    x["disclaimers"] <- NULL
  }
  x
}

coerce_timestamps <- function(x) {
  time_probes <- c("timestamp", "StartOf", "EndOf", "LastUpdates")
  x |>
    dplyr::mutate(
      dplyr::across(dplyr::contains(time_probes), parse_timestamp)
    )
}

# rename_wp <- function(x) {
#   UseMethod("rename_wp")
# }
#
# rename_wp.sf <- function(x) {
#
# }
rename_nested <- function(x) {
  rename_list <- function(x) {
    if (is.null(x)) {
      x
    } else if (is.data.frame(x)) {
      dplyr::rename_with(x, snakecase::to_snake_case)
    } else if (is.list(x)) {
      setNames(x, snakecase::to_snake_case(names(x)))
    } else {
      x
    }
  }
  x |>
    dplyr::mutate(
      dplyr::across(
        dplyr::where(is.list),
        \(x) map(x, rename_list)
      )
    )
}
rename_wp <- function(x) {
  rename <- getOption("webportal.rename")
  is_sf <- inherits(x, "sf")
  fun <- if (is_sf) sf:::rename_with.sf else dplyr::rename_with

  if (rename) {
    x <- x |>
      fun(snakecase::to_snake_case) |>
      rename_nested()
  }
  x
}

drop_one_platform <- function(x) {
  dplyr::select(x, -dplyr::contains("OnePlatform"))
}

parse_timestamp <- function(x) {
  allna <- all(is.na(x))
  if(allna){
    return(as.POSIXct(x, tz = "America/Los_angeles"))
  }
  if(lubridate::is.POSIXct(x)) return(x)
  ret <- lubridate::fast_strptime(x, "%Y-%m-%dT%H:%M:%OS%z", lt = FALSE)
  lubridate::with_tz(ret, tzone = Sys.timezone())
}

convert_time <- function(x, fields) {
  x |>
    dplyr::mutate(
      dplyr::across(.cols = dplyr::any_of(fields), .fns = parse_timestamp)
    )
}

unnest_wider_namevalue <- function(x, col, value_col = "value", name_col = "name") {
  f <- function(x, name_col, value_col){
    has_value <- rlang::has_name(x, value_col)
    if(has_value){
      tidyr::deframe(x[, c(name_col, value_col)])
    } else {
      list(NULL)
    }
  }
  x |>
    dplyr::mutate(
      "{{ col }}" := f({{ col }},
      name_col = name_col,
      value_col = value_col)
    ) |>
    tidyr::unnest_wider(col = {{ col }})
}


# Web Portal Responses ---------------------------------------------------------
#' @export
format_response <- function(x, ...) {
  UseMethod("format_response")
}

#' @export
format_response.wp_response <- function(x, query,  ...) {
  ret <- tibble::tibble(
    response = simd_parse(x$.response, query = query, ...),
    .req_id = x$.req_id
  ) |>
    tidyr::unnest(response) |>
    drop_one_platform()
  ret
}

#' @export
format_response.version <- function(x) {
  unlist(simd_parse(x$.response, query = "/webPortalVersion"))
}

## Filters -----------------------------------------
#' @export
format_response.filter <- function(x) {
  format_response.wp_response(x, "/filters") |>
    rename_wp()
}


## Locations -----------------------------------------
#' @export
format_response.wplocation <- function(x, multiple = FALSE) {
  ret <- tibble::tibble(
    location =  simd_parse(x$.response, query = "/location", max_simplify_lvl = 0L),
    .req_id = x$.req_id
  ) |>
    tidyr::unnest_wider(location) |>
    tidyr::unnest_wider(elevationUnit, names_sep = "_") |>
    drop_one_platform() |>
    unnest_wider_namevalue(extendedAttributes, "value", "name") |>
    rename_wp()
  type.convert(ret, as.is = TRUE)
}

#' @export
format_response.wplocations <- function(x, multiple = FALSE) {
  ret <- tibble::tibble(
    location = simd_parse(x$.response, query = "/locations", max_simplify_lvl = 0L),
    .req_id = x$.req_id
  )  |>
    tidyr::unnest(location) |>
    drop_one_platform() |>
    tidyr::unnest_wider(elevationUnit, names_sep = "_")|>
    unnest_wider_namevalue(extendedAttributes, "value", "name") |>
    rename_wp()
  type.convert(ret, as.is = TRUE)
}

## Dataset -----------------------------------------
#' @export
format_response.dataset <- function(x) {
  format_response.wp_response(x, query = "/datasets") |>
    rename_wp()
}

#' @export
format_response.lateststatdef <- function(x) {
  format_response.wp_response(x, query = "/latestStatistics") |>
    tidyr::unnest_wider(unit, names_sep = "_") |>
    rename_wp()
}

#' @export
format_response.lateststatistic <- function(x) {
  format_response.wp_response(
    x,
    query = "/latestStatisticValues"
  ) |>
    tidyr::hoist(
      statistic,
      statistic_id = "id",
      parameter = "parameter",
      unit = c("unit", "symbol")
    ) |>
    dplyr::select(-statistic) |>
    rename_wp()
}

#'@export
format_response.geojson <- function(x) {
  fmt_resp <- function(x, id) {
    sf::st_read(
      dsn = httr2::resp_body_string(x),
      as_tibble = TRUE,
      quiet = TRUE
    ) |>
      coerce_timestamps() |>
      dplyr::bind_cols(.req_id = id)
  }
  ret <- dplyr::bind_rows(purrr::map2(x$.response, x$.req_id, fmt_resp))
  rename <- getOption("webportal.rename")
  if (rename) {
    ret <- ret |>
      sf:::rename_with.sf(snakecase::to_snake_case)
  }
  ret
}

#' @export
format_response.export <- function(x) {
  pointers <- c(
    dataset = "/dataset",
    timeRange = "/timeRange",
    numPoints = "/numPoints",
    points = "/points"
  )
  ret <- tibble::new_tibble(
    tibble::tibble(
      export = simd_parse(x$.response, pointers),
      .req_id = x$.req_id
    )
  ) |>
    tidyr::unnest_wider(export) |>
    tidyr::unnest_wider(dataset)
  if (any(!is.na(ret$timeRange))) {
    ret <- ret |>
      tidyr::unnest_wider(timeRange) |>
      convert_time(c("startTime", "endTime"))
  }
  ret <- ret |>
    dplyr::mutate(
      points = purrr::map_if(
        points,
        \(x) !is.null(x),
        \(x) tibble::as_tibble(x) |>
          convert_time(c("timestamp", "eventTimestamp"))
      )
    ) |>
    rename_wp()
  ret
}


#' @export
format_response.bulk <- function(x) {
  ret <- format_response.wp_response(x, "/series")
  ret <- ret |>
    tidyr::unnest_wider(dataset) |>
    tidyr::unnest_wider(timeRange) |>
    dplyr::mutate(
      points = purrr::map_if(
        points,
        \(x) !is.null(x),
        \(x) convert_time(x, "timestamp")
      )
    ) |>
    convert_time(c("startTime", "endTime")) |>
    rename_wp()
  ret
}

#' @export
format_response.aligned <- function(x) {
  pointer <- c(
    datasets = "/datasets",
    startTime = "/timeRange/startTime",
    endTime = "/timeRange/endTime",
    rows = "/rows"
  )
  ret <- simd_parse(x$.response, query = pointer)
  ret <- tibble::tibble(aligned = ret, .req_id = x$.req_id) |>
    tidyr::unnest_wider(aligned) |>
    convert_time(c("startTime", "endTime"))
  rows <- purrr::list_rbind(ret$rows) |>
    tidyr::unnest(points)
  ret |>
    dplyr::select(-rows) |>
    tidyr::unnest(datasets) |>
    dplyr::left_join(
      y = rows,
      by = dplyr::join_by(identifier == dataset)
    ) |>
    tidyr::nest(
      points = !one_of(c(
        "identifier", "parameter", "label", "unit", "locationIdentifier",
        "startTime", "endTime"
      ))
    ) |>
    rename_wp()
}


