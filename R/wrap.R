# Web Portal API Wrapper -------------------------------------------------------

wp_get_url  <- make_get_env("AQUARIUS_WEBPORTAL_URL")
wp_get_user <- make_get_env("AQUARIUS_WEBPORTAL_USER")
wp_get_pw   <- make_get_env("AQUARIUS_WEBPORTAL_PW")

rlang::on_load({
  check_host_reachable(wp_get_url())
})

webportal <- function(
    ...,
    .path = NULL,
    .template = NULL,
    .template_vars = NULL,
    .body = NULL,
    .class = NULL,
    .perform = FALSE,
    .format = FALSE,
    .url = wp_get_url(),
    .username = wp_get_user(),
    .password = wp_get_pw(),
    .multi = "explode"
) {

  verbose <- getOption("webportal.verbose")

  params <- rlang::list2(...)

  req <- httr2::request(.url) |>
    httr2::req_url_path_append("api", "v1", .path) |>
    httr2::req_user_agent("https://github.com/jasonelaw/raquarius") |>
    httr2::req_auth_basic(.username, .password) |>
    httr2::req_url_query(!!!params, .multi = .multi) |>
    httr2::req_headers(
      `Accept-Encoding` = "gzip"
    ) |>
    httr2::req_error(body = wp_req_error)

  if (!is.null(.template)) {
    req <- req |>
      httr2::req_template(.template, !!!.template_vars)
  }

  if (!is.null(.body)) {
    req <- httr2::req_body_json(req, .body)
  }

  if (verbose) {
    req <- httr2::req_verbose(req)
  }

  ret <- new_wp_request(req, class = .class)

  if (.perform) {
    ret <- perform_wp_request(ret)
    if (.format) {
      ret <- format_response(ret)
    }
  }

  ret
}

new_wp_request <- function(x, class = NULL) {
  if (rlang::is_missing(x)) {
    x <- httr2::request(wp_get_url())
  }
  stopifnot(
    "`x` must be an `httr2_request`" = inherits(x, "httr2_request"),
    "`class` must be a scalar character or `NULL`" =
      rlang::is_scalar_character(class) || rlang::is_null(class)
  )
  tibble::new_tibble(
    tibble::tibble_row(.req_id = rlang::hash(x), .request = list(x)),
    class = c(class, "wp_request")
  )
}

new_wp_response <- function(x, class = NULL) {
  if(rlang::is_missing(x)) {
    x <- tibble::tibble(
      .req_id := character(),
      .request = list(NULL),
      .response = list(NULL)
    )
  }
  stopifnot(
    tibble::is_tibble(x),
    tibble::has_name(x, ".req_id"),
    tibble::has_name(x, ".request"),
    tibble::has_name(x, ".response")
  )

  tibble::new_tibble(x, class = c(class, "wp_response"))
}

validate_wp_response <- function(x) {
  stopifnot(
    "`x` must be a tibble" = tibble::is_tibble(x),
    "`x` must have a field `.req_id`" = tibble::has_name(x, ".req_id"),
    "`x` must have a field `.request`" = tibble::has_name(x, ".request"),
    "`x` must have a field `.response`" = tibble::has_name(x, ".response"),
    "`x$.request` must be a list of `httr2_request` objects"  =
      purrr::map_lgl(x$.request, \(x) inherits(x, "httr2_request")),
    "`x$.response` must be a list of `httr2_response` objects" =
      purrr::map_lgl(x$.response, \(x) inherits(x, "httr2_response"))
  )
  x
}

perform_wp_request <- function(x, ...) {
  webportal.parallel <- getOption("webportal.parallel")
  stopifnot(
    "The `webportal.parallel` option must be logical" =
      is.logical(webportal.parallel)
  )
  cls <- class(x)[1]
  n <- nrow(x)
  if (n > 1L && webportal.parallel) {
    x$.response <- httr2::req_perform_parallel(
      x$.request,
      progress = "Completing requests to Web Portal",
      ...
    )
  } else {
    x$.response <- httr2::req_perform_sequential(x$.request)
  }

  ret <- new_wp_response(x, class = cls)
}

#' @export
print.wp_request <- function(x, ...) {
  NextMethod()
}
resp_status <- function(x) {
  purrr::map_int(x$.response, httr2::resp_status)
}

check_status <- function(x) {
  status <- resp_status(x)
}

wp_req_error <- function(x) {
  status <- httr2::resp_body_json(x)$responseStatus
  paste0("errorCode: ", status$erorCode, " errorMessage: ", status$message, status$meta)
}
