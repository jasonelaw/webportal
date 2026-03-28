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

  verbose <- getOption("raquarius.verbose")

  params <- rlang::list2(...)

  req <- httr2::request(.url) |>
    httr2::req_url_path_append("api", "v1", .path) |>
    httr2::req_user_agent("https://github.com/jasonelaw/raquarius") |>
    httr2::req_auth_basic(.username, .password) |>
    httr2::req_url_query(!!!params, .multi = .multi) |>
    httr2::req_headers(
      `Accept-Encoding` = "gzip"
    )

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
    ret <- req_perform_wp(ret)
    if (.format) {
      ret <- format_response(ret)
    }
  }

  ret
}

new_wp_request <- function(x, class) {
  stopifnot(
    "`x` must be an `httr2_request`" = inherits(x, "httr2_request")
  )
  tibble::new_tibble(
    tibble::tibble_row(.req_id = rlang::hash(x), .request = list(x)),
    class = c(class, "wp_request")
  )
}

#' @export
print.wp_request <- function(x, max = 5) {
  NextMethod()
}

new_wp_response <- function(x, df = tibble(), class) {

  is_response <- inherits(x, "httr2_response")
  x <- if(is_response) list(x) else x

  stopifnot(
    "x must be an `httr2_response` object or a list of responses" =
      is_list_of_responses(x)
  )

  tibble::new_tibble(
    dplyr::reframe(df, response = x),
    class = c(class, "wp_response")
  )
}

resp_status <- function(x) {
  map_int(x$response, httr2::resp_status)
}

req_perform_wp2 <- function(x, .format, max_active = 10, on_error = "stop") {

  if (inherits(x, "httr2_request")) {
    x <- list(x)
  }

  stopifnot(
    "`x` must be an httr2_request or a list of them" = is_list_of_requests(x)
  )

  cls <- class(x[[1]])[1]

  if (identical(length(x), 1L)) {
    ret <- httr2::req_perform(x[[1]])
  } else if (is_list_of_requests(x)) {
    ret <- httr2::req_perform_parallel(
      x,
      on_error = on_error,
      max_active = max_active,
      progress = "Performing requests"
    )
  }

  new_wp_response(ret, class = cls)
}

req_perform_wp <- function(x, ...) {
  cls <- class(x)[1]
  n <- nrow(x)
  if (identical(n, 1L)){
    x$.response <- httr2::req_perform_sequential(x$.request)
  } else {
    x$.response <- httr2::req_perform_parallel(
      x$.request,
      progress = "Completing requests to Web Portal",
      ...
    )
  }
  new_tibble(unclass(x), class = c(cls, "wp_response"))
}
