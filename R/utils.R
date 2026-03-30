.onLoad <- function(libname, pkgname) {
  rlang::run_on_load()
}

rlang::on_load({
  options(
    "webportal.verbose" = FALSE,
    "webportal.rename"  = FALSE,
    "webportal.parallel" = FALSE
  )
})


check_host_reachable <- function(url) {
  url <- httr2::url_parse(url)
  dns <- curl::nslookup(url$hostname, error = FALSE)
  if(is.null(dns)) {
    cli::cli_abort(
      message = "Cannot reach host {url$hostname}. Check your internet or VPN connection if attempting to reach a private network.",
      call = caller_env(n = 2)
    )
  }
}

make_get_env <- function(keyname, signal_error = TRUE) {
  function() {
    key <- Sys.getenv(keyname, unset = NA)
    if (is.na(key) && signal_error) {
      msg <- glue::glue("{ keyname } key not found, please set { keyname } env var using Sys.setenv or .Rprofile")
      stop(msg)
    }
    key
  }
}

vector_call <- function(.f, .perform, .format, ...) {
  args <- tibble::as_tibble(rlang::list2(...))
  args <- dplyr::bind_cols(
    purrr::list_rbind(purrr::pmap(args, .f = .f)),
    args
  ) |>
    dplyr::relocate(dplyr::starts_with("."), .after = dplyr::everything())
  if (.perform) {
    args <- perform_wp_request(args)
    if (.format) {
      args <- format_response(args)
    }
  }
  args
}
