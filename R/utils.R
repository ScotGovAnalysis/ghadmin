check_length <- function(x,
                         length = 1,
                         error_env = rlang::caller_env(),
                         error_arg = rlang::caller_arg(x)) {

  if (length(x) != length) {
    cli::cli_abort("{.arg {error_arg}} must be length {length}.")
  } else {
    invisible(x)
  }

}

possibly_hoist <- function(..., otherwise = NULL) {

  purrr::possibly(tidyr::hoist, otherwise)(...)

}
