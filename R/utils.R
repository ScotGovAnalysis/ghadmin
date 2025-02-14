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


#' Knit Rmarkdown file
#'
#' @param rmd_path Character string of file path to Rmarkdown file.
#' @param params List of parameters to pass to Rmd file.
#'
#' @return Character string of Rmarkdown content.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' knit_rmd(here("confirm-membership.Rmd"), list(date = "1 March 2025"))
#' }

knit_rmd <- function(rmd_path, params) {

  knitr::knit_child(rmd_path, envir = environment(), quiet = TRUE)

}
