check_arg <- function(x,
                      length = 1,
                      class = "character",
                      error_env = rlang::caller_env(),
                      error_arg = rlang::caller_arg(x)) {

  if (!is.null(length) & length(x) != length) {
    cli::cli_abort("{.arg {error_arg}} must be length {length}.",
                   call = error_env)
  }

  if (!is.null(class) & class(x) != class) {
    cli::cli_abort("{.arg {error_arg}} must be of class {class}.",
                   call = error_env)
  }

  invisible(x)

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
