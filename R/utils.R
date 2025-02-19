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


#' User input to confirm opening review issues
#'
#' @param n_members Number of issues to be opened
#'
#' @return If user responds 'y', TRUE is returned invisibly. This allows
#'  following code to run. If user responds 'n', an error will be returned. This
#'  stops following code from running. If user responds something else, they
#'  will be prompted to retry.
#'
#' @export
#'
#' @examples
#' \dontrun{confirm_to_continue(100)}

confirm_to_continue <- function(n_members) {

  response <- readline(prompt = glue::glue(
    "Do you want to proceed to open GitHub issues for {n_members} members ",
    "(y/n)? "
  ))

  response <- tolower(trimws(response))

  if (response == "y") {
    invisible(TRUE)
  } else if (response == "n") {
    cli::cli_abort(
      "Process stopped. Issues will not be opened."
    )
  } else {
    cat("Invalid input. Please enter 'y' or 'n'.\n")
    confirm_to_continue(n_members)
  }

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
