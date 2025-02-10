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


#' Parse markdown file
#'
#' @param md_path Character. File path of markdown file.
#'
#' @return Character string of markdown content.
#'
#' @export
#'
#' @examples
#' \dontrun{parse_md(here("confirm-membership.md"))}

parse_md <- function(md_path) {

  if (tools::file_ext(md_path) != "md") {
    cli::cli_abort(c(
      "x" = "{.arg md_path} must be a .md file.",
      "i" = "Supplied {.arg md_path} is a .{tools::file_ext(md_path)} file."
    ))
  }

  if (!file.exists(md_path)) {
    cli::cli_abort(
      "File does not exist at {.file {md_path}}."
    )
  }

  readLines(con = md_path) %>% paste(collapse = "\n")

}
