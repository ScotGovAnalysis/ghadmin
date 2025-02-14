#' Summary of task completion
#'
#' @details Within `body`, tasks are identified by square brackets
#' containing either an 'x' (complete) or a space (incomplete).
#'
#' @param body Character string, length 1.
#'
#' @return Integer.
#'
#' @export
#'
#' @examples
#' n_complete("Some info: [x] task 1 [ ] task 2")

n_tasks <- function(body) {
  stringr::str_extract_all(body, "\\[(x|\\s)\\]") %>%
    purrr::map(\(x) {length(x)}) %>%
    purrr::list_c()
}

#' @export
#' @rdname n_tasks

n_complete <- function(body) {
  stringr::str_extract_all(body, "\\[(x|\\s)\\]") %>%
    purrr::map(\(x) {sum(x == "[x]")}) %>%
    purrr::list_c()
}

#' @export
#' @rdname n_tasks

n_incomplete <- function(body) {
  stringr::str_extract_all(body, "\\[(x|\\s)\\]") %>%
    purrr::map(\(x) {sum(x == "[ ]")}) %>%
    purrr::list_c()
}


#' Summary of task completion
#'
#' @details Within `body`, tasks are identified by square brackets
#' containing either an 'x' (complete) or a space (incomplete).
#'
#' @param body Character string, length 1.
#'
#' @return Integer.
#'
#' @export
#'
#' @examples
#' task_status("Some info: [x] task 1 [ ] task 2")

task_status <- function(body) {
  dplyr::case_when(
    n_tasks(body) == 0 ~ "no tasks",
    n_complete(body) == 0 ~ "not started",
    n_complete(body) == n_tasks(body) ~ "complete",
    n_complete(body) > 0 ~ "in progress"
  )
}


#' Expand tasks in issue body
#'
#' @param issue_tibble Tibble containing a variable called `body`.
#'
#' @return The supplied tibble is returned with two additional character
#' variables; `status` ('complete' or 'incomplete') and `task_name`.
#'
#' @export
#'
#' @examples
#' issue_tibble <- dplyr::tibble(
#'    issue_number = c(1, 2),
#'    body = c("Tasks:\n- [x] task 1\n- [ ] task 2",
#'             "Tasks:\n- [x] task 1\n- [x] task 2")
#' )
#' expand_tasks(issue_tibble)

expand_tasks <- function(issue_tibble) {

  issue_tibble %>%
    dplyr::mutate(
      task = stringr::str_extract_all(.data$body, "[\n\r]- \\[(x|\\s)\\].+")
    ) %>%
    tidyr::unnest_longer("task", keep_empty = TRUE) %>%
    dplyr::mutate(task = stringr::str_remove(.data$task, "^[\n\r]-\\s")) %>%
    tidyr::separate_wider_regex(
      .data$task,
      c(status = "^\\[[x|\\s]\\]", "\\s", task_name = ".*$")
    ) %>%
    dplyr::mutate(status = dplyr::case_match(.data$status,
                                             "[ ]" ~ "incomplete",
                                             "[x]" ~ "complete",
                                             .default = NA))

}
