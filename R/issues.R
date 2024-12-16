#' Repository issues
#'
#' @inheritParams repo_access
#' @param state Optional. To filter issues returned based on status. Either
#' "open" (default), "closed" or "all".
#' @param labels Optional. Character vector of labels to filter issues by.
#' @param issue_number Numeric. Issue number to return.
#' @param tasks Logical. If `TRUE`, the returned tibble will contain a row for
#' each task in each issue. If `FALSE`, one row per issue will be returned with
#' no task detail.
#'
#' @return A tibble.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' issues("scotgovanalysis", "ghadmin")
#' issue_number("scotgovanalysis", "ghadmin", 1)
#' }

issues <- function(owner,
                   repo,
                   state = c("open", "closed", "all"),
                   labels = NULL,
                   tasks = FALSE) {

  state <- rlang::arg_match(state)

  labels <- if (!is.null(labels)) paste0(labels, collapse = ",")

  dplyr::tibble(
    owner = owner,
    repo = repo,
    issues = gh::gh(
      "/repos/{owner}/{repo}/issues",
      owner = owner,
      repo = repo,
      labels = labels,
      state = state,
      .limit = Inf
    )
  ) %>%
    issues_util(tasks)

}

#' @export
#' @rdname issues

issue_number <- function(owner, repo, issue_number, tasks = FALSE) {

  dplyr::tibble(
    owner = owner,
    repo = repo,
    issues = list(gh::gh(
      "/repos/{owner}/{repo}/issues/{issue_number}",
      owner = owner,
      repo = repo,
      issue_number = issue_number
    ))
  ) %>%
  issues_util(tasks)

}

issues_util <- function(tib_resp, tasks = FALSE) {

  if (nrow(tib_resp) == 0) return(dplyr::tibble())

  tib <-
    tib_resp %>%
    tidyr::hoist(.data$issues, "number", "state", "title", "body", "labels") %>%
    tidyr::unnest_longer(.data$labels) %>%
    tidyr::hoist(.data$labels, "name") %>%
    dplyr::group_by(dplyr::across(-c("issues", "labels", "name"))) %>%
    dplyr::summarise(labels = list(.data$name), .groups = "drop")

  if (tasks) expand_tasks(tib) else tib

}

issue_tasks <- function(body) {

  dplyr::tibble(
    task = stringr::str_extract_all(body, "\\[(x|\\s)\\].+")
  ) %>%
    tidyr::unnest_longer(.data$task) %>%
    tidyr::separate_wider_regex(
      .data$task,
      c(status = "^\\[[x|\\s]\\]", "\\s", task_name = ".*$")
    ) %>%
    dplyr::mutate(status = dplyr::case_match(.data$status,
                                             "[ ]" ~ "incomplete",
                                             "[x]" ~ "complete",
                                             .default = NA))

}

expand_tasks <- function(issue_tibble) {

  issue_tibble %>%
    dplyr::rowwise() %>%
    dplyr::mutate(tasks = list(issue_tasks(.data$body))) %>%
    dplyr::ungroup() %>%
    tidyr::unnest_longer("tasks")

}
