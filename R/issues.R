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
                   labels = NULL) {

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
    issues_util()

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

issues_util <- function(response, tasks = FALSE) {

  if (nrow(response) == 0) return(dplyr::tibble())

  tib <-
    response %>%
    tidyr::hoist(
      .data$issues,
      "issue_number" = "number", "state", "title", "body", "labels", "assignees"
    ) %>%
    tidyr::unnest_longer("labels", keep_empty = TRUE) %>%
    tidyr::unnest_longer("assignees", keep_empty = TRUE)

  if (typeof(tib$labels) == "list") {
    tib <-
      tib %>%
      tidyr::hoist(.data$labels, "name") %>%
      dplyr::group_by(dplyr::across(
        -dplyr::any_of(c("issues", "labels", "name"))
      )) %>%
      dplyr::summarise(labels = list(.data$name), .groups = "drop")
  }

  if (typeof(tib$assignees) == "list") {
    tib <-
      tib %>%
      tidyr::hoist(.data$assignees, "login") %>%
      dplyr::group_by(dplyr::across(
        -dplyr::any_of(c("assignees", "login"))
      )) %>%
      dplyr::summarise(assignees = list(.data$login), .groups = "drop")
  }

  if (tasks) expand_tasks(tib) else tib

}


#' Close issue
#'
#' @inheritParams repo_access
#' @param issue_number Number of issue to close
#'
#' @return A tibble.
#' @export
#'
#' @examples
#' \dontrun{close_issue("scotgovanalysis", "ghadmin", 1)}

close_issue <- function(owner, repo, issue_number) {

  check_arg(owner)
  check_arg(repo)
  check_arg(issue_number, class = "numeric")

  gh::gh(
    "/repos/{owner}/{repo}/issues/{issue_number}",
    owner = owner,
    repo = repo,
    issue_number = issue_number,
    state = "closed",
    .method = "PATCH"
  )

  dplyr::tibble(owner = owner,
                repo = repo,
                issue_number = issue_number,
                date_closed = Sys.Date())

}
