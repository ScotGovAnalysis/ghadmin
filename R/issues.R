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
      .data$issues, "number", "state", "title", "body", "labels", "assignees"
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


#' Create a new issue for membership review
#'
#' @inheritParams repo_access
#' @param assign_user A character string of the username to assign the issue to.
#' @param body_template A character string of the file path to Rmd template.
#' @param deadline A character string to include as deadline to respond; e.g.
#'  "Friday 28 February".
#' @param label A character vector of labels to add to the issue.
#' @param issue_number Number of issue to add a comment to.
#'
#' @return If the API call is successful, a tibble containing `owner`, `repo`,
#'  `user`, `issue_number` and for `member_review_reminder`, `reminder_issued`.
#'
#' @export

member_review_issue <- function(owner,
                                repo,
                                assign_user,
                                body_template,
                                deadline,
                                label) {

  check_arg(owner)
  check_arg(repo)
  check_arg(assign_user)
  check_arg(body_template)
  check_arg(deadline)

  response1 <-
    gh::gh(
      "/repos/{owner}/{repo}/issues",
      owner = owner,
      repo = repo,
      title = paste("Review membership -", assign_user),
      labels = list(label),
      .method = "POST"
    )

  url <- glue::glue(
    "https://github.com/login?",
    "return_to=https://github.com/{owner}/{repo}/issues/{response1$number}"
  )

  response2 <-
    gh::gh(
      "/repos/{owner}/{repo}/issues/{issue_number}",
      owner = owner,
      repo = repo,
      issue_number = response1$number,
      body = knit_rmd(
        body_template,
        params = list(date = deadline,
                      issue_url = url)
      ),
      assignees = list(assign_user),
      .method = "PATCH"
    )

  dplyr::tibble(owner = owner,
                repo = repo,
                user = assign_user,
                issue_number = response2$number)

}

#' @export
#' @rdname member_review_issue

member_review_reminder <- function(owner,
                                   repo,
                                   issue_number,
                                   body_template,
                                   deadline) {

  response <-
    gh::gh(
      "/repos/{owner}/{repo}/issues/{issue_number}/comments",
      owner = owner,
      repo = repo,
      issue_number = issue_number,
      body = knit_rmd(
        body_template,
        params = list(date = deadline)
      ),
      .method = "POST"
    )

  dplyr::tibble(owner = owner,
                repo = repo,
                user = response$user$login,
                issue_number = issue_number,
                reminder_issued = TRUE)

}

