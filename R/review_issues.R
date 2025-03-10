#' Create a new issue for membership review
#'
#' @inheritParams repo_access
#' @param assign_user A character string of the username to assign the issue to.
#' @param body_template A character string of the file path to Rmd template.
#' @param ... Named values to pass to params of `body_template` Rmd.
#' @param label A character vector of labels to add to the issue.
#' @param issue_number Number of issue to add a reminder to.
#'
#' @return If the API call is successful, a tibble containing `owner`, `repo`,
#'  `user`, `issue_number`, `date_opened` (for `new_review_issue`) and
#'  `date_commented` (for `review_comment`).
#'
#' @export

new_review_issue <- function(owner,
                             repo,
                             assign_user,
                             body_template,
                             label,
                             ...) {

  check_arg(owner)
  check_arg(repo)
  check_arg(assign_user)
  check_arg(body_template)

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
        params = list(..., issue_url = url)
      ),
      assignees = list(assign_user),
      .method = "PATCH"
    )

  dplyr::tibble(owner = owner,
                repo = repo,
                user = assign_user,
                issue_number = response2$number,
                date_opened = Sys.Date())

}

#' @export
#' @rdname new_review_issue

review_comment <- function(owner,
                           repo,
                           issue_number,
                           body_template,
                           ...) {

  response <-
    gh::gh(
      "/repos/{owner}/{repo}/issues/{issue_number}/comments",
      owner = owner,
      repo = repo,
      issue_number = issue_number,
      body = knit_rmd(
        body_template,
        params = list(...)
      ),
      .method = "POST"
    )

  dplyr::tibble(owner = owner,
                repo = repo,
                user = response$user$login,
                issue_number = issue_number,
                date_commented = Sys.Date())

}


#' Validate review issues
#'
#' @param tib A tibble containing at least the following variables:
#'  `owner`, `repo`, `user`, `issue_number`, `date_opened`, `state`, `body`,
#'  `labels`, `assignees`. This tibble should be created by joining the record
#'  of review issues opened, and the latest data for these issues from GitHub.
#' @param exp_tasks Integer of number of tasks expected to be within the issue
#'  body.
#' @param exp_label Character string of expected label assigned to GitHub issue.
#'
#' @return The rows of `tib` containing errors are returned as a tibble. If
#'  there are no errors, NULL is returned invisibly.
#'
#' @export

validate_review_issues <- function(tib, exp_tasks, exp_label) {

  if (!tibble::is_tibble(tib)) {
    cli::cli_abort("{.arg tib} must be a tibble.")
  }

  exp_names <- c("owner", "repo", "user", "issue_number", "date_opened",
                 "state", "body", "labels", "assignees")

  if (!all(exp_names %in% names(tib))) {
    missing <- setdiff(exp_names, names(tib))
    cli::cli_abort(c(
      "{.arg tib} does not contain all expected variables.",
      "i" = "The following variables are missing: {.var {missing}}."
    ))
  }

  check_arg(exp_tasks, class = "numeric")
  check_arg(exp_label)

  check_tib <- tib %>%
    dplyr::mutate(
      issue_exists = !is.na(.data$state),
      assignees_intact = .data$user == .data$assignees,
      tasks_intact = n_tasks(.data$body) == exp_tasks,
      label_intact = .data$labels == exp_label,
      state_valid = dplyr::case_when(
        state == "open" ~ TRUE,
        state == "closed" & task_status(.data$body) == "complete" ~ TRUE,
        .default = FALSE
      ),
      any_errors =
        !.data$issue_exists |
        !.data$assignees_intact |
        !.data$tasks_intact |
        !.data$label_intact |
        !.data$state_valid
    )

  n_errors <- sum(check_tib$any_errors)

  if (n_errors > 0) {
    cli::cli_inform(c(
      "!" = "Errors identified in {n_errors} review issue{?s}.",
      "i" = paste("Inspect and resolve issues manually on GitHub before",
                  "continuing.")
    ))
    return(check_tib %>% dplyr::filter(.data$any_errors))
  } else {
    cli::cli_alert_success("No errors identified.")
    return(invisible(NULL))
  }

}
