#' Create a new issue for membership review
#'
#' @inheritParams repo_access
#' @param assign_user A character string of the username to assign the issue to.
#' @param body_template A character string of the file path to Rmd template.
#' @param deadline A character string to include as deadline to respond; e.g.
#'  "Friday 28 February".
#' @param label A character vector of labels to add to the issue.
#' @param issue_number Number of issue to add a reminder to.
#'
#' @return If the API call is successful, a tibble containing `owner`, `repo`,
#'  `user`, `issue_number`, `date_opened` (for `new_review_issue`) and
#'  `date_reminded` (for `review_reminder`).
#'
#' @export

new_review_issue <- function(owner,
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
                issue_number = response2$number,
                date_opened = Sys.Date())

}

#' @export
#' @rdname new_review_issue

review_reminder <- function(owner,
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
                date_reminded = Sys.Date())

}
