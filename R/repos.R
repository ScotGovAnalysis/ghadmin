#' Organisation repositories
#'
#' @inheritParams users
#'
#' @return A tibble.
#'
#' @export
#'
#' @examples
#' \dontrun{repos("scotgovanalysis")}

repos <- function(org) {

  check_length(org, 1)

  dplyr::tibble(repos = gh::gh("/orgs/{org}/repos",
                               org = org,
                               .limit = Inf)) %>%
    tidyr::hoist(.data$repos,
                 repo = "name",
                 owner = c("owner", "login"),
                 "visibility",
                 "created_at",
                 "updated_at",
                 "pushed_at"
    ) %>%
    dplyr::mutate(dplyr::across(
      dplyr::matches("*._at$"),
      \(x) lubridate::round_date(lubridate::ymd_hms(x), "day")
    )) %>%
    dplyr::select(-"repos")

}


#' Repository user access and contributions
#'
#' @param owner String. Repository owner (e.g. organisation or user profile).
#' @param repo Character vector of repository names.
#'
#' @return A tibble. For `repo_access` a row for all users with access to the
#' repository. For `repo_contrib` a row for all users who have contributed to
#' the repository.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'  repo_access("scotgovanalysis", "welcome")
#'  repo_access("scotgovanalysis", c("welcome", "sgplot"))
#'
#'  repo_contrib("scotgovanalysis", "welcome")
#'  repo_contrib("scotgovanalysis", c("welcome", "sgplot"))
#' }

repo_access <- function(owner, repo) {

  check_length(owner, 1)

  purrr::map(
    repo,
    \(repo) {
      dplyr::tibble(repo = repo,
                    access = gh::gh("GET /repos/{owner}/{repo}/collaborators",
                                    owner = owner,
                                    repo = repo,
                                    .progress = FALSE,
                                    .limit = Inf)) %>%
        tidyr::hoist(.data$access,
                     user = "login",
                     role = "role_name") %>%
        dplyr::select(-"access")
    },
    .progress = list(name = "Getting repository collaborators...",
                     type = "tasks")
  ) %>%
    purrr::list_rbind()

}


#' @rdname repo_access
#' @export

repo_contrib <- function(owner, repo) {

  check_length(owner, 1)

  purrr::map(
    repo,
    \(repo) {
      dplyr::tibble(repo = repo,
                    contrib = gh::gh("GET /repos/{owner}/{repo}/contributors",
                                     owner = owner,
                                     repo = repo,
                                     anon = "true",
                                     .progress = FALSE,
                                     .limit = Inf)) %>%
        possibly_hoist(
          .data$contrib,
          user = "login",
          "contributions",
          otherwise = dplyr::tibble(name = repo,
                                    user = NA_character_,
                                    contributions = NA_real_)
        ) %>%
        dplyr::select(-dplyr::any_of("contrib"))
    },
    .progress = list(name = "Getting repository contributors...",
                     type = "tasks")
  ) %>%
    purrr::list_rbind()

}
