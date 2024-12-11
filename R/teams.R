#' Organisation teams
#'
#' @param org String. Name of GitHub organisation
#'
#' @return A tibble.
#'
#' @export
#'
#' @examples
#' \dontrun{teams("ScotGovAnalysis")}

teams <- function(org) {

  dplyr::tibble(teams = gh::gh("/orgs/{org}/teams",
                               org = org,
                               .limit = Inf)) %>%
    tidyr::hoist(.data$teams,
                 team = "slug",
                 "description",
                 "privacy",
                 parent = c("parent", "slug")) %>%
    dplyr::select(-"teams")

}


#' Team repositories
#'
#' @inheritParams teams
#' @param team String. Name of team
#'
#' @return A tibble.
#'
#' @export
#'
#' @examples
#' \dontrun{team_repos("ScotGovAnalysis", "team_name")}

team_repos <- function(org, team) {

  check_length(org, 1)
  check_length(team, 1)

  repos <-
    dplyr::tibble(
      team = team,
      repos = gh::gh("/orgs/{org}/teams/{team}/repos",
                     org = org,
                     team = team,
                     .limit = Inf)
    )

  if (nrow(repos) > 0) {
    tidyr::hoist(repos,
                 .data$repos,
                 repo = "name",
                 repo_visibility = "visibility") %>%
      dplyr::select(-"repos")
  } else {
    cli::cli_warn("The {.str {team}} team has no repositories.")
    return(NULL)
  }

}


#' Team members
#'
#' @inheritParams team_repos
#'
#' @return A tibble.
#'
#' @export
#'
#' @examples
#' \dontrun{team_members("ScotGovAnalysis", "team_name")}

team_members <- function(org, team) {

  check_length(org, 1)
  check_length(team, 1)

  members<-
    dplyr::tibble(
      team = team,
      members = gh::gh("/orgs/{org}/teams/{team}/members",
                       org = org,
                       team = team,
                       .limit = Inf)
    )

  if (nrow(members) > 0) {
    tidyr::hoist(members, .data$members, user = "login") %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      resp = list(gh::gh("/orgs/{org}/teams/{team}/memberships/{username}",
                         org = org,
                         team = team,
                         username = .data$user))
    ) %>%
    dplyr::ungroup() %>%
    tidyr::hoist(.data$resp, "state", "role") %>%
    dplyr::select(-c("members", "resp"))
  } else {
    cli::cli_warn("The {.str {team}} team has no members.")
    return(NULL)
  }

}
