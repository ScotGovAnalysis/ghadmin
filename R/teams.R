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

  check_length(org, 1)

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


#' Add member to an organisation team
#'
#' @inheritParams team_repos
#' @param user String. GitHub username of member.
#'
#' @return Logical (invisibly).
#'
#' @export
#'
#' @examples
#' \dontrun{add_team_member("scotgovanalysis", "test-team", "alice-hannah")}

add_team_member <- function(org, team, user) {

  if (!check_member(org, user)) {
    cli::cli_abort(
      "{user} is not a member of the {org} organisation."
    )
  }

  resp <-
    gh::gh("/orgs/{org}/teams/{team_slug}/memberships/{username}",
           org = org,
           team_slug = team,
           username = user,
           .method = "PUT")

  if (resp$role == "member") {
    cli::cli_inform(c(
      "i" = "{user} has been added to the {team} team."
    ))
    return(invisible(TRUE))
  }

}
