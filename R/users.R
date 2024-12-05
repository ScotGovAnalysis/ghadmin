#' Organisation users
#'
#' @param org String. Name of GitHub organisation
#' @param user_type One of "all", "member", or "outside" (for outside
#' collaborators). Defauls to "all".
#'
#' @return A tibble.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' users("ScotGovAnalysis")
#' }

users <- function(org,
                  user_type = c("all", "members", "outside_collaborators")) {

  user_type <- rlang::arg_match(user_type)

  if (user_type == "all") {
    user_type <- c("members", "outside_collaborators")
  }

  purrr::map(user_type, \(x) get_users(org, x)) %>%
    purrr::list_rbind() %>%
    tidyr::hoist(.data$member, "site_admin") %>%
    dplyr::rowwise() %>%
    dplyr::mutate(public_info = list(gh::gh("GET /users/{username}",
                                            username = .data$username,
                                            .limit = Inf))) %>%
    dplyr::ungroup() %>%
    tidyr::hoist(.data$public_info,
      "name",
      "company",
      "email",
      .transform = list(company = trimws)
    ) %>%
    dplyr::select(-c("member", "public_info")) %>%
    dplyr::relocate(.data$site_admin, .after = dplyr::everything())
}

get_users <- function(org, user_type = c("members", "outside_collaborators")) {

  user_type <- rlang::arg_match(user_type)

  dplyr::tibble(
    type = user_type,
    member = gh::gh("GET /orgs/{org}/{user_type}",
      org = org,
      user_type = user_type,
      .limit = Inf
    )
  ) %>%
    tidyr::hoist(.data$member, username = "login")

}

#' Organisation users to follow up
#'
#' @description Find organisation users who either do not have a public email
#' address or are outside collaborations.
#'
#' @inheritParams users
#' @param return Either "detail" to return a row for each user, or "summary" to
#' return a summary of number of users per type of issue.
#'
#' @return A tibble.
#' @export
#'
#' @examples
#' \dontrun{
#' users_follow_up("ScotGovAnalysis")
#' }

users_follow_up <- function(
    org,
    user_type = c("all", "members", "outside_collaborators"),
    return = c("detail", "summary")
) {

  return <- rlang::arg_match(return)

  follow_up <-
    users(org, user_type) %>%
    dplyr::mutate(issue = dplyr::case_when(
        .data$type == "outside_collaborators" ~ "outside_collaborator",
        is.na(.data$email) ~ "email_missing",
        .default = "no_issue"
    ))

  if (return == "detail") {
    return(dplyr::filter(follow_up, .data$issue != "no_issue"))
  }

  if (return == "summary") {
    return(
      follow_up %>%
        dplyr::group_by(.data$issue) %>%
        dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
        dplyr::mutate(sort = dplyr::case_match(
          .data$issue,
          "no_issue" ~ 2,
          .default = 1
        )) %>%
        dplyr::arrange(.data$sort) %>%
        dplyr::select(-"sort")
    )
  }

}
