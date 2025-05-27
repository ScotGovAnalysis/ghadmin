#' Organisation users
#'
#' @param org String. Name of GitHub organisation
#' @param user_type One of "all", "member", or "outside" (for outside
#' collaborators). Defaults to "all".
#'
#' @return A tibble.
#'
#' @export
#'
#' @examples
#' \dontrun{users("ScotGovAnalysis")}

users <- function(org,
                  user_type = c("all", "members", "outside_collaborators")) {

  check_arg(org, 1)
  user_type <- rlang::arg_match(user_type)

  if (user_type == "all") {
    user_type <- c("members", "outside_collaborators")
  }

  purrr::map(
    user_type,
    \(user_type) {
      dplyr::tibble(
        user_type = user_type,
        member = gh::gh("/orgs/{org}/{user_type}",
                        org = org,
                        user_type = user_type,
                        .limit = Inf
        )
      ) %>%
        tidyr::hoist(.data$member, user = "login")
    }
  ) %>%
    purrr::list_rbind() %>%
    dplyr::rowwise() %>%
    dplyr::mutate(public_info = list(gh::gh("/users/{user}",
                                            user = .data$user,
                                            .limit = Inf))) %>%
    dplyr::ungroup() %>%
    tidyr::hoist(.data$public_info,
      "name",
      "company",
      "email",
      .transform = list(company = trimws)
    ) %>%
    dplyr::select(-c("member", "public_info"))
}


#' Check if GitHub user is an organisation member
#'
#' @inheritParams users
#' @param user String. GitHub user name.
#'
#' @return Logical.
#'
#' @export
#'
#' @examples
#' \dontrun{check_member("scotgovanalysis", "alice-hannah")}

check_member <- function(org, user) {

  gh_safe <- purrr::safely(gh::gh)

  resp <- gh_safe("/orgs/{org}/members/{username}",
                  org = org,
                  username = user)

  status <- httr2::last_response() %>% httr2::resp_status()

  if (status %in% c(302, 404)) return(FALSE)
  if (status == 204) return(TRUE)

}


#' Remove member from organisation
#'
#' @inheritParams check_member
#'
#' @return A tibble.
#'
#' @export
#'
#' @examples
#' \dontrun{remove_member("scotgovanalysis", "alice-hannah")}

remove_member <- function(org, user) {

  if (!check_member(org, user)) {
    cli::cli_abort(
      "{user} is not a member of {org}."
    )
  }

  resp <- gh::gh("/orgs/{org}/memberships/{user}",
                 org = org,
                 user = user,
                 .method = "DELETE")

  tibble::tibble(
    owner = org,
    user = user,
    date_removed = Sys.Date()
  )

}
