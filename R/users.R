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
#' users("ScotGovAnalysis")

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
