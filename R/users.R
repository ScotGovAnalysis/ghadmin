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

  check_length(org, 1)
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
