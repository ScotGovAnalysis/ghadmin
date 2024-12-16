#' Repository issues
#'
#' @inheritParams repo_access
#' @param state Optional. To filter issues returned based on status. Either
#' "open" (default), "closed" or "all".
#' @param labels Optional. Character vector of labels to filter issues by.
#'
#' @return A tibble.
#'
#' @export
#'
#' @examples
#' \dontrun{issues("scotgovanalysis", "ghadmin")}

issues <- function(owner,
                   repo,
                   state = c("open", "closed", "all"),
                   labels = NULL) {

  state <- rlang::arg_match(state)

  labels <- if (!is.null(labels)) paste0(labels, collapse = ",")

  resp <- gh::gh("/repos/{owner}/{repo}/issues",
                 owner = owner,
                 repo = repo,
                 labels = labels,
                 state = state,
                 .limit = Inf)

  if (length(resp) == 0) return(dplyr::tibble())

  dplyr::tibble(owner = owner,
                repo = repo,
                issues = resp) %>%
    tidyr::hoist(.data$issues, "number", "state", "title", "body", "labels") %>%
    tidyr::unnest_longer(.data$labels) %>%
    tidyr::hoist(.data$labels, "name") %>%
    dplyr::group_by(dplyr::across(-c("issues", "labels", "name"))) %>%
    dplyr::summarise(labels = list(.data$name), .groups = "drop")

}
