# Name: 05_remove-members.R
# Desc: Remove members and close issues
# Date: May 2025

# 0 - Run set up ----

source(here::here("reviews", "2025", "00_setup.R"))


# 1 - Get list of members to remove ----

review_issues <-
  read_rds(here("reviews", "2025", "data", "2025_review-issues.rds"))

to_remove <-
  review_issues %>%
  filter(is.na(date_confirmed) & is.na(date_removed))


# 2 - Remove members from organisation ----

removed <-
  map(
    to_remove$user,
    \(user) remove_member(review_params$org, user)
  ) %>%
  list_rbind()


# 3 - Comment and close issues ----

closed <-
  map(
    as.numeric(to_remove$issue_number),
    \(issue_number) {
      review_comment(
        review_params$org,
        review_params$repo,
        issue_number,
        body_template = here(
          "reviews", "2025", "templates", "confirm-removal.Rmd"
        )
      )
      close_issue(review_params$org, review_params$repo, issue_number)
    }
  ) %>%
  list_rbind()


# 4 - Save summary ----

review_issues <- if(nrow(removed) > 0 & nrow(closed) > 0) {
  review_issues %>%
    left_join(closed %>% select(issue_number, date_closed),
              by = "issue_number") %>%
    mutate(date_removed = if_else(
      !is.na(date_closed), date_closed, date_removed
    )) %>%
    select(-date_closed)
} else {
  review_issues
}

review_issues <-
  review_issues %>%
  mutate(result = case_when(
    !is.na(date_confirmed) ~ "responded",
    !is.na(date_removed) ~ "removed"
  ))

write_rds(
  review_issues,
  here("reviews", "2025", "data", "2025_review-issues.rds")
)

write_xlsx(
  review_issues,
  here("reviews", "2025", "data", "2025_github-member-review.xlsx")
)

### END OF SCRIPT ###
