# Name: 02_review-members.R
# Desc: Set up review of organisation members
# Date: February 2025

# 0 - Run set up ----

source(here::here("reviews", "2025", "00_setup.R"))


# 1 - Extract data for all organisation members ----

all_members <- users(review_params$org, user_type = "members")


# 2 - Ensure all members in 'org-admin-all-members' team ----

all_org_team <- team_members(review_params$org, "org-admin-all-members")

members_to_add <- setdiff(all_members$user, all_org_team$user)

walk(
  members_to_add,
  \(user) add_team_member(review_params$org, "org-admin-all-members", user)
)

rm(all_org_team, members_to_add)


# 3 - Create member review issues ----

confirm_to_continue(nrow(all_members))

review_issues <-
  map(
    c("alice-hannah"),
    \(user) {
      member_review_issue(
        owner = review_params$org,
        repo = review_params$repo,
        assign_user = user,
        body_template =
          here("reviews", "2025", "templates", "confirm-membership.Rmd"),
        deadline = review_params$deadline,
        label = review_params$label
      )
    }
  ) %>%
  list_rbind() %>%
  mutate(date_opened = Sys.Date())


# 4 - Save summary ----

review_status <-
  all_members %>%
  select(user) %>%
  left_join(review_issues, by = "user")

if (any(is.na(review_status$issue_number))) {
  cli_alert_warning(paste(
    "{sum(is.na(review_status$issue_number))} members do not have an",
    "open member review issue."
  ))
}

write_rds(
  review_status,
  here("reviews", "2025", "data", "2025_member-review.rds")
)


### END OF SCRIPT ###
