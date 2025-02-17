# Name: 02_review-members.R
# Desc: Set up review of organisation members
# Date: February 2025

# 0 - Load packages ----

library(ghadmin)
library(dplyr)
library(stringr)
library(purrr)
library(here)
library(cli)


# 1 - Extract data for all organisation members ----

all_members <- users("scotgovanalysis", user_type = "members")


# 2 - Ensure all members in 'org-admin-all-members' team ----

all_org_team <- team_members("ScotGovAnalysis", "org-admin-all-members")

members_to_add <- setdiff(all_members$user, all_org_team$user)

walk(
  members_to_add,
  \(user) add_team_member("scotgovanalysis", "org-admin-all-members", user)
)

rm(all_org_team, members_to_add)


# 3 - Create member review issues ----

review_issues <-
  map(
    c("alice-hannah"),
    \(user) {
      member_review_issue(
        owner = "scotgovanalysis",
        repo = "ahtest",
        assign_user = user,
        body_template =
          here("reviews", "2025", "templates", "confirm-membership.Rmd"),
        deadline = "Friday 28 March",
        label = "2025-review"
      )
    }
  ) %>%
  list_rbind()


# 4 - Save summary ----

review_status <- all_members %>% left_join(review_issues, by = "user")

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
