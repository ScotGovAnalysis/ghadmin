# Name: 02_review-members.R
# Desc: Set up review of organisation members
# Date: February 2025

# 0 - Load packages ----

library(ghadmin)
library(dplyr)
library(stringr)
library(purrr)
library(here)


# 1 - Extract data for all organisation members ----

all_members <-
  users("scotgovanalysis", user_type = "members") %>%
  mutate(email_issue = case_when(
    is.na(email) ~ TRUE,
    !str_detect(email, "gov\\.scot$") ~ TRUE,
    .default = FALSE
  ))


# 2 - Ensure all members in 'org-admin-all-members' team ----

all_org_team <- team_members("ScotGovAnalysis", "org-admin-all-members")

members_to_add <- setdiff(all_members$user, all_org_team$user)

walk(
  members_to_add,
  \(user) add_team_member("scotgovanalysis", "org-admin-all-members", user)
)

rm(all_org_team, members_to_add)


# 3 - Create member review issues ----

create_review_issue(
  owner = "scotgovanalysis",
  repo = "ahtest",
  assign_user = "alice-hannah",
  body = knit_rmd(
    here("reviews", "2025-01", "templates", "confirm-membership.Rmd"),
    params = list(date = "Friday 28 March")
  ),
  label = "2025-review"
)

# walk(
#   all_members$user,
#   \(user) {
#     create_review_issue(
#       owner = "scotgovanalysis",
#       repo = "ahtest",
#       assign_user = user,
#       body = parse_md(
#         here("reviews", "2025-01", "templates", "confirm-membership.md")
#       ),
#       label = "2025-review"
#     )
#   }
# )


### END OF SCRIPT ###
