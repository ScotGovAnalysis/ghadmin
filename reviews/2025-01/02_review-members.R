# Name: 02_review-members.R
# Desc: Set up review of organisation members
# Date: February 2025

# 0 - Load packages ----

library(ghadmin)
library(dplyr)
library(stringr)
library(purrr)


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


### END OF SCRIPT ###
