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
    all_members$user,
    \(user) {
      new_review_issue(
        owner = review_params$org,
        repo = review_params$repo,
        assign_user = user,
        body_template =
          here("reviews", "2025", "templates", "request-review.Rmd"),
        label = review_params$label,
        date = review_params$deadline
      )
    }
  ) %>%
  list_rbind()


# 4 - Check issues opened for all members ----

missed <- setdiff(all_members$user, review_issues$user)

if (length(missed) > 0) {
  cli_abort(c(
    "{length(missed)} members do not have an open member review issue.",
    "i" = "Run {.code print(missed)} to see which members have been missed."
  ))
}


# 5 - Save summary ----

review_issues %>%
  mutate(reminder_message = lubridate::NA_Date_,
         confirm_message = lubridate::NA_Date_,
         complete = NA) %>%
  write_rds(here("reviews", "2025", "data", "2025_review-issues.rds"))


### END OF SCRIPT ###
