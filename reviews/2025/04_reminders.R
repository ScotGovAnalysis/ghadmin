# Name: 04_reminders.R
# Desc: Send reminder messages
# Date: February 2025

# 0 - Run set up ----

source(here::here("reviews", "2025", "00_setup.R"))


# 1 - Get review issues ----

github_issues <- issues(
  review_params$org,
  review_params$repo,
  state = "open",
  labels = review_params$label
)


# 2 - Post reminder message ----

to_remind <- github_issues %>% pull(issue_number)

reminded <-
  map(
    to_remind,
    \(issue_number) {
      review_comment(
        owner = review_params$org,
        repo = review_params$repo,
        issue_number = issue_number,
        body_template = here("reviews", "2025", "templates", "reminder.Rmd"),
        date = review_params$deadline
      )
    }
  ) %>%
  list_rbind()

review_issues <-
  read_rds(here("reviews", "2025", "data", "2025_review-issues.rds"))

review_issues <- if(nrow(reminded) > 0) {
  review_issues %>%
    left_join(reminded %>% select(issue_number, date_commented),
              by = "issue_number") %>%
    mutate(reminder_message = if_else(
      !is.na(date_commented), date_commented, reminder_message
    )) %>%
    select(-date_commented)
} else {
  review_issues
}


# 3 - Save summary ----

write_rds(
  review_issues,
  here("reviews", "2025", "data", "2025_review-issues.rds")
)


### END OF SCRIPT ###
