# Name: 04_reminders.R
# Desc: Send reminder messages
# Date: February 2025

# 0 - Run set up ----

source(here::here("reviews", "2025", "00_setup.R"))


# 1 - Get open issues ----

to_remind <-
  issues(review_params$org, review_params$repo,
         state = "open",
         labels = review_params$label)


# 2 - Post reminder message ----

reminders <-
  map(
    to_remind$issue_number,
    \(issue_number) {
      review_reminder(
        owner = review_params$org,
        repo = review_params$repo,
        issue_number = issue_number,
        body_template = here("reviews", "2025", "templates", "reminder.Rmd"),
        deadline = review_params$deadline
      )
    }
  ) %>%
  list_rbind()


# 3 - Save summary ----




### END OF SCRIPT ###
