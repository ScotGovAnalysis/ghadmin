# Name: 04_reminders.R
# Desc: Send reminders
# Date: February 2025

# 0 - Run set up ----

source(here::here("reviews", "2025", "00_setup.R"))


# 1 - Check status of issues ----

review_issues <-
  issues(review_params$org, review_params$repo,
         state = "all",
         labels = review_params$label)


# 2 - Post reminder message ----

incomplete <-
  summary %>%
  filter(task_status %in% c("in progress", "not started"))

reminders <-
  map(
    incomplete$number,
    \(issue_number) {
      member_review_reminder(
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
