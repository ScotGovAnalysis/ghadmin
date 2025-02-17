# Name: 03_reminders.R
# Desc: Check status of issues and send reminders
# Date: February 2025

# 0 - Load packages ----

library(ghadmin)
library(dplyr)
library(here)


# 1 - Check status of issues ----

summary <-
  issues("scotgovanalysis", "ahtest", labels = "2025-review") %>%
  mutate(task_status = task_status(body))

incomplete <-
  summary %>%
  filter(task_status %in% c("in progress", "not started"))


# 2 - Post reminder message ----

member_review_reminder(
  "scotgovanalysis",
  "ahtest",
  issue_number = 25,
  body_template = here("reviews", "2025-01", "templates", "reminder.Rmd"),
  deadline = "Friday 28 March"
)


### END OF SCRIPT ###
