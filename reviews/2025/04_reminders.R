# Name: 04_reminders.R
# Desc: Send reminder messages
# Date: February 2025

# 0 - Run set up ----

source(here::here("reviews", "2025", "00_setup.R"))


# 1 - Get review issues ----

review_issues <-
  read_rds(here("reviews", "2025", "data", "2025_review-issues.rds"))


# 2 - Post reminder message ----

to_remind <-
  review_issues %>%
  filter(is.na(date_reminded) & is.na(date_confirmed)) %>%
  pull(issue_number)

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


review_issues <- if(nrow(reminded) > 0) {
  review_issues %>%
    left_join(reminded %>% select(issue_number, date_commented),
              by = "issue_number") %>%
    mutate(date_reminded = if_else(
      !is.na(date_commented), date_commented, date_reminded
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
