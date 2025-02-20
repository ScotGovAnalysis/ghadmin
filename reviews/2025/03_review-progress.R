# Name: 03_review-progress.R
# Desc: Check status and close completed issues
# Date: February 2025

# 0 - Run set up ----

source(here::here("reviews", "2025", "00_setup.R"))


# 1 - Check status of issues ----

github_issues <- issues(review_params$org, review_params$repo, state = "all")

review_issues <-
  read_rds(here("reviews", "2025", "data", "2025_review-issues.rds"))

validate <-
  review_issues %>%
  left_join(
    github_issues %>% select(issue_number, state, body, labels, assignees),
    by = "issue_number"
  ) %>%
  validate_review_issues(exp_tasks = 3, exp_label = "2025-review")


# 2 - Close completed issues ----

completed <-
  github_issues %>%
  filter(issue_number %in% review_issues$issue_number &
           task_status(body) == "complete")

closed <-
  map(
    completed %>% filter(state == "open") %>% pull(issue_number),
    \(issue_number) {
      close_issue(review_params$org, review_params$repo, issue_number)
    }
  ) %>%
  list_rbind()


# 3 - Save summary ----

review_issues <-
  review_issues %>%
  mutate(
    complete = if_else(issue_number %in% completed$issue_number, TRUE, complete)
  )

write_rds(
  review_issues,
  here("reviews", "2025", "data", "2025_review-issues.rds")
)


### END OF SCRIPT ###
