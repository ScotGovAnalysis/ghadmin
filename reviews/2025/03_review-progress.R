# Name: 03_review-progress.R
# Desc: Check status and close completed issues
# Date: February 2025

# 0 - Run set up ----

source(here::here("reviews", "2025", "00_setup.R"))


# 1 - Check status of issues ----

review_issues <-
  issues(review_params$org, review_params$repo,
         state = "all",
         labels = review_params$label)


# Check issue still exists for all members

opened_issues <-
  read_rds(here("reviews", "2025", "data", "2025_member-review.rds")) %>%
  filter(!is.na(issue_number)) %>%
  select(user, issue_number) %>%
  left_join(
    review_issues %>% select(issue_number, state),
    by = "issue_number"
  )


    by = c("number" = "issue_number")
  ) %>%

  # Check issue status valid
  mutate(
    task_status = task_status(body),
    n_tasks = n_tasks(body),
    check_tasks = case_when(
      n_tasks == 3 ~ "ok",
      .default = "investigate"
    ),
    check_state = case_when(
      state == "open" ~ "ok",
      state == "closed" & task_status == "complete" ~ "ok",
      .default = "investigate"
    ),
    check_assignee = case_when(
      assignees == user ~ "ok",
      .default = "investigate"
    )
  )

investigate <-
  summary %>%
  filter(if_any(starts_with("check_"), ~ . == "investigate"))

if (nrow(investigate) > 0) {
  cli_abort(
    "{nrow(investigate)} issues require investigation before proceeding."
  )
}


# 3 - Save summary ----




### END OF SCRIPT ###
