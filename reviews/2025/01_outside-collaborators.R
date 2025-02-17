# Name: 01_outside-collaborators.R
# Desc: Analysis of outside collaborators
# Date: December 2024

# 0 - Load packages ----

library(ghadmin)
library(dplyr)
library(here)
library(readr)
library(writexl)


# 1 - Repository info ----

repos <- repos("scotgovanalysis")

repo_use <-
  full_join(
    repo_access("scotgovanalysis", repos$repo),
    repo_contrib("scotgovanalysis", repos$repo),
    by = c("repo", "user")
  ) %>%
  mutate(contributions = case_when(
    !is.na(role) & is.na(contributions) ~ 0,
    TRUE ~ contributions
  )) %>%
  arrange(repo)


# 2 - Outside collaborators ----

# Who are outside collaborators?
outside_users <-
  users("scotgovanalysis", user_type = "outside_collaborators")

# What repositories do they have access to and/or have contributed to?
outside_repos <-
  repo_use %>%
  filter(user %in% outside_users$user & !is.na(role)) %>%
  arrange(tolower(user))

# Who are the admins of repos with outside collaborators?
outside_admins <-
  repo_use %>%
  filter(repo %in% outside_repos$repo &
           role == "admin") %>%
  mutate(member_type = case_when(
    user %in% c("alice-hannah", "tomwilsonsco") ~ "org_admin",
    user %in% outside_users$user ~ "outside_collaborator"
  )) %>%
  arrange(repo)


# 3 - Save data ----

list_data <- list(users  = outside_users,
                  repos  = outside_repos,
                  admins = outside_admins)

write_rds(
  list_data,
  here("reviews", "2025", "data",
       paste0(Sys.Date(), "_outside-collborators.rds"))
)

# xlsx file for manual editing
write_xlsx(
  list_data,
  here("reviews", "2025", "data",
       paste0(Sys.Date(), "_outside-collborators-to-edit.xlsx")),
  format_headers = FALSE
)


### END OF SCRIPT ###
