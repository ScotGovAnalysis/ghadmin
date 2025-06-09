# Name: 06_review-emails.R
# Desc: Review members with no public email
# Date: May 2025

# 0 - Run set up ----

source(here::here("reviews", "2025", "00_setup.R"))


# 1 - Get members with no public email ----

missing_email <-
  users("scotgovanalysis", user_type = "members") %>%
  filter(is.na(email) | !str_detect(email, "gov\\.scot$"))


# 2 - Save users ----

write_rds(
  missing_email,
  here("reviews", "2025", "data", "2025_missing-email.rds")
)


### END OF SCRIPT ###
