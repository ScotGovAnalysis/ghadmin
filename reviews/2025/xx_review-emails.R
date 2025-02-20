# Name: xx_review-emails.R
# Desc: Review members with no public email
# Date: February 2025

# 0 - Run set up ----

source(here::here("reviews", "2025", "00_setup.R"))


# 1 - Get members with no public email ----

missing_email <-
  users("scotgovanalysis", user_type = "members") %>%
  filter(is.na(email) | !str_detect(email, "gov\\.scot$"))


### END OF SCRIPT ###
