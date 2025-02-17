# Name: 05_review-emails.R
# Desc: Review members with no public email
# Date: February 2025

# 0 - Load packages ----

library(ghadmin)
library(dplyr)


# 1 - Get members with no public email ----

missing_email <-
  users("scotgovanalysis", user_type = "members") %>%
  filter(is.na(email) | !str_detect(email, "gov\\.scot$"))


### END OF SCRIPT ###
