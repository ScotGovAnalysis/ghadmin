# Name: 00_setup.R
# Desc: Load packages and set parameters
# Date: February 2025

# 1 - Load packages ----

library(ghadmin)
library(dplyr)
library(stringr)
library(purrr)
library(here)
library(cli)
library(readr)
library(writexl)


# 2 - Set repository details for member review ----

review_params <- list(org   = "scotgovanalysis",
                      repo  = "ghadmin",
                      label = "2025-review",
                      deadline = "Friday 21 March",
                      admin = c("alice-hannah", "tomwilsonsco"))


### END OF SCRIPT ###
