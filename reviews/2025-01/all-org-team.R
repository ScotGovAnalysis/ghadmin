
# Make sure all org members are in all-org team
# Date: December 2024

library(ghadmin)
library(dplyr)

all_members <- users("scotgovanalysis", user_type = "members")

all_org_team <- team_members("ScotGovAnalysis", "org-admin-all-members")

members_to_add <-
  setdiff(all_members$user, all_org_team$user)

purrr::walk(
  members_to_add,
  \(user) add_team_member("scotgovanalysis", "org-admin-all-members", user)
)


### END OF SCRIPT ###
