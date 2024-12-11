
library(ghadmin)
library(dplyr)

# 1 - User summary ----

all_users <- users("ScotGovAnalysis")

follow_up <- users_follow_up("ScotGovAnalysis")

# 2 - Repository use ----

repos <- repos("scotgovanalysis")

access <- repo_access("scotgovanalysis", repos$name)

contrib <- repo_contrib("scotgovanalysis", repos$name)


# 3 - Outside collaborators ----

outside <-
  users_follow_up("ScotGovAnalysis",
                  user_type = "outside_collaborators")

outside_repos <-
  outside %>%
  select(user) %>%
  left_join(access, by = "user") %>%
  left_join(contrib, by = c("user", "name")) %>%
  mutate(contributions = tidyr::replace_na(contributions, 0))

outside <-
  outside %>%
  left_join(
    outside_repos %>%
      group_by(user) %>%
      summarise(repos = n_distinct(name),
                contributions = sum(contributions, na.rm = TRUE),
                .groups = "drop"),
    by = "user"
  )

outside_access <-
  repo_access("scotgovanalysis", repos("scotgovanalysis")$name) %>%
  filter(user %in% outside$user)

outside_contrib <-
  repo_contrib("scotgovanalysis", repos("scotgovanalysis")$name) %>%
  filter(user %in% outside$user)


# What repos do they have access to
# Who are owners of these repos


# 2 - Check all members in org-admin-all-members ----

all_org_team_missing <-
  all_users %>%
  filter(type == "members") %>%
  filter(!username %in%
           team_members("ScotGovAnalysis", "org-admin-all-members")$member)
