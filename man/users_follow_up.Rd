% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/users.R
\name{users_follow_up}
\alias{users_follow_up}
\title{Organisation users to follow up}
\usage{
users_follow_up(
  org,
  user_type = c("all", "members", "outside_collaborators"),
  return = c("detail", "summary")
)
}
\arguments{
\item{org}{String. Name of GitHub organisation}

\item{user_type}{One of "all", "member", or "outside" (for outside
collaborators). Defauls to "all".}

\item{return}{Either "detail" to return a row for each user, or "summary" to
return a summary of number of users per type of issue.}
}
\value{
A tibble.
}
\description{
Find organisation users who either do not have a public email
address or are outside collaborations.
}
\examples{
\dontrun{
users_follow_up("ScotGovAnalysis")
}
}
