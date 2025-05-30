% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/users.R
\name{remove_member}
\alias{remove_member}
\title{Remove member from organisation}
\usage{
remove_member(org, user)
}
\arguments{
\item{org}{String. Name of GitHub organisation}

\item{user}{String. GitHub user name.}
}
\value{
A tibble.
}
\description{
Remove member from organisation
}
\examples{
\dontrun{remove_member("scotgovanalysis", "alice-hannah")}
}
