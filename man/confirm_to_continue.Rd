% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{confirm_to_continue}
\alias{confirm_to_continue}
\title{User input to confirm opening review issues}
\usage{
confirm_to_continue(n_members)
}
\arguments{
\item{n_members}{Number of issues to be opened}
}
\value{
If user responds 'y', TRUE is returned invisibly. This allows
following code to run. If user responds 'n', an error will be returned. This
stops following code from running. If user responds something else, they
will be prompted to retry.
}
\description{
User input to confirm opening review issues
}
\examples{
\dontrun{confirm_to_continue(100)}
}
