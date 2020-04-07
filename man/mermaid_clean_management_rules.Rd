% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mermaid_clean_management_rules.R
\name{mermaid_clean_management_rules}
\alias{mermaid_clean_management_rules}
\title{Clean MERMAID Management Rules}
\usage{
mermaid_clean_management_rules(
  .data,
  management_rules,
  .name = NA,
  .remove = !is.na(.name)
)
}
\arguments{
\item{.data}{Input data}

\item{management_rules}{Column containing management rules, e.g. \code{management_rules}}

\item{.name}{Name of the clean column, e.g. "Management Rules". Defaults to NA, in which case the name in \code{management_rules} is used.}

\item{.remove}{Whether to remove the \code{management_rules} column (TRUE/FALSE). Only applicable if \code{.name} is set.}
}
\value{

}
\description{
Clean up MERMAID management rules. Recodes any (or multiple of, and values are case insensitive) Period Closure, Size Limits, Gear Restrictions, and Species Restrictions to "Partial Restrictions". Other rules are converted to title case (e.g. no take to "No Take") if they aren't already. Empty strings ("") are converted to NAs.
}
\examples{
library(mermaidr)
sample_events <- mermaid_search_projects(name = "Aceh Jaya Coastal Park") \%>\%
  mermaid_get_project_endpoint(endpoint = "beltfishes/sampleevents", limit = 5)

unique(sample_events[["management_rules"]])
# [1] "gear restriction; size limits; species restriction"
# [2] "open access"
# [3] "no take"

sample_events_clean <- sample_events \%>\%
  mermaid_clean_management_rules(management_rules, .name = "Management Rules")

unique(sample_events_clean[["Management Rules"]])
# [1] "Partial Restrictions" "Open Access"
# [3] "No Take"
}