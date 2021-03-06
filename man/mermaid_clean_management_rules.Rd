% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mermaid_clean_management_rules.R
\name{mermaid_clean_management_rules}
\alias{mermaid_clean_management_rules}
\title{Clean MERMAID Management Rules}
\usage{
mermaid_clean_management_rules(
  .data,
  management_rules = management_rules,
  name = NA,
  remove = !is.na(name),
  missing_value = NA_character_
)
}
\arguments{
\item{.data}{Input data}

\item{management_rules}{Column containing management rules. Defaults to \code{management_rules}.}

\item{name}{Name of the clean column, e.g. "Management Rules". By default just replaces the column in \code{management_rules}.}

\item{remove}{Whether to remove the \code{management_rules} column (TRUE/FALSE). Only applicable if \code{name} is set - defaults to TRUE.}

\item{missing_value}{How to recode missing values. Defaults to a literal NA. Can change to a specific value, e.g. "Not Specified".}
}
\description{
Clean up MERMAID management rules. Recodes any (or multiple of, and values are case insensitive) Period Closure, Size Limits, Gear Restrictions, and Species Restrictions to "Partial Restrictions". Other rules are converted to title case (e.g. no take to "No Take") if they aren't already. Empty strings ("") are converted to \code{NA}s by default, but this option can be changed via \code{missing_value}.
}
\examples{
\donttest{
library(mermaidr)
sample_events <- mermaid_search_projects(name = "Aceh Jaya Coastal Park") \%>\%
  mermaid_get_project_data("fishbelt", "sampleevents")

unique(sample_events[["management_rules"]])
# [1] "gear restriction; size limits; species restriction"
# [2] "open access"
# [3] "no take"

sample_events_clean <- sample_events \%>\%
  mermaid_clean_management_rules()

unique(sample_events_clean[["management_rules"]])
# [1] "Partial Restrictions" "Open Access"
# [3] "No Take"
}
}
