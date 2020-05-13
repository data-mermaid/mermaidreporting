#' Clean MERMAID Management Rules
#'
#' Clean up MERMAID management rules. Recodes any (or multiple of, and values are case insensitive) Period Closure, Size Limits, Gear Restrictions, and Species Restrictions to "Partial Restrictions". Other rules are converted to title case (e.g. no take to "No Take") if they aren't already. Empty strings ("") are converted to NAs.
#'
#'
#' @param .data Input data
#' @param management_rules Column containing management rules. Defaults to \code{management_rules}
#' @param name Name of the clean column, e.g. "Management Rules". By default just replaces the column in \code{.management_rules}.
#' @param remove Whether to remove the \code{.management_rules} column (TRUE/FALSE). Only applicable if \code{.name} is set.
#' @param missing_value How to recode missing values. Defaults to a literal NA. Can change to a specific value, e.g. "Not Specified".
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(mermaidr)
#' sample_events <- mermaid_search_projects(name = "Aceh Jaya Coastal Park") %>%
#'   mermaid_get_project_data("fishbelt", "sampleevents")
#'
#' unique(sample_events[["management_rules"]])
#' # [1] "gear restriction; size limits; species restriction"
#' # [2] "open access"
#' # [3] "no take"
#'
#' sample_events_clean <- sample_events %>%
#'   mermaid_clean_management_rules()
#'
#' unique(sample_events_clean[["management_rules"]])
#' # [1] "Partial Restrictions" "Open Access"
#' # [3] "No Take"
#' }
mermaid_clean_management_rules <- function(.data, management_rules = management_rules, name = NA, remove = !is.na(name), missing_value = NA_character_) {
  validate_clean_management_rules(.data, management_rules = rlang::quo_name(rlang::enquo(management_rules)), name, remove)

  check_management_rules_values(values = dplyr::pull(.data, {{ management_rules }}))

  clean_rules <- .data %>%
    dplyr::mutate(clean_rules := dplyr::case_when(
      tolower({{ management_rules }}) == "no take" ~ "No Take",
      tolower({{ management_rules }}) == "open access" ~ "Open Access",
      {{ management_rules }} == "" ~ missing_value,
      grepl(
        "periodic closure|size limit|gear restriction|species restriction",
        tolower({{ management_rules }})
      ) ~ "Partial Restrictions"
    ))

  if (is.na(name) | rlang::quo_name(rlang::enquo(management_rules)) == name) {
    clean_rules <- clean_rules %>%
      dplyr::select(-{{ management_rules }}) %>%
      dplyr::rename({{ management_rules }} := clean_rules)
  } else {
    clean_rules <- clean_rules %>%
      dplyr::rename({{ name }} := clean_rules)

    if (remove) {
      clean_rules <- clean_rules %>%
        dplyr::select(-{{ management_rules }})
    }
  }

  return(clean_rules)
}

validate_clean_management_rules <- function(.data, management_rules, name, remove) {
  # Check .data is a data frame
  if (!inherits(.data, "data.frame")) {
    stop("`.data` must be a data frame.", call. = FALSE)
  }

  # management_rules is in .data
  if (!management_rules %in% names(.data)) {
    stop("`.data` does not contain column `", management_rules, "`", call. = FALSE)
  }
  # .name is the same as management_rules
  if (!is.na(name) & management_rules == name) {
    message("Don't specify `name` if it's the same as `management_rules`.", call. = FALSE)
  }

  # .remove is TRUE and .name is not set
  if (remove & is.na(name)) {
    stop("You can't set `remove = TRUE` (to remove the original `management_rules` column ) without specifying `name`.", call. = FALSE)
  }
}

check_management_rules_values <- function(values) {
  management_rules_values <- unique(tolower(values))
  expected_matches <- "periodic closure|size limit|gear restriction|species restriction|no take|open access"
  values_expected <- grepl(expected_matches, management_rules_values) | is.na(management_rules_values) | management_rules_values == ""

  if (!all(values_expected)) {
    warning("Failed to clean all management rules values.\nExpected values: periodic closure, size limit, gear restriction, species restriction, no take, open access\nUnexpected values coerced to NA", call. = FALSE)
  }
}
