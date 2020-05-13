#' Expand columns in MERMAID output
#'
#' Expands data-frame columns (df-cols) that come from \code{mermaidr}. Optionally cleans the column names.
#'
#' @param .data Input data
#' @param append_column_prefix Whether to append the df-cols name as a prefix. Defaults to FALSE
#' @param clean_names Whether clean the new column names (with snake case as default - see \code{clean_names_case}), in case they are not clean (e.g. contain spaces, dashes, etc). Note that the name cleaning applies to *all* columns, not just ones resulting from the df-cols.
#' @param clean_names_case The desired clean names case (default is "snake").
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(mermaidr)
#' units <- mermaid_get_my_projects() %>%
#'   mermaid_get_project_data("fishbelt", "sampleunits", limit = 1)
#'
#' names(units)
#' # [1] "project"                       "tags"
#' # [3] "country"                       "site"
#' # [5] "latitude"                      "longitude"
#' # [7] "reef_type"                     "reef_zone"
#' # [9] "reef_exposure"                 "reef_slope"
#' # [11] "tide"                          "current"
#' # [13] "visibility"                    "management"
#' # [15] "management_secondary"          "management_est_year"
#' # [17] "management_size"               "management_parties"
#' # [19] "management_compliance"         "management_rules"
#' # [21] "sample_date"                   "depth"
#' # [23] "transect_number"               "size_bin"
#' # [25] "transect_length"               "transect_width"
#' # [27] "biomass_kgha"                  "biomass_kgha_by_trophic_group"
#' # [29] "data_policy_beltfish"          "project_notes"
#' # [31] "site_notes"                    "management_notes"
#' # [33] "id"                            "contact_link"
#'
#' units_expanded <- units %>%
#'   mermaid_clean_columns()
#'
#' names(units_expanded)
#'
#' # [1] "project"
#' # [2] "tags"
#' # [3] "country"
#' # [4] "site"
#' # [5] "latitude"
#' # [6] "longitude"
#' # [7] "reef_type"
#' # [8] "reef_zone"
#' # [9] "reef_exposure"
#' # [10] "reef_slope"
#' # [11] "tide"
#' # [12] "current"
#' # [13] "visibility"
#' # [14] "management"
#' # [15] "management_secondary"
#' # [16] "management_est_year"
#' # [17] "management_size"
#' # [18] "management_parties"
#' # [19] "management_compliance"
#' # [20] "management_rules"
#' # [21] "sample_date"
#' # [22] "depth"
#' # [23] "transect_number"
#' # [24] "size_bin"
#' # [25] "transect_length"
#' # [26] "transect_width"
#' # [27] "biomass_kgha"
#' # [28] "omnivore"
#' # [29] "piscivore"
#' # [30] "invertivore_mobile"
#' # [31] "invertivore_sessile"
#' # [32] "herbivore_detritivore"
#' # [33] "planktivore"
#' # [34] "herbivore_macroalgae"
#' # [35] "data_policy_beltfish"
#' # [36] "project_notes"
#' # [37] "site_notes"
#' # [38] "management_notes"
#' # [39] "id"
#' # [40] "contact_link"
#' }
mermaid_clean_columns <- function(.data, append_column_prefix = FALSE, clean_names = TRUE, clean_names_case = c("snake", "sentence", "title", "lower_camel", "upper_camel")) {
  clean_names_case <- match.arg(clean_names_case)

  df_cols <- sapply(.data, function(x) inherits(x, "data.frame"))
  df_cols <- names(df_cols[df_cols])

  if (!append_column_prefix) {
    df_unpack <- .data %>%
      tidyr::unpack(
        cols = tidyselect::all_of(df_cols)
      )
  } else {
    df_unpack <- .data %>%
      tidyr::unpack(
        cols = tidyselect::all_of(df_cols),
        names_sep = ifelse(append_column_prefix, "_", NULL)
      )
  }


  if (clean_names) {
    df_unpack %>%
      janitor::clean_names(case = clean_names_case)
  } else {
    df_unpack
  }
}
