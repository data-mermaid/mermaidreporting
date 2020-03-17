#' Benthic PIT and Beltfish Summary Table
#'
#' Generates a summary table that includes percent cover for all benthic categories, mean total biomass for all (main) fish families, overall mean total biomass, as well as site metadata, for the given sites:
#'
#' @param benthicpit_data Benthic PIT data
#' @param beltfish_data Beltfish data
#'
#' @examples
#' # summary_table(fiji_benthicpit_summary, fiji_beltfish_summary)
summary_table <- function(benthicpit_data, beltfish_data) {
  check_data(
    benthicpit_data, "mean_depth", "benthic_category", "mean_percent_cover",
    "country", "site", "year", "latitude", "longitude", "exposure", "reef_type", "reef_zone",
    "management_name", "management_rules", "estimated_compliance"
  )
  check_data(
    beltfish_data, "mean_depth", "fish_family", "mean_total_biomass_fish_family",
    "country", "site", "year", "latitude", "longitude", "exposure", "reef_type", "reef_zone",
    "management_name", "management_rules", "estimated_compliance"
  )

  benthicpit_summary_wide <- benthicpit_data %>%
    dplyr::select(-.data$mean_depth) %>%
    tidyr::pivot_wider(
      names_from = .data$benthic_category,
      values_from = .data$mean_percent_cover,
      names_prefix = "percent_cover_"
    ) %>%
    janitor::clean_names()

  beltfish_summary_wide <- beltfish_data %>%
    dplyr::select(-.data$mean_depth) %>%
    tidyr::pivot_wider(
      names_from = .data$fish_family,
      values_from = .data$mean_total_biomass_fish_family,
      names_prefix = "mean_total_biomass_"
    ) %>%
    janitor::clean_names()

  benthicpit_summary_wide %>%
    dplyr::full_join(beltfish_summary_wide,
      by = c(
        "country", "site", "year", "latitude", "longitude",
        "exposure", "reef_type", "reef_zone", "management_name",
        "management_rules", "estimated_compliance"
      )
    )
}
