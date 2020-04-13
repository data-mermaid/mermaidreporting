#' @param .data Sample event data containing mean fish belt biomass, with variables indicated in \code{group_var} and \code{compare_var}
#' @param group_var Variable to group by, e.g. \code{biomass_kgha_by_trophic_group_avg}
#' @param compare_var Variable to compare by, e.g. \code{management_rules}
mermaid_boxplot <- function(.data, group_var, group_var_string, compare_var, compare_var_string, y_axis_name) {
  check_data(.data, group_var_string, compare_var_string)

  .data <- .data %>%
    tidyr::unpack(!!group_var, names_sep = "_") %>%
    tidyr::pivot_longer(
      cols = dplyr::contains(paste0(group_var_string, "_")),
      names_to = "group",
      names_prefix = paste0(group_var_string, "_"),
      values_to = "value"
    ) %>%
    tidyr::drop_na(value) %>%
    dplyr::mutate(
      group = forcats::fct_expand(group, "other"),
      group = forcats::fct_relevel(group, "other", after = Inf)
    )

  .data %>%
    ggplot2::ggplot(ggplot2::aes(x = !!compare_var, y = .data$value)) +
    ggplot2::geom_boxplot() +
    ggplot2::facet_wrap(dplyr::vars(.data$group), scales = "free_y", ncol = 4) +
    ggplot2::scale_y_continuous(y_axis_name) +
    ggplot2::scale_x_discrete(snakecase::to_title_case(compare_var_string)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
}

#' Plot MERMAID fish belt biomass
#'
#' Plot box plots of fish belt biomass by a grouping variable \code{group_var} (e.g. trophic group), separated by a supplied \code{compare_var}.
#'
#' @inheritParams mermaid_boxplot
#' @param group_var Variable to group by, e.g. \code{biomass_kgha_by_trophic_group_avg}
#' @export
#' @examples
#' library(mermaidr)
#' mermaid_search_projects(name = "XPDC Kei Kecil 2018") %>%
#'   mermaid_get_project_endpoint(endpoint = "beltfishes/sampleevents") %>%
#'   mermaid_plot_fish_belt_biomass(biomass_kgha_by_trophic_group_avg, reef_exposure)
mermaid_plot_fish_belt_biomass <- function(.data, group_var, compare_var) {
  group_var <- rlang::enquo(group_var)
  group_var_string <- rlang::quo_name(group_var)
  compare_var <- rlang::enquo(compare_var)
  compare_var_string <- rlang::quo_name(compare_var)

  mermaid_boxplot(.data, group_var, group_var_string, compare_var, compare_var_string, "Mean Total Biomass (kg/h)")
}
