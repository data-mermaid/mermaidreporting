#' Plot MERMAID benthic PIT percent cover
#'
#' Plot box plots of benthic PIT percent cover by a grouping variable \code{group_var} (e.g. benthic category), separated by a supplied \code{compare_var}.
#'
#' @param .data Sample event data containing average benthic PIT percent cover, with variables indicated in \code{group_var} and \code{compare_var}
#' @param group_var Variable to group by, e.g. \code{percent_cover_by_benthic_category_avg}
#' @param compare_var Variable to compare by, e.g. \code{management_rules}
#'
#' @export
#' @examples
#' library(mermaidr)
#' mermaid_search_projects(name = "XPDC Kei Kecil 2018") %>%
#'   mermaid_get_project_endpoint(endpoint = "benthicpits/sampleevents") %>%
#'   mermaid_clean_management_rules() %>%
#'   mermaid_plot_benthic_pit_percent_cover(percent_cover_by_benthic_category_avg, management_rules)
mermaid_plot_benthic_pit_percent_cover <- function(.data, group_var, compare_var) {

  group_var_string <- rlang::quo_text(rlang::enquo(group_var))
  compare_var_string <- rlang::quo_text(rlang::enquo(compare_var))

  check_data(.data, group_var_string, compare_var_string)

  .data <- .data %>%
    tidyr::unpack({{ group_var }}, names_sep = "_") %>%
    tidyr::pivot_longer(
      cols = dplyr::contains(paste0(group_var_string, "_")),
      names_to = "group",
      names_prefix = paste0(group_var_string, "_"),
      values_to = "value"
    ) %>%
    tidyr::drop_na(value) %>%
    dplyr::mutate(group = forcats::fct_expand(group, "other"),
                  group = forcats::fct_relevel(group, "other", after = Inf))

  .data %>%
    ggplot2::ggplot(ggplot2::aes(x = {{ compare_var }}, y = .data$value)) +
    ggplot2::geom_boxplot() +
    ggplot2::facet_wrap(dplyr::vars(.data$group), scales = "free_y", ncol = 4) +
    ggplot2::scale_y_continuous("Mean Percent Cover") +
    ggplot2::scale_x_discrete(snakecase::to_title_case(compare_var_string)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
}
