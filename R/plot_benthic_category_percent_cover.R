#' Plot benthic category percent cover
#'
#' Plot box plots of benthic category percent cover, separated by a supplied \code{variable}.
#'
#' @param data Data with percent cover by benthic category, with variables \code{benthic_category} and \code{mean_percent_cover}
#' @param variable Variable to compare by, e.g. \code{management_rules}
#'
#' @examples
#' # plot_benthic_category_percent_cover(fiji_benthicpit_summary, management_rules)
plot_benthic_category_percent_cover <- function(data, variable) {
  check_data(data, "benthic_category", rlang::quo_text(rlang::enquo(variable)), "mean_percent_cover")
  data %>%
    dplyr::select(.data$benthic_category, {{ variable }}, .data$mean_percent_cover) %>%
    ggplot2::ggplot(ggplot2::aes(x = {{ variable }}, y = .data$mean_percent_cover)) +
    ggplot2::geom_boxplot() +
    ggplot2::facet_wrap(dplyr::vars(.data$benthic_category), scales = "free_y", ncol = 2) +
    ggplot2::scale_y_continuous("Mean Percent Cover", labels = scales::percent) +
    ggplot2::scale_x_discrete(snakecase::to_title_case(deparse(substitute(variable)))) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
}
