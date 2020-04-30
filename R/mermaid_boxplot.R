#' MERMAID boxplots
#'
#' @param .data Sample event data containing mean fish belt biomass, with variables indicated in \code{.group_var} and \code{.compare_var}. Likely the output from a \code{mermaid_get_project_endpoint()} call.
#' @param .group_var Variable to group by, e.g. \code{biomass_kgha_by_trophic_group_avg}. Must be a data frame column.
#' @param group_var_string String version of \code{.group_var}. Internal use only.
#' @param .compare_var Variable to compare by, e.g. \code{management_rules}
#' @param compare_var_string String version of \code{.compare_var}. Internal use only.
#' @param y_axis_name Name of the y axis. Internal use only.
#' @param .clean_values Whether to clean the values in \code{.group_var} and \code{.compare_var} (with title case as default - see \code{.clean_values_case}), in case they are not clean (e.g. contain dashes, underscores, are lowercase, etc). Defaults to TRUE.
#' @param .clean_values_case The desired clean values case (default is "title"). For example, if a value is "invertivore-mobile", title case converts it to "Invertivore Mobile" while sentence case converts it to "Invertivore mobile". If you don't want to replace dashes with spaces (e.g. get "Interivore-Mobile", set \code{.replace_dashes = FALSE}.
#' @param .replace_dashes Whether to also remove dashes from values when cleaning them. Defaults to TRUE.
mermaid_boxplot <- function(.data, .group_var, group_var_string, .compare_var, compare_var_string, y_axis_name, .clean_values = TRUE, .clean_values_case = c("title", "sentence"), .replace_dashes = TRUE) {

  check_df_col(.data, group_var_string)
  check_data(.data, group_var_string, compare_var_string)

      .data <- .data %>%
      tidyr::unpack(!!.group_var, names_sep = "_") %>%
      tidyr::pivot_longer(
        cols = dplyr::contains(paste0(group_var_string, "_")),
        names_to = "group",
        names_prefix = paste0(group_var_string, "_"),
        values_to = "value"
      ) %>%
      tidyr::drop_na(.data$value)


  if (.clean_values) {
    if (.clean_values_case == "title") {
      .data <- .data %>%
        dplyr::mutate_at(dplyr::vars(.data$group, !!.compare_var), stringr::str_to_title)
    } else if (.clean_values_case == "sentence") {
      .data <- .data %>%
        dplyr::mutate_at(dplyr::vars(.data$group, !!.compare_var), stringr::str_to_sentence)
    }
    if (.replace_dashes) {
      .data <- .data %>%
        dplyr::mutate_at(dplyr::vars(group, !!.compare_var), stringr::str_replace_all, "-", " ")
    }
  }

  .data %>%
    ggplot2::ggplot(ggplot2::aes(x = !!.compare_var, y = .data$value)) +
    ggplot2::geom_boxplot() +
    ggplot2::facet_wrap(dplyr::vars(.data$group), scales = "free_y", ncol = 4) +
    ggplot2::scale_y_continuous(y_axis_name) +
    ggplot2::scale_x_discrete(snakecase::to_title_case(compare_var_string)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
}

check_df_col <- function(.data, .col_string) {
  if(!inherits(.data[[.col_string]], "data.frame")) {
    stop("Column `", .col_string, "` must be a data frame column (the raw output from a `mermaid_get_project_endpoint()` function).\nIf you ran `mermaid_clean_columns()` on the data, use the data *before* that cleaning to plot instead.", call. = FALSE)
  }
}

#' Plot MERMAID fish belt biomass
#'
#' Plot box plots of fish belt biomass by a grouping variable \code{.group_var} (e.g. trophic group), separated by a supplied \code{.compare_var}.
#'
#' @inheritParams mermaid_boxplot
#' @param .group_var Variable to group by, e.g. \code{biomass_kgha_by_trophic_group_avg}
#' @export
#' @examples
#' \donttest{
#' library(mermaidr)
#' events <- mermaid_search_projects(name = "XPDC Kei Kecil 2018") %>%
#'   mermaid_get_project_data("fishbelt", "sampleevents")
#'
#' events %>%
#'   mermaid_plot_fish_belt_biomass(biomass_kgha_by_trophic_group_avg, reef_exposure)
#' }
mermaid_plot_fish_belt_biomass <- function(.data, .group_var, .compare_var, .clean_values = TRUE, .clean_values_case = c("title", "sentence"), .replace_dashes = TRUE) {
  .group_var <- rlang::enquo(.group_var)
  group_var_string <- rlang::quo_name(.group_var)
  .compare_var <- rlang::enquo(.compare_var)
  compare_var_string <- rlang::quo_name(.compare_var)

  .clean_values_case <- match.arg(.clean_values_case)

  mermaid_boxplot(.data, .group_var, group_var_string, .compare_var, compare_var_string, "Mean Total Biomass (kg/h)", .clean_values, .clean_values_case, .replace_dashes)
}
