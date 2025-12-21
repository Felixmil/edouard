#' Plot Activity Equalizer Chart
#'
#' Creates an equalizer-style visualization showing measurement activity over time.
#' Each measurement is represented as a colored square, stacked vertically by day.
#'
#' @param data A data frame containing measurement data with `date` and `variable_label` columns
#'
#' @return A ggplot2 object
#' @export
#'
#' @examples
#' \dontrun{
#' plot_activity_equalizer(edouard_data)
#' }
plot_activity_equalizer <- function(data) {
  # Prepare data: count measurements per day and variable type
  activity_data <- data |>
    dplyr::group_by(date, variable_label) |>
    dplyr::summarise(count = dplyr::n(), .groups = "drop") |>
    dplyr::arrange(date, variable_label) |>
    dplyr::group_by(date) |>
    dplyr::mutate(
      # Create stacked position (each measurement is a unit square)
      ymax = cumsum(count),
      ymin = dplyr::lag(ymax, default = 0),
      # Expand to individual squares
      squares = purrr::map2(ymin, ymax, ~ seq(.x + 1, .y))
    ) |>
    tidyr::unnest(squares)

  # Create equalizer chart with square tiles
  ggplot2::ggplot(
    activity_data,
    ggplot2::aes(x = date, y = squares, fill = variable_label)
  ) +
    ggplot2::geom_tile(color = "white", linewidth = 1.5) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::scale_x_date(
      date_breaks = "1 month",
      date_labels = "%m/%y",
    ) +
    scale_fill_edouard() +
    ggplot2::labs(fill = NULL) +
    ggplot2::theme_void() +
    ggplot2::theme(
      text = ggplot2::element_text(size = 14),
      legend.position = "top",
      plot.margin = ggplot2::margin(10, 10, 10, 10),
      axis.text.x = ggplot2::element_text(vjust = -2)
    ) +
    ggplot2::coord_fixed(ratio = 1)
}
