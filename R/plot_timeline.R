#' Prepare Timeline Data
#'
#' Filters and prepares data for timeline visualization of events and symptoms.
#'
#' @param data A data frame containing health tracking data
#' @return A data frame with columns: datetime, type, side, label, x_position
#' @export
prepare_timeline_data <- function(data) {
  timeline_data <- data |>
    # filter(variable %in% c("evenement", "symptome")) |>
    filter(variable %in% c("evenement")) |>
    arrange(datetime) |>
    mutate(
      type = case_when(
        variable == "evenement" ~ "Événement",
        variable == "symptome" ~ "Symptôme"
      ),
      type = factor(type, levels = c("Événement", "Symptôme")),
      label = paste0(
        format(datetime, "%d/%m/%y %H:%M"),
        ": ",
        notes
      ),
      x_position = 0.1,
      time_index = row_number()
    )

  return(timeline_data)
}

#' Plot Timeline
#'
#' Creates a vertical timeline visualization of events and symptoms.
#'
#' @param data A data frame containing health tracking data
#' @return A ggplot object
#' @export
plot_timeline <- function(data) {
  timeline_data <- prepare_timeline_data(data)

  # Handle empty data case
  if (nrow(timeline_data) == 0) {
    return(
      ggplot() +
        annotate(
          "text",
          x = 1,
          y = 1,
          label = "Aucun événement ou symptôme enregistré",
          size = 5
        ) +
        theme_void()
    )
  }

  # Create the timeline plot
  ggplot(timeline_data, aes(y = datetime)) +
    # Vertical central line
    geom_segment(
      aes(x = -0.1, xend = -0.1, y = min(datetime), yend = max(datetime)),
      color = "black",
      linewidth = 1
    ) +
    # Points on the line with gradient
    geom_point(
      aes(x = -0.1, color = time_index),
      size = 4
    ) +
    # Text labels
    geom_text(
      aes(x = 0, label = label, hjust = 0),
      size = 4,
      vjust = 0.5
    ) +
    # Color gradient scale (dark blue for recent, light blue for old)
    scale_color_gradient(
      low = "#B3D9FF",
      high = "#1E3A5F"
    ) +
    # Set x-axis limits to center the line
    scale_x_continuous(limits = c(-1, 1), expand = c(0, 0)) +
    scale_y_datetime(expand = c(0.1, 0.1)) +
    # Clean theme
    theme_void() +
    theme(
      legend.position = "none"
    )
}
