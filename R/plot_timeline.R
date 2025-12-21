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
      side = ifelse(row_number() %% 2 == 1, "left", "right"),
      label = paste0(
        format(datetime, "%d/%m %H:%M"),
        "\n",
        notes
      ),
      x_position = ifelse(side == "left", 0.85, 1.15)
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
      aes(x = 1, xend = 1, y = min(datetime), yend = max(datetime)),
      color = "black",
      linewidth = 1
    ) +
    # Horizontal segments to labels
    geom_segment(
      aes(x = 1, xend = x_position, y = datetime, yend = datetime),
      color = "grey50",
      linewidth = 0.3
    ) +
    # Points on the line
    geom_point(
      aes(x = 1, color = type),
      size = 4
    ) +
    # Text labels
    geom_text(
      aes(x = x_position, label = label, hjust = ifelse(side == "left", 1, 0)),
      size = 3,
      vjust = 0
    ) +
    # Color scale
    scale_color_manual(
      values = c("Événement" = "black", "Symptôme" = "red")
    ) +
    # Set x-axis limits to prevent text from being cut off
    scale_x_continuous(limits = c(0, 2), expand = c(0.1, 0.1)) +
    # Clean theme
    theme_void() +
    theme(
      legend.position = "none",
      legend.title = element_blank()
    )
}
