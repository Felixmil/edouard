#' Filter Data by Variable Type
#'
#' Filters the dataset to include only rows for a specific variable type.
#' Supports both French and English variable names by mapping them to the
#' canonical French names used in the dataset.
#'
#' @param data A data frame containing the measurement data with a `variable` column.
#' @param variable_type Character string specifying which variable to filter for.
#'   Accepts the following values (case-insensitive):
#'   \itemize{
#'     \item "biberon", "milk", "milk_volume" - Milk/bottle feeding data
#'     \item "poids", "weight" - Weight measurements
#'     \item "taille", "size", "height" - Size/height measurements
#'     \item "temperature", "temp" - Temperature measurements
#'     \item "evenement", "event" - Event markers
#'   }
#' @param arrange_by Character string specifying column to arrange by. Default is "datetime".
#'   Use NULL to skip sorting.
#'
#' @return A filtered (and optionally sorted) data frame containing only rows
#'   for the specified variable type.
#'
#' @export
#'
#' @examples
#' # Get all bottle feeding data
#' filter_variable(example_data, "biberon")
#' filter_variable(example_data, "milk")
#'
#' # Get all weight measurements
#' filter_variable(example_data, "poids")
#' filter_variable(example_data, "weight")
#'
#' # Get size data without sorting
#' filter_variable(example_data, "taille", arrange_by = NULL)
filter_variable <- function(data, variable_type, arrange_by = "datetime") {
  # Normalize input to lowercase for case-insensitive matching
  variable_type <- tolower(variable_type)

  # Map English/aliases to French canonical names
  variable_map <- list(
    milk = "biberon",
    milk_volume = "biberon",
    weight = "poids",
    size = "taille",
    height = "taille",
    temp = "temperature",
    event = "evenement"
  )

  # Convert to canonical French name if it's an alias
  if (variable_type %in% names(variable_map)) {
    variable_type <- variable_map[[variable_type]]
  }

  # Filter the data on the variable column
  filtered_data <- data |>
    dplyr::filter(variable == variable_type)

  # Arrange if requested
  if (!is.null(arrange_by) && arrange_by %in% names(filtered_data)) {
    filtered_data <- filtered_data |>
      dplyr::arrange(!!rlang::sym(arrange_by))
  }

  filtered_data
}


#' Get Milk/Bottle Feeding Data
#'
#' Convenience function to filter for milk/bottle feeding measurements.
#' Equivalent to `filter_variable(data, "biberon")`.
#'
#' @param data A data frame containing the measurement data.
#' @param arrange_by Character string specifying column to arrange by. Default is "datetime".
#'
#' @return A filtered data frame containing only bottle feeding data.
#' @export
#'
#' @examples
#' get_milk_data(example_data)
get_milk_data <- function(data, arrange_by = "datetime") {
  filter_variable(data, "biberon", arrange_by = arrange_by)
}


#' Get Weight Data
#'
#' Convenience function to filter for weight measurements.
#' Equivalent to `filter_variable(data, "poids")`.
#'
#' @param data A data frame containing the measurement data.
#' @param arrange_by Character string specifying column to arrange by. Default is "datetime".
#'
#' @return A filtered data frame containing only weight data.
#' @export
#'
#' @examples
#' get_weight_data(example_data)
get_weight_data <- function(data, arrange_by = "datetime") {
  filter_variable(data, "poids", arrange_by = arrange_by)
}


#' Get Size/Height Data
#'
#' Convenience function to filter for size/height measurements.
#' Equivalent to `filter_variable(data, "taille")`.
#'
#' @param data A data frame containing the measurement data.
#' @param arrange_by Character string specifying column to arrange by. Default is "datetime".
#'
#' @return A filtered data frame containing only size data.
#' @export
#'
#' @examples
#' get_size_data(example_data)
get_size_data <- function(data, arrange_by = "datetime") {
  filter_variable(data, "taille", arrange_by = arrange_by)
}


#' Get Temperature Data
#'
#' Convenience function to filter for temperature measurements.
#' Equivalent to `filter_variable(data, "temperature")`.
#'
#' @param data A data frame containing the measurement data.
#' @param arrange_by Character string specifying column to arrange by. Default is "datetime".
#'
#' @return A filtered data frame containing only temperature data.
#' @export
#'
#' @examples
#' get_temperature_data(example_data)
get_temperature_data <- function(data, arrange_by = "datetime") {
  filter_variable(data, "temperature", arrange_by = arrange_by)
}
