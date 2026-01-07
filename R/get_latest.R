#' Get Latest Measurement
#'
#' Generic function to retrieve the most recent measurement for any variable.
#'
#' @param data A data frame containing the measurement data with columns
#'   `datetime`, `variable`, and `value`.
#' @param variable_name Character string specifying which variable to retrieve
#'   (e.g., "weight", "size", "temperature", "milk_volume").
#'
#' @return A numeric value representing the latest measurement for the specified variable.
#' @export
#'
#' @examples
#' get_latest(example_data, "weight")
#' get_latest(example_data, "size")
#' get_latest(example_data, "temperature")
get_latest <- function(data, variable_name) {
  data |>
    dplyr::filter(variable == variable_name) |>
    dplyr::slice_max(datetime, n = 1) |>
    dplyr::pull(value)
}

#' Get Latest Weight Measurement
#'
#' Retrieves the most recent weight measurement from the dataset.
#' Supports both "weight" (English) and "poids" (French) variable names.
#'
#' @param data A data frame containing the measurement data with columns
#'   `datetime`, `variable`, and `value`.
#'
#' @return A numeric value representing the latest weight measurement in kg.
#' @export
#'
#' @examples
#' get_latest_weight(example_data)
get_latest_weight <- function(data) {
  # Try "poids" first (French), then "weight" (English) if not found
  weight_data <- data |> dplyr::filter(variable %in% c("poids", "weight"))
  if (nrow(weight_data) == 0) {
    return(NA_real_)
  }
  weight_data |>
    dplyr::slice_max(datetime, n = 1) |>
    dplyr::pull(value)
}

#' Get Latest Size Measurement
#'
#' Retrieves the most recent size (height) measurement from the dataset.
#' Supports both "size" (English) and "taille" (French) variable names.
#'
#' @param data A data frame containing the measurement data with columns
#'   `datetime`, `variable`, and `value`.
#'
#' @return A numeric value representing the latest size measurement in cm.
#' @export
#'
#' @examples
#' get_latest_size(example_data)
get_latest_size <- function(data) {
  # Try "taille" first (French), then "size" (English) if not found
  size_data <- data |> dplyr::filter(variable %in% c("taille", "size"))
  if (nrow(size_data) == 0) {
    return(NA_real_)
  }
  size_data |>
    dplyr::slice_max(datetime, n = 1) |>
    dplyr::pull(value)
}

#' Get Latest Temperature Measurement
#'
#' Retrieves the most recent temperature measurement from the dataset.
#'
#' @param data A data frame containing the measurement data with columns
#'   `datetime`, `variable`, and `value`.
#'
#' @return A numeric value representing the latest temperature measurement in °C.
#' @export
#'
#' @examples
#' get_latest_temperature(example_data)
get_latest_temperature <- function(data) {
  get_latest(data, "temperature")
}

#' Get Latest Milk Volume Measurement
#'
#' Retrieves the most recent milk volume measurement from the dataset.
#' Supports both "milk_volume" (English) and "biberon" (French) variable names.
#'
#' @param data A data frame containing the measurement data with columns
#'   `datetime`, `variable`, and `value`.
#'
#' @return A numeric value representing the latest milk volume measurement in ml.
#' @export
#'
#' @examples
#' get_latest_milk_volume(example_data)
get_latest_milk_volume <- function(data) {
  # Try "biberon" first (French), then "milk_volume" (English) if not found
  milk_data <- data |> dplyr::filter(variable %in% c("biberon", "milk_volume"))
  if (nrow(milk_data) == 0) {
    return(NA_real_)
  }
  milk_data |>
    dplyr::slice_max(datetime, n = 1) |>
    dplyr::pull(value)
}

#' Get Latest Daily Bottle Count
#'
#' Calculates the number of bottles consumed in the last 24 hours.
#'
#' @param data A data frame containing the measurement data with columns
#'   `datetime`, `variable`, and `value`.
#'
#' @return An integer representing the number of bottles in the last 24 hours.
#' @export
#'
#' @examples
#' get_latest_daily_bottles(example_data)
get_latest_daily_bottles <- function(data) {
  milk_data <- data |> dplyr::filter(variable %in% c("biberon", "milk_volume"))
  if (nrow(milk_data) == 0) {
    return(NA_integer_)
  }

  latest_datetime <- milk_data |>
    dplyr::pull(datetime) |>
    max(na.rm = TRUE)

  cutoff_datetime <- latest_datetime - lubridate::hours(24)

  milk_data |>
    dplyr::filter(datetime >= cutoff_datetime) |>
    nrow()
}

#' Get Latest Daily Milk Volume
#'
#' Calculates the total milk volume consumed in the last 24 hours.
#'
#' @param data A data frame containing the measurement data with columns
#'   `datetime`, `variable`, and `value`.
#'
#' @return A numeric value representing the total volume in ml in the last 24 hours.
#' @export
#'
#' @examples
#' get_latest_daily_volume(example_data)
get_latest_daily_volume <- function(data) {
  milk_data <- data |> dplyr::filter(variable %in% c("biberon", "milk_volume"))
  if (nrow(milk_data) == 0) {
    return(NA_real_)
  }

  latest_datetime <- milk_data |>
    dplyr::pull(datetime) |>
    max(na.rm = TRUE)

  cutoff_datetime <- latest_datetime - lubridate::hours(24)

  milk_data |>
    dplyr::filter(datetime >= cutoff_datetime) |>
    dplyr::pull(value) |>
    sum(na.rm = TRUE)
}

#' Get Selle Data
#'
#' Retrieves all selle (poop) entries with parsed texture and color information.
#'
#' @param data A data frame containing the measurement data with columns
#'   `datetime`, `variable`, `value`, `unit`, and `notes`.
#'
#' @return A data frame with columns: datetime, texture (numeric value 1-4),
#'   color (extracted from notes), and notes.
#' @export
#'
#' @examples
#' get_selle_data(example_data)
get_selle_data <- function(data) {
  selle_data <- data |> dplyr::filter(variable == "selle")
  if (nrow(selle_data) == 0) {
    return(data.frame(
      datetime = lubridate::POSIXct(),
      texture = numeric(),
      color = character(),
      notes = character()
    ))
  }

  selle_data |>
    dplyr::mutate(
      texture = value,
      color = stringr::str_extract(notes, "(?<=Couleur: )\\w+")
    ) |>
    dplyr::select(datetime, texture, color, notes)
}

#' Get Latest Selle
#'
#' Retrieves the datetime of the most recent selle (poop) entry.
#'
#' @param data A data frame containing the measurement data with columns
#'   `datetime`, `variable`, and `value`.
#'
#' @return A POSIXct datetime of the most recent selle entry, or NA if no entries.
#' @export
#'
#' @examples
#' get_latest_selle(example_data)
get_latest_selle <- function(data) {
  selle_data <- data |> dplyr::filter(variable == "selle")
  if (nrow(selle_data) == 0) {
    return(as.POSIXct(NA))
  }

  selle_data |>
    dplyr::slice_max(datetime, n = 1) |>
    dplyr::pull(datetime)
}
