#' Process Edouard Data with Human-Readable Enhancements
#'
#' Adds emoji indicators, readable labels, time-of-day categories, and
#' value-based categories to raw tracking data.
#'
#' @param data A data frame with columns: date, time, variable, value, unit, notes
#'
#' @return A processed data frame with additional columns:
#'   \itemize{
#'     \item datetime: Combined date and time as POSIXct
#'     \item emoji: Variable-specific emoji (🍼 for milk_volume, 🌡️ for temperature, etc.)
#'     \item variable_label: Human-readable variable names
#'     \item description: Friendly text combining emoji, label, and value
#'   }
#'
#' @examples
#' \dontrun{
#' raw_data <- read.csv("data-raw/edouard_data.csv")
#' processed_data <- process_edouard_data(raw_data)
#' }
#'
#' @export
process_edouard_data <- function(data) {
  data |>
    dplyr::mutate(
      # Datetime
      datetime = lubridate::as_datetime(paste(date, time)),
    ) |>
    dplyr::mutate(
      # Add emoji based on variable type
      emoji = dplyr::case_when(
        variable %in% c("temperature") ~ "🌡️",
        variable %in% c("milk_volume", "biberon") ~ "🍼",
        variable %in% c("weight", "poids") ~ "⚖️",
        variable %in% c("size", "taille") ~ "📏",
        variable %in% c("evenement", "event") ~ "🔔",
        TRUE ~ "📊"
      ),
      # Create human-readable variable label
      variable_label = dplyr::case_when(
        variable %in% c("temperature") ~ "Température",
        variable %in% c("milk_volume", "biberon") ~ "Biberon",
        variable %in% c("weight", "poids") ~ "Poids",
        variable %in% c("size", "taille") ~ "Taille",
        variable %in% c("evenement", "event") ~ "Événement",
        TRUE ~ variable
      ),
      # Add friendly description combining emoji, label, and value
      description = paste0(emoji, " ", variable_label, ": ", value, " ", unit),
      # Title case Notes
      notes = stringr::str_to_sentence(notes)
      # Add time of day indicator with emoji
      # time_of_day = dplyr::case_when(
      #   lubridate::hour(time) >= 5 & lubridate::hour(time) < 12 ~ "🌅 Morning",
      #   lubridate::hour(time) >= 12 &
      #     lubridate::hour(time) < 17 ~ "☀️ Afternoon",
      #   lubridate::hour(time) >= 17 & lubridate::hour(time) < 21 ~ "🌆 Evening",
      #   TRUE ~ "🌙 Night"
      # ),
      # Add category with emoji based on variable type and value
      # category = dplyr::case_when(
      #   # Milk volume categories
      #   variable == "milk_volume" & value < 80 ~ "💧 Small",
      #   variable == "milk_volume" & value >= 80 & value < 150 ~ "🥛 Medium",
      #   variable == "milk_volume" & value >= 150 ~ "🍶 Large",
      #   # Temperature categories (assuming Celsius)
      #   variable == "temperature" & value < 36.5 ~ "🧊 Low",
      #   variable == "temperature" & value >= 36.5 & value < 37.5 ~ "✅ Normal",
      #   variable == "temperature" &
      #     value >= 37.5 &
      #     value < 38.5 ~ "🔥 Elevated",
      #   variable == "temperature" & value >= 38.5 ~ "🚨 Fever",
      #   # Weight categories (assuming kg for baby)
      #   variable == "weight" & value < 3 ~ "🪶 Light",
      #   variable == "weight" & value >= 3 & value < 5 ~ "👶 Average",
      #   variable == "weight" & value >= 5 ~ "💪 Growing",
      #   # Size categories (assuming cm)
      #   variable == "size" & value < 50 ~ "📐 Small",
      #   variable == "size" &
      #     value >= 50 &
      #     variable == "size" &
      #     value < 60 ~ "📊 Average",
      #   variable == "size" & value >= 60 ~ "📈 Tall",
      #   TRUE ~ NA_character_
      # )
    ) |>
    # Reorder columns for better readability
    dplyr::select(
      date,
      time,
      # time_of_day,
      emoji,
      variable,
      variable_label,
      value,
      unit,
      # category,
      description,
      notes,
      dplyr::everything()
    ) |>
    dplyr::arrange(datetime)
}
