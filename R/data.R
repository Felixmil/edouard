#' Newborn Health Tracking Data
#'
#' A dataset containing health metrics tracked for a newborn, including
#' milk quantity, temperature, weight, and other variables.
#'
#' @format A tibble with 2 rows and 6 columns:
#' \describe{
#'   \item{date}{Date of the measurement (Date class)}
#'   \item{time}{Time of the measurement (hms class, time of day)}
#'   \item{variable}{Type of measurement (character: e.g., "milk_quantity", "temperature", "weight")}
#'   \item{value}{Numeric value of the measurement (double)}
#'   \item{unit}{Unit of measurement (character: e.g., "ml", "°C", "kg")}
#'   \item{notes}{Optional notes about the measurement (character)}
#' }
#'
#' @source Data collected from newborn health tracking.
#'   Raw data is stored in \code{inst/extdata/newborn_data.csv}.
#'   See \code{data-raw/edouard_data.R} for data preparation script.
#'
#' @examples
#' # View the data
#' head(edouard_data)
"edouard_data"

"example_data"
