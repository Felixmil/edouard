#' Launch Data Entry Shiny App
#'
#' Opens a Shiny application for entering new data into edouard_data.csv.
#' The app provides a form to add entries with preview functionality before
#' committing to the CSV file and rebuilding the package data.
#'
#' @return No return value, launches the Shiny app
#' @export
#'
#' @examples
#' \dontrun{
#' # Launch the data entry application
#' launch_data_entry()
#' }
launch_data_entry <- function() {
  app_dir <- system.file("shiny/data_entry", package = "edouard")

  if (app_dir == "" || !dir.exists(app_dir)) {
    stop(
      "Could not find Shiny app directory. ",
      "Make sure the package is properly installed."
    )
  }

  shiny::runApp(app_dir, display.mode = "normal")
}
