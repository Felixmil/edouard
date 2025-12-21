library(shiny)
library(DT)
library(readr)
library(here)
library(dplyr)

# Validation helper
validate_time_format <- function(time_string) {
  grepl("^\\d{2}:\\d{2}$", time_string)
}

server <- function(input, output, session) {

  # Reactive values to store state
  rv <- reactiveValues(
    preview_data = data.frame(
      date = character(),
      time = character(),
      variable = character(),
      value = numeric(),
      unit = character(),
      notes = character(),
      stringsAsFactors = FALSE
    ),
    last_entry = list(
      date = Sys.Date(),
      time = format(Sys.time(), "%H:%M"),
      variable = ""
    )
  )

  # Observer: Auto-select unit based on variable
  observeEvent(input$input_variable, {
    unit <- switch(input$input_variable,
      "poids" = "kg",
      "biberon" = "ml",
      "temperature" = "c",
      "taille" = "cm",
      "evenement" = "",
      "symptome" = "",
      ""
    )
    updateSelectInput(session, "input_unit", selected = unit)
  })

  # Observer: Add entry to preview
  observeEvent(input$btn_add_preview, {
    # Validate required fields
    if (is.null(input$input_date)) {
      showNotification("⚠ La date est obligatoire", type = "warning")
      return()
    }

    if (is.null(input$input_time)) {
      showNotification("⚠ L'heure est obligatoire", type = "warning")
      return()
    }

    if (is.null(input$input_variable) || input$input_variable == "") {
      showNotification("⚠ La variable est obligatoire", type = "warning")
      return()
    }

    # Check if value is required for this variable type
    if (input$input_variable %in% c("poids", "biberon", "temperature", "taille")) {
      if (is.null(input$input_value) || is.na(input$input_value)) {
        showNotification(
          paste0("⚠ Une valeur est obligatoire pour ", input$input_variable),
          type = "warning"
        )
        return()
      }
    }

    # Convert timeInput to HH:MM format (timeInput returns character "HH:MM:SS")
    time_formatted <- substr(as.character(input$input_time), 1, 5)

    # Create new entry
    new_entry <- data.frame(
      date = as.character(input$input_date),
      time = time_formatted,
      variable = input$input_variable,
      value = if (is.null(input$input_value) || is.na(input$input_value)) NA_real_ else input$input_value,
      unit = if (is.null(input$input_unit) || input$input_unit == "") NA_character_ else input$input_unit,
      notes = if (is.null(input$input_notes) || input$input_notes == "") NA_character_ else input$input_notes,
      stringsAsFactors = FALSE
    )

    # Add to preview data
    rv$preview_data <- rbind(rv$preview_data, new_entry)

    # Update last_entry
    rv$last_entry$date <- input$input_date
    rv$last_entry$time <- input$input_time
    rv$last_entry$variable <- input$input_variable

    # Partial form reset (keep date, time, variable; clear value and notes)
    updateNumericInput(session, "input_value", value = NA)
    updateTextAreaInput(session, "input_notes", value = "")

    # Success notification
    showNotification("✓ Entrée ajoutée au preview", type = "message")
  })

  # Output: Preview table
  output$preview_table <- renderDT({
    if (nrow(rv$preview_data) == 0) {
      return(
        datatable(
          data.frame(Message = "Aucune entrée en attente"),
          options = list(dom = 't', ordering = FALSE),
          rownames = FALSE
        )
      )
    }

    # Add row numbers for deletion
    display_data <- rv$preview_data
    display_data$Action <- seq_len(nrow(display_data))

    datatable(
      display_data,
      options = list(
        dom = 't',
        ordering = FALSE,
        columnDefs = list(
          list(
            targets = ncol(display_data) - 1,
            render = JS(
              "function(data, type, row, meta) {",
              "  return '<button class=\"btn btn-danger btn-sm delete-btn\" data-row=\"' + data + '\">Supprimer</button>';",
              "}"
            )
          )
        )
      ),
      rownames = FALSE,
      escape = FALSE
    )
  })

  # Observer: Delete from preview
  observeEvent(input$preview_table_cell_clicked, {
    info <- input$preview_table_cell_clicked

    # Check if Action column was clicked
    if (!is.null(info$col) && info$col == ncol(rv$preview_data)) {
      # Remove the row
      rv$preview_data <- rv$preview_data[-info$row, , drop = FALSE]

      showNotification("✓ Entrée supprimée du preview", type = "message")
    }
  })

  # Observer: Save to CSV and rebuild package
  observeEvent(input$btn_save, {
    # Check if there's data to save
    if (nrow(rv$preview_data) == 0) {
      showNotification("⚠ Aucune entrée à enregistrer", type = "warning")
      return()
    }

    tryCatch({
      # Construct path to CSV
      csv_path <- here::here("data-raw", "edouard_data.csv")

      # Read existing data
      existing_data <- readr::read_csv(
        csv_path,
        col_types = cols(
          date = col_character(),
          time = col_character(),
          variable = col_character(),
          value = col_double(),
          unit = col_character(),
          notes = col_character()
        ),
        show_col_types = FALSE
      )

      # Append new data
      updated_data <- dplyr::bind_rows(existing_data, rv$preview_data)

      # Write back to CSV
      readr::write_csv(updated_data, csv_path)

      # Rebuild package data
      rebuild_script <- here::here("data-raw", "edouard_data.R")
      source(rebuild_script)

      # Clear preview data
      rv$preview_data <- rv$preview_data[0, ]

      # Success notification
      showNotification(
        "✓ Données enregistrées et package reconstruit",
        type = "message",
        duration = 5
      )

    }, error = function(e) {
      showNotification(
        paste0("✗ Erreur lors de l'enregistrement : ", e$message),
        type = "error",
        duration = NULL
      )
    })
  })
}
