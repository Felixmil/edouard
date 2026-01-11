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
    ),
    last_save_time = Sys.time()
  )

  # Reactive: Get last 5 entries from CSV
  last_entries <- reactive({
    # Trigger on save
    rv$last_save_time

    # Read from CSV directly
    csv_path <- here::here("data-raw", "edouard_data.csv")

    data <- readr::read_csv(
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

    # Get last 5 entries
    tail(data, 5)
  })

  # Output: Last entries table
  output$last_entries_table <- renderDT({
    data <- last_entries()

    datatable(
      data,
      options = list(
        dom = 't',
        ordering = FALSE,
        pageLength = 5
      ),
      rownames = FALSE
    )
  })

  # Observer: Auto-select unit based on variable
  observeEvent(input$input_variable, {
    unit <- switch(
      input$input_variable,
      "poids" = "kg",
      "biberon" = "ml",
      "temperature" = "c",
      "taille" = "cm",
      "selle" = "texture",
      "evenement_medical" = "",
      "evenement_vie" = "",
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

    # Handle selle-specific validation and data construction
    if (input$input_variable == "selle") {
      # Validate texture (always provided by slider) and color
      if (is.null(input$input_color) || input$input_color == "") {
        showNotification("⚠ La couleur est obligatoire pour selle", type = "warning")
        return()
      }

      # Convert timeInput to HH:MM format
      time_formatted <- substr(as.character(input$input_time), 1, 5)

      # Construct notes with color prefix
      notes_base <- paste0("Couleur: ", input$input_color)
      notes_full <- if (!is.null(input$input_notes_selle) && input$input_notes_selle != "") {
        paste0(notes_base, "; ", input$input_notes_selle)
      } else {
        notes_base
      }

      # Create new entry for selle
      new_entry <- data.frame(
        date = as.character(input$input_date),
        time = time_formatted,
        variable = "selle",
        value = as.numeric(input$input_texture),
        unit = "texture",
        notes = notes_full,
        stringsAsFactors = FALSE
      )
    } else {
      # Check if value is required for this variable type
      if (
        input$input_variable %in% c("poids", "biberon", "temperature", "taille")
      ) {
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

      # Create new entry for non-selle variables
      new_entry <- data.frame(
        date = as.character(input$input_date),
        time = time_formatted,
        variable = input$input_variable,
        value = if (is.null(input$input_value) || is.na(input$input_value)) {
          NA_real_
        } else {
          input$input_value
        },
        unit = if (is.null(input$input_unit) || input$input_unit == "") {
          NA_character_
        } else {
          input$input_unit
        },
        notes = if (is.null(input$input_notes) || input$input_notes == "") {
          NA_character_
        } else {
          input$input_notes
        },
        stringsAsFactors = FALSE
      )
    }

    # Add to preview data
    rv$preview_data <- rbind(rv$preview_data, new_entry)

    # Update last_entry
    rv$last_entry$date <- input$input_date
    rv$last_entry$time <- input$input_time
    rv$last_entry$variable <- input$input_variable

    # Partial form reset (keep date, time, variable; clear value and notes)
    if (input$input_variable == "selle") {
      updateTextAreaInput(session, "input_notes_selle", value = "")
    } else {
      updateNumericInput(session, "input_value", value = NA)
      updateTextAreaInput(session, "input_notes", value = "")
    }

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

  # Observer: Save to CSV
  observeEvent(input$btn_save, {
    # Check if there's data to save
    if (nrow(rv$preview_data) == 0) {
      showNotification("⚠ Aucune entrée à enregistrer", type = "warning")
      return()
    }

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
    readr::write_csv(updated_data, csv_path, na = "")

    # Clear preview data
    rv$preview_data <- rv$preview_data[0, ]

    # Trigger refresh of last entries
    rv$last_save_time <- Sys.time()

    # Success notification
    showNotification(
      "✓ Données enregistrées dans le CSV",
      type = "message",
      duration = 5
    )
  })
}
