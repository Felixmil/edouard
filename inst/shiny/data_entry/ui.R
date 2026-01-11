library(shiny)
library(DT)
library(shinyWidgets)

ui <- fluidPage(
  titlePanel("Edouard Data Entry"),

  # Last entries panel
  wellPanel(
    h3("Dernières entrées"),
    DTOutput("last_entries_table")
  ),

  # Form panel
  wellPanel(
    h3("Nouvelle entrée"),
    fluidRow(
      column(
        3,
        dateInput(
          "input_date",
          "Date:",
          value = Sys.Date()
        )
      ),
      column(
        3,
        timeInput(
          "input_time",
          "Heure:",
          value = Sys.time()
        )
      ),
      column(
        3,
        selectInput(
          "input_variable",
          "Variable:",
          choices = c(
            "",
            "poids",
            "biberon",
            "temperature",
            "taille",
            "selle",
            "evenement_medical",
            "evenement_vie",
            "symptome"
          ),
          selected = ""
        )
      )
    ),
    conditionalPanel(
      condition = "input.input_variable != 'selle'",
      fluidRow(
        column(
          3,
          numericInput(
            "input_value",
            "Valeur:",
            value = NA,
            min = 0
          )
        ),
        column(
          3,
          selectInput(
            "input_unit",
            "Unité:",
            choices = c("", "kg", "ml", "c", "cm"),
            selected = ""
          )
        ),
        column(
          6,
          textAreaInput(
            "input_notes",
            "Notes:",
            value = "",
            rows = 2,
            placeholder = "Commentaires optionnels..."
          )
        )
      )
    ),
    conditionalPanel(
      condition = "input.input_variable == 'selle'",
      fluidRow(
        column(
          3,
          radioGroupButtons(
            "input_texture",
            "Texture:",
            choices = c(
              "Dense" = "1",
              "Normale" = "2",
              "Mousseuse" = "3",
              "Liquide" = "4"
            ),
            selected = "2",
            direction = "horizontal"
          )
        ),
        column(
          3,
          selectInput(
            "input_color",
            "Couleur:",
            choices = c(
              "",
              "marron",
              "jaune",
              "vert",
              "noir",
              "rouge",
              "blanc"
            ),
            selected = ""
          )
        ),
        column(
          6,
          textAreaInput(
            "input_notes_selle",
            "Notes:",
            value = "",
            rows = 2,
            placeholder = "Commentaires optionnels..."
          )
        )
      )
    ),
    fluidRow(
      column(
        12,
        actionButton(
          "btn_add_preview",
          "Ajouter au preview",
          icon = icon("plus"),
          class = "btn-primary"
        )
      )
    )
  ),

  # Preview panel
  wellPanel(
    h3("Preview des entrées"),
    DTOutput("preview_table"),
    br(),
    actionButton(
      "btn_save",
      "Valider et enregistrer",
      icon = icon("save"),
      class = "btn-success"
    )
  )
)
