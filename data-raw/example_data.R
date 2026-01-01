## Code to prepare `edouard_data_dev` dataset (example/development data)

library(readr)
library(dplyr)
library(lubridate)
library(here)

# Load package to access processing function
devtools::load_all()

# Read the example CSV data
example_data <- read_csv(
  here("data-raw", "example_data.csv"),
  col_types = cols(
    date = col_date(format = "%Y-%m-%d"),
    time = col_time(format = "%H:%M"),
    variable = col_character(),
    value = col_double(),
    unit = col_character(),
    notes = col_character()
  )
)

# Process the data with human-readable enhancements
example_data <- process_edouard_data(example_data)

# Save as package data
usethis::use_data(example_data, overwrite = TRUE)
