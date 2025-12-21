## Code to prepare `edouard_data` dataset

library(readr)
library(dplyr)
library(lubridate)
library(here)

# Load the processing function
source(here("data-raw", "process_data.R"))

# Read the CSV data
edouard_data_raw <- read_csv(
  here("data-raw", "edouard_data.csv"),
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
edouard_data <- process_edouard_data(edouard_data_raw)

# Save as package data
usethis::use_data(edouard_data, overwrite = TRUE)
