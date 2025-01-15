

# RangeX community cover data cleaning  -------------------------------

## Data used: RangeX_raw_comcov_high_2021.xlsx, 
##            RangeX_raw_comcov_low_2021.xlsx,
##            RangeX_raw_comcov_high_2023.xlsx, 
##            RangeX_raw_comcov_low_2023.xlsx
## Date:      15.01.2025
## Author:    Nadine Arzt
## Purpose:   Cleaning of the complete raw data files of community 2021-2023


# load library ------------------------------------------------------------
library(conflicted)
conflict_prefer_all("dplyr", quiet = TRUE)
library(tidyverse)
library(purrr)
library(readxl)


# import community metadata -----------------------------------------------
files <- dir(path = "Data/Data_community/", pattern = "\\.xlsx$", full.names = TRUE, recursive = TRUE)

#Function to read in meta data
meta_com_raw <- map_df(set_names(files), function (file) {
  print(file)
  sheets <- excel_sheets(file)[-1]
  map_df(sheets,  ~ {
    read_xlsx(
      path = file, 
      sheet = .x, 
      range = cell_limits(c(1, 20), c(15, 21)), # Start at row 1, column 20 and end at row 15, column 21
      col_names = FALSE,       # Column names are not present
      # col_types = "text" # Flexible to handle varying column numbers
    ) |> 
      setNames(c("metadata_name", "metadata_value")) |> 
      mutate(sheet_name = .x)
  })
}, .id = "file") |> 
  # date needs to be date format and sometimes it's a range 
  mutate(
    metadata_value = case_when(
      metadata_name == "date" & !is.na(as.numeric(metadata_value)) ~ 
        as.character(as.Date(as.numeric(metadata_value), origin = "1899-12-30")), # Numeric to Date
      metadata_name == "date" ~ metadata_value, # Leave text-based ranges as is
      TRUE ~ metadata_value
    )
  )




























