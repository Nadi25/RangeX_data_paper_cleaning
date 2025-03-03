
# Climate data TOMST loggers NOR --------------------------------------------

## Data used: ,
##            
## Date:      03.03.2025
## Author:    Nadine Arzt
## Purpose:   Clean TOMST logger data 

# load library ------------------------------------------------------------
library(conflicted)
conflict_prefer_all("dplyr", quiet = TRUE)
library(tidyverse)
library(openxlsx)

library(readr)
library(purrr)
library(myClim)



# import data 2023 --------------------------------------------------------

# List all files in the 'Data_tomst_loggers' folder that start with 'data'
tomst_23 <- list.files(path = "Data/Data_tomst_loggers/tomst_2023/", pattern = "^data_\\d+.*\\.csv$", full.names = TRUE)

# Import each file and combine them into a single data frame
# name columns
column_names <- c("number", "Date", "Column1", "Temp1", "Temp2", "Temp3", "Soilmoisture", "Column6", "Column7")

# Define column types to avoid mismatches
column_types <- cols(
  number = col_double(),
  Date = col_character(),
  Column1 = col_double(),
  Temp1 = col_double(),
  Temp2 = col_double(),
  Temp3 = col_double(),
  Soilmoisture = col_double(),
  Column6 = col_double(),
  Column7 = col_double()
)

# Function to extract the number from the filename
extract_number <- function(file) {
  str_extract(basename(file), "\\d+")
}

# Read all files, specifying there is no header and setting the column names manually
# add a column for the file number

tomst_data_23 <- tomst_23 |> 
  map_dfr(~read_delim(.x, col_names = column_names, col_types = column_types, 
                      delim = ";", skip = 1) |> 
            mutate(tomst = extract_number(.x)))

head(tomst_data_23)

# get plot codes 23 ------------------------------------------------------

plot_codes_23 <- read.xlsx("Data/Data_tomst_loggers/tomst_plot_codes_2023.xlsx", sheet = 1, colNames = FALSE)

plot_codes_23

# split dataset into low and high 
# high
plot_high <- plot_codes_23 |> 
  select(X1, X2, X3, X4, X5)
# low
plot_low <- plot_codes_23 |> 
  select(X6, X7, X8, X9, X10)

plot_low <- plot_low |> 
  drop_na(X6)

# row 2 as header
colnames(plot_high) <- plot_high[2, ]
plot_high <- plot_high[c(-1,-2), ]

colnames(plot_low) <- plot_low[2, ]
plot_low <- plot_low[c(-1,-2), ]

# add column with site
plot_high <- plot_high |> 
  mutate(site = "high")

plot_low <- plot_low |> 
  mutate(site = "low")

# combine again under each other
plot_codes_clean <- bind_rows(plot_high, plot_low)































