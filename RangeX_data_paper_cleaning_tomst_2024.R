# Climate data TOMST loggers NOR 2024 --------------------------------------------

## Data used: Data/Data_tomst_loggers/tomst_2024/,
##            tomst_plot_codes_2024.csv,
##            RangeX_metadata_plot_NOR.csv
## Date:      14.03.2025
## Author:    Nadine Arzt
## Purpose:   Clean TOMST logger data 2024

# load library ------------------------------------------------------------
library(tidyverse)
library(conflicted)
conflict_prefer_all("dplyr", quiet = TRUE)
library(janitor)

# default theme
theme_set(theme_bw())

# comments --------------------------------------------------------------
# https://tomst.com/web/en/systems/tms/software/
# temp1 is in soil, temp2 at 0cm and temp3 20cm above ground

# 2023: high out: "08.06.2023" and "20.06.2023" - "23.10.2023"
# 2023: low out: "12.05.2023" and "19.06.2023" - "24.10.2023"

# source script with functions --------------------------------------------
source("RangeX_data_paper_functions.R")

# import data 2024 --------------------------------------------------------
# List all files in the 'Data_tomst_loggers' folder that start with 'data'
tomst_24 <- list.files(path = "Data/Data_tomst_loggers/tomst_2024/", pattern = "^data_\\d+.*\\.csv$", full.names = TRUE)

# test to see structure of files
test_file <- read_delim(tomst_24[1], delim = ";", skip = 1)
head(test_file)
# has , 

# define colnames as header
column_names <- c("number", "date_time", "Column1", "TMS_T1", "TMS_T2", "TMS_T3", "Soilmoisture_raw", "Column6", "Column7")

# define coltypes to have the values correct later
column_types <- cols(
  number = col_double(),
  date_time = col_character(),   # Read as character first, convert to datetime later
  Column1 = col_double(),
  TMS_T1 = col_character(),  # Read as character to handle commas, convert later
  TMS_T2 = col_character(),
  TMS_T3 = col_character(),
  Soilmoisture_raw = col_double(),
  Column6 = col_double(),
  Column7 = col_double()
)

# get one dataframe with data from all files using the list of files (tomst_24) with a loop
# use same function as for 21 data with several date formats
tomst_data_24 <- map(tomst_24, read_tomst_file_21) |> 
  list_rbind()
head(tomst_data_24)

# get plot codes 24 ------------------------------------------------------
plot_codes_24 <- read.csv2("Data/Data_tomst_loggers/tomst_plot_codes_2024.csv", skip = 1)
plot_codes_24

# split dataset into low and high 
# high
plot_high_24 <- plot_codes_24 |> 
  select(block:comment)
# low
plot_low_24 <- plot_codes_24 |> 
  select(block.1:comment.1)

plot_low_24 <- plot_low_24 |> 
  filter(if_any(everything(), ~ !is.na(.) & . != "")) |> 
  rename(block = block.1,
         treat = treat.1,
         tomst = tomst.1,
         date_out = date_out.1,
         date_home = date_home.1, # column wasn't in 23
         comment = comment.1)

# combine again under each other
plot_codes_clean_24 <- bind_rows(hi = plot_high_, lo = plot_low_24, .id = "site")
head(plot_codes_clean_24)
str(plot_codes_clean_24) # tomst = int 

# tomst into character
plot_codes_clean_24 <- plot_codes_clean_24 |> 
  mutate(tomst = as.character(tomst))

# combine tomst data with plot labels -------------------------------------
tomst_24_raw <- left_join(plot_codes_clean_24, tomst_data_24, by = "tomst")
head(tomst_24_raw)

