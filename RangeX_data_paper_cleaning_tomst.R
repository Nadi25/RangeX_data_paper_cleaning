
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
library(janitor)
library(lubridate)

# comments ----------------------------------------------------------------
# 2023: high out: "08.06.2023" "20.06.2023" - "23.10.2023"
# 2023: low out: "12.05.2023" "19.06.2023" - "24.10.2023"
# so take the later one each?

# import data 2023 --------------------------------------------------------
# List all files in the 'Data_tomst_loggers' folder that start with 'data'
tomst_23 <- list.files(path = "Data/Data_tomst_loggers/tomst_2023/", pattern = "^data_\\d+.*\\.csv$", full.names = TRUE)

# test to see structure of files
test_file <- read_delim(tomst_23[1], delim = ";", skip = 1)
head(test_file)
# has , 

# define colnames as header
column_names <- c("number", "Date", "Column1", "Temp1", "Temp2", "Temp3", "Soilmoisture", "Column6", "Column7")

# define coltypes to have the values correct later
column_types <- cols(
  number = col_double(),
  Date = col_character(),   # Read as character first, convert to datetime later
  Column1 = col_double(),
  Temp1 = col_character(),  # Read as character to handle commas, convert later
  Temp2 = col_character(),
  Temp3 = col_character(),
  Soilmoisture = col_double(),
  Column6 = col_double(),
  Column7 = col_double()
)

# function to extract tomst logger number
extract_number <- function(file) {
  str_extract(basename(file), "\\d+")
}

# function to read a bunch of files at the same time
read_tomst_file <- function(file) {
  read_delim(file, delim = ";", skip = 1, col_names = column_names, col_types = column_types, 
             locale = locale(decimal_mark = ","), show_col_types = FALSE) |>
    mutate(Date = dmy_hms(Date),  # Convert Date column to datetime
      across(c(Temp1, Temp2, Temp3), ~ as.numeric(str_replace(.x, ",", "."))), # Convert temps to numeric
      tomst = extract_number(file)  # Add the tomst logger number
    )
}

# get one dataframe with data from all files using the list of files (tomst_23) with a loop
tomst_data_23 <- map_dfr(tomst_23, read_tomst_file)

head(tomst_data_23)


# get plot codes 23 ------------------------------------------------------

plot_codes_23 <- read.xlsx("Data/Data_tomst_loggers/tomst_plot_codes_2023.xlsx", sheet = 1, colNames = FALSE)

plot_codes_23 <- read.csv2("Data/Data_tomst_loggers/tomst_plot_codes_2023.csv", header =  FALSE)

plot_codes_23

# split dataset into low and high 
# high
plot_high <- plot_codes_23 |> 
  select(V1:V5)
# low
plot_low <- plot_codes_23 |> 
  select(V7:V11)

plot_low <- plot_low |> 
  filter(if_any(everything(), ~ !is.na(.) & . != ""))


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

# combine tomst data with plot labels -------------------------------------

tomst_23_raw <- left_join(plot_codes_clean, tomst_data_23, by = tomst)




# Add treat_warming and treat_competition columns based on treat ----------
tomst_23_raw <- tomst_23_raw |> 
  mutate(
    treat_warming = case_when(
      treat == "A" ~ "warm",
      treat == "B" ~ "ambi",
      treat == "C" ~ "warm",
      treat == "D" ~ "ambi",
      treat == "E" ~ "warm",
      treat == "F" ~ "ambi"
    ),
    treat_competition = case_when(
      treat == "A" ~ "vege",
      treat == "B" ~ "vege",
      treat == "C" ~ "control",
      treat == "D" ~ "control",
      treat == "E" ~ "bare",
      treat == "F" ~ "bare"
    )
  )


# delete empty columns -----------------------------------------------------
tomst_23_raw <- tomst_23_raw |> 
  select(where(~ !all(is.na(.))))


# get temperature data ----------------------------------------------------

# Extract temperature columns into a new data frame
temperature <- tomst_23_raw %>%
  select(tomst, date_out, Date, Temp1, Temp2, Temp3, block, treat, treat_warming, treat_competition, site) 

# View the first few rows of the extracted temperature data
head(temperature)
summary(temperature)

# fix date
temperature <- temperature |> 
  mutate(Date = as.POSIXct(Date, format = "%d.%m.%Y %H:%M:%S"))

# split low and high site -------------------------------------------------
temp_high <- temperature |> 
  filter(site == "high") 

temp_low <- temperature |> 
  filter(site == "low") 

# define time period field season -----------------------------------------
# high
start_date <- as.POSIXct("2023-06-21 10:00:00", format = "%Y-%m-%d %H:%M:%S")
end_date <- as.POSIXct("2023-10-23 10:00:00", format = "%Y-%m-%d %H:%M:%S")

# Filter the data for the specified date range
filtered_temp_high <- temp_high |> 
  filter(Date >= start_date & Date <= end_date)

head(filtered_temp_high)


# Create the plot for Temp1 per logger with the filtered data
ggplot(high_filtered_temperature, aes(x = Date, y = Temp1, color = tomst)) +
  geom_line() +
  labs(title = "Temperature (Temp1) per Logger from 31.05.2024 to 15.10.2024",
       x = "Date",
       y = "Temperature (\u00B0C)",
       color = "Logger ID") +
  theme_minimal() +
  theme(legend.position = "right")



















