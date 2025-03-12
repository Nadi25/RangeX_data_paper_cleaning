
# Climate data weather station NOR 2023 --------------------------------------------

## Data used: Data/Data_climate_station/2021/
##            RangeX_metadata_plot_NOR.csv
## Date:      08.03.2025
## Author:    Nadine Arzt
## Purpose:   Clean weather station data 2023


# load library ------------------------------------------------------------
library(conflicted)
conflict_prefer_all("dplyr", quiet = TRUE)
library(tidyverse)
library(lubridate)

# import data -------------------------------------------------------------
climate_hi_23 <- read_table("Data/Data_climate_station/2023/RangeX_HIGH_fall_2023.txt", col_names = FALSE)

# Extract labels and units
units <- climate_hi_23[2, -1]
labels <- climate_hi_23[1, -1]
# Remove the first two rows from the data
climate_hi_23 <- climate_hi_23[-c(1, 2), ]  


# combine first two colums date and time 
climate_hi_23 <- climate_hi_23 |> 
  mutate(date_time = paste(X1, X2)) |> 
  select(date_time, X3:X8)

# have colnames without units
column_names <- c("date_time", "AirTemp_Avg", "Humidity_Avg", "WindDir_Avg", "WindSpd_Avg", "Radiation_Avg", "Rainfall")

colnames(climate_hi_23) <- column_names
climate_hi_23

str(climate_hi_23)

# make date_time a date format
climate_hi_23 <- climate_hi_23 |> 
  mutate(date_time = dmy_hms(date_time))

# filter time as for tomst loggers ----------------------------------------
start_date <- as.Date("2023-06-21") 
end_date <- as.Date("2023-10-23") # were collected on 24.10

# Filter the data for the specified date range
climate_hi_23 <- climate_hi_23 |> 
  filter(between(date_time, left = start_date, right = end_date))

# Replace commas with . and convert to numeric
climate_hi_23 <- climate_hi_23 |> 
  mutate(across(-date_time, ~as.numeric(str_replace_all(., ",", ".")))  
  )


