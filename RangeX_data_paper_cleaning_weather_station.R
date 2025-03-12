
# Climate data weather station NOR 2023 --------------------------------------------

## Data used: Data/Data_climate_station/2023/RangeX_HIGH_fall_2023.txt
##            
## Date:      12.03.2025
## Author:    Nadine Arzt
## Purpose:   Clean weather station data 2023


# load library ------------------------------------------------------------
library(conflicted)
conflict_prefer_all("dplyr", quiet = TRUE)
library(tidyverse)
library(lubridate)

theme_set(theme_bw())

# import data hi 23 ----------------------------------------------------------
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
  mutate(across(-date_time, ~as.numeric(str_replace_all(., ",", "."))))


# add column site and year ------------------------------------------------
climate_hi_23 <- climate_hi_23 |>
  mutate(site = "hi") |> 
  mutate(year = 2023)



# import data lo 23 ----------------------------------------------------------
climate_lo_23 <- read_table("Data/Data_climate_station/2023/RangeX_LOW_fall_2023.txt", col_names = FALSE)

# Extract labels and units
# units <- climate_hi_23[2, -1]
# labels <- climate_hi_23[1, -1]
# Remove the first two rows from the data
climate_lo_23 <- climate_lo_23[-c(1, 2), ]  


# combine first two colums date and time 
climate_lo_23 <- climate_lo_23 |> 
  mutate(date_time = paste(X1, X2)) |> 
  select(date_time, X3:X8)

# # have colnames without units
# column_names <- c("date_time", "AirTemp_Avg", "Humidity_Avg", "WindDir_Avg", "WindSpd_Avg", "Radiation_Avg", "Rainfall")

colnames(climate_lo_23) <- column_names
climate_lo_23

str(climate_lo_23)

# make date_time a date format
climate_lo_23 <- climate_lo_23 |> 
  mutate(date_time = dmy_hms(date_time))

# filter time as for tomst loggers ----------------------------------------
start_date <- as.Date("2023-06-21") 
end_date <- as.Date("2023-10-23") # were collected on 24.10

# Filter the data for the specified date range
climate_lo_23 <- climate_lo_23 |> 
  filter(between(date_time, left = start_date, right = end_date))

# Replace commas with . and convert to numeric
climate_lo_23 <- climate_lo_23 |> 
  mutate(across(-date_time, ~as.numeric(str_replace_all(., ",", "."))))


# add column site and year ------------------------------------------------
climate_lo_23 <- climate_lo_23 |>
  mutate(site = "lo") |> 
  mutate(year = 2023)


# combine 23 hi and lo ----------------------------------------------------
climate_23 <- bind_rows(climate_hi_23, climate_lo_23)


# import data hi 21 ----------------------------------------------------------
climate_hi_21 <- read_table("Data/Data_climate_station/2021/weather_high_2021_fall.txt", col_names = FALSE)

# Remove the first two rows from the data
climate_hi_21 <- climate_hi_21[-c(1, 2), ]  

# combine first two colums date and time 
climate_hi_21 <- climate_hi_21 |> 
  mutate(date_time = paste(X1, X2)) |> 
  select(date_time, X3:X8)

colnames(climate_hi_21) <- column_names
climate_hi_21

str(climate_hi_21)

# make date_time a date format
climate_hi_21 <- climate_hi_21 |> 
  mutate(date_time = dmy_hms(date_time))
# 
# # filter time as for tomst loggers ----------------------------------------
# # check and correct
# start_date_21 <- as.Date("2021-06-21") 
# end_date_21 <- as.Date("2021-10-23") # were collected on 24.10
# 
# # Filter the data for the specified date range
# climate_hi_21 <- climate_hi_21 |> 
#   filter(between(date_time, left = start_date, right = end_date))

# Replace commas with . and convert to numeric
climate_hi_21 <- climate_hi_21 |> 
  mutate(across(-date_time, ~as.numeric(str_replace_all(., ",", "."))))


# add column site and year ------------------------------------------------
climate_hi_21 <- climate_hi_21 |>
  mutate(site = "hi") |> 
  mutate(year = 2021)


# import data lo 21 ----------------------------------------------------------
climate_lo_21 <- read_table("Data/Data_climate_station/2021/weather_low_2021_fall.txt", col_names = FALSE)

# Remove the first two rows from the data
climate_lo_21 <- climate_lo_21[-c(1, 2), ]  

# combine first two colums date and time 
climate_lo_21 <- climate_lo_21 |> 
  mutate(date_time = paste(X1, X2)) |> 
  select(date_time, X3:X8)

colnames(climate_lo_21) <- column_names
climate_lo_21

str(climate_lo_21)

# make date_time a date format
climate_lo_21 <- climate_lo_21 |> 
  mutate(date_time = dmy_hms(date_time))

# # filter time as for tomst loggers ----------------------------------------
# start_date <- as.Date("2023-06-21") 
# end_date <- as.Date("2023-10-23") # were collected on 24.10
# 
# # Filter the data for the specified date range
# climate_lo_21 <- climate_lo_21 |> 
#   filter(between(date_time, left = start_date, right = end_date))

# Replace commas with . and convert to numeric
climate_lo_21 <- climate_lo_21 |> 
  mutate(across(-date_time, ~as.numeric(str_replace_all(., ",", "."))))


# add column site and year ------------------------------------------------
climate_lo_21 <- climate_lo_21 |>
  mutate(site = "lo") |> 
  mutate(year = 2021)



# combine 21 and 23 -------------------------------------------------------

climate <- bind_rows(climate_23, climate_hi_21, climate_lo_21)

climate_plot <- climate |> 
  mutate(date_time = as.Date(date_time)) |> 
  group_by(date_time, site, year) |> 
  summarize(AirTemp = mean(AirTemp_Avg, na.rm = TRUE),
            Humidity = mean(Humidity_Avg, na.rm = TRUE),
            WindDir = mean(WindDir_Avg, na.rm = TRUE),
            WindSpd = mean(WindSpd_Avg, na.rm = TRUE),
            Radiation = mean(Radiation_Avg, na.rm = TRUE),.groups = 'drop')
climate_plot
#
# plot average temp per site and year
ggplot(climate_plot, aes(x = date_time, y = AirTemp, colour = site)) +
  geom_line()+
  labs(title = "Daily temperature 2m above ground", y = "Temperature (°C)", x = "Date")+
  scale_color_manual(values = c("lo" = "pink3", "hi" = "turquoise"))




# control plotting --------------------------------------------------------
climate_23_plot <- climate_23 |> 
  mutate(date_time = as.Date(date_time)) |> 
  group_by(date_time, site) |> 
  summarize(AirTemp = mean(AirTemp_Avg, na.rm = TRUE),
            Humidity = mean(Humidity_Avg, na.rm = TRUE),
            WindDir = mean(WindDir_Avg, na.rm = TRUE),
            WindSpd = mean(WindSpd_Avg, na.rm = TRUE),
            Radiation = mean(Radiation_Avg, na.rm = TRUE),.groups = 'drop')
climate_23_plot

# plot average temp per treat
ggplot(climate_23_plot, aes(x = date_time, y = AirTemp, colour = site)) +
  geom_line()+
  labs(title = "Daily temperature 2m above ground", y = "Temperature (°C)", x = "Date")+
  scale_color_manual(values = c("lo" = "pink3", "hi" = "turquoise"))

# plot average Humidity per treat
ggplot(climate_23_plot, aes(x = date_time, y = Humidity, colour = site)) +
  geom_line()+
  labs(title = "Daily average humidity", y = "Humidity (%)", x = "Date")+
  scale_color_manual(values = c("lo" = "pink3", "hi" = "turquoise"))

# plot average Radiation per treat
ggplot(climate_23_plot, aes(x = date_time, y = Radiation, colour = site)) +
  geom_line()+
  labs(title = "Daily average radiation", y = "Radiation (deg)", x = "Date")+
  scale_color_manual(values = c("lo" = "pink3", "hi" = "turquoise"))


# rainfall ----------------------------------------------------------------
# Aggregate rainfall data by day
daily_rainfall <- climate_23 |> 
  group_by(date_time, site) |> 
  summarize(total_rainfall = sum(Rainfall, na.rm = TRUE), .groups = 'drop')

# plot average Rainfall per treat
ggplot(daily_rainfall, aes(x = date_time, y = total_rainfall, fill = site)) +
  geom_col() +
  labs(title = "Daily Rainfall", x = "Date", y = "Sum rainfall per day (mm)")+
  scale_color_manual(values = c("lo" = "pink3", "hi" = "turquoise"))


