
# Climate data weather station NOR 2021, 2022, 2023, 2024, 2025 --------------------------------------------

## Data used: RangeX_HIGH_fall_2023.txt
##            RangeX_LOW_fall_2023.txt,
##            RangeX_HIGH_Spring2023.txt,
##            RangeX_LOW_Spring2023.txt
##            RangeX_HIGH_2022_fall.txt,
##            RangeX_LOW_2022_fall.txt,
##            weather_high_mai_2022.txt,
##            weather_high_2021_fall.txt
##            weather_low_2021_fall.txt,
##            RangeX_HIGH_15.10.24.txt,
##            RangeX_LOW_15.10.24.txt,
##            RangeX_HIGH_21.05.24.txt,
##            RangeX_LOW_21.05.24.txt,
##            RangeX_HIGH_08.05.25.txt,
##            RangeX_LOW_16.04.25.txt
## Date:      12.03.2025
## Author:    Nadine Arzt
## Purpose:   Clean weather station data 2021, 2022, 2023, 2024 and spring 2025


# load library ------------------------------------------------------------
library(conflicted)
conflict_prefer_all("dplyr", quiet = TRUE)
library(tidyverse)
library(gridExtra)
library(quantreg)
library(splines)
library(broom)

theme_set(theme_bw())


# comments ----------------------------------------------------------------
# change to 23 first and fix Ra ditation problem in 21

# should I use all days that have been recorded or shorten it to same as in tomst logger time frame? --> Kept everything for now

# 2021 --------------------------------------------------------------------
# import data hi 21 ---------------------------------------------------------
climate_hi_21 <- read_table("Data/Data_climate_station/2021/weather_high_2021_fall.txt", col_names = FALSE)

# Extract labels and units ------------------------------------------------
# use for all years
labels <- climate_hi_21[1, -1] # take first row and delete first argument
labels
# AirTemp.Avg, Humidity.Avg, WindDir.Avg, WindSpd.Avg, Ra  diation.Avg, Rainfall

units <- climate_hi_21[2, -1]
units
# deg C, %, deg, m.s-1, W.m-2 (watts per square meter), mm

# Remove the first two rows from the data
climate_hi_21 <- climate_hi_21[-c(1, 2), ]  

# combine first two colums date and time 
climate_hi_21 <- climate_hi_21 |> 
  mutate(date_time = paste(X1, X2)) |> 
  select(date_time, X3:X8)

column_names <- c("date_time", "AirTemp_Avg", "Humidity_Avg", "WindDir_Avg", "WindSpd_Avg", "Radiation_Avg", "Rainfall")

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


# combine 21 low and high -------------------------------------------------------
climate_21 <- bind_rows(climate_hi_21, climate_lo_21)

climate_21 <- climate_21 |> 
  mutate(region = "NOR") |> 
  select(region, site, year, date_time, AirTemp_Avg, Humidity_Avg, 
         WindDir_Avg, WindSpd_Avg, Radiation_Avg, Rainfall)


# save clean data 21 ------------------------------------------------------
# write.csv(climate_21, "Data/Data_climate_station/2021/RangeX_clean_climate_station_NOR_2021.csv")


# control plotting 21 -----------------------------------------------------
climate_21_plot <- climate_21 |> 
  mutate(date_time = as.Date(date_time)) |> 
  group_by(date_time, site, year) |> 
  summarize(AirTemp = mean(AirTemp_Avg, na.rm = TRUE),
            Humidity = mean(Humidity_Avg, na.rm = TRUE),
            WindDir = mean(WindDir_Avg, na.rm = TRUE),
            WindSpd = mean(WindSpd_Avg, na.rm = TRUE),
            Radiation = mean(Radiation_Avg, na.rm = TRUE),.groups = 'drop')
climate_21_plot

# plot average temp per site and year
temp_21 <- ggplot(climate_21_plot, aes(x = date_time, y = AirTemp, colour = site)) +
  geom_line()+
  labs(title = "Daily temperature 2m above ground 2021", y = "Temperature (°C)", x = "Date")+
  scale_color_manual(values = c("lo" = "orange", "hi" = "turquoise"))
temp_21

ggsave(filename = "RangeX_climate_station_temp_21.png", 
       plot = temp_21, 
       path = "Data/Data_climate_station/2021/Graphs", 
       width = 10, height = 6)

# plot average Humidity per treat
humidity_21 <- ggplot(climate_21_plot, aes(x = date_time, y = Humidity, colour = site)) +
  geom_line()+
  labs(title = "Daily average humidity 21", y = "Humidity (%)", x = "Date")+
  scale_color_manual(values = c("lo" = "orange", "hi" = "turquoise"))+
  scale_y_continuous(limits = c(25, 100))
humidity_21

ggsave(filename = "RangeX_climate_station_humidity_21.png", 
       plot = humidity_21, 
       path = "Data/Data_climate_station/2021/Graphs", 
       width = 10, height = 6)

# plot average Radiation per treat
radiation_21 <- ggplot(climate_21_plot, aes(x = date_time, y = Radiation, colour = site)) +
  geom_line()+
  labs(title = "Daily average radiation 21", y = "Radiation (W.m-2)", x = "Date")+
  scale_color_manual(values = c("lo" = "orange", "hi" = "turquoise"))
radiation_21

ggsave(filename = "RangeX_climate_station_radiation_21.png", 
       plot = radiation_21, 
       path = "Data/Data_climate_station/2021/Graphs", 
       width = 10, height = 6)

# rainfall 21----------------------------------------------------------------
# Aggregate rainfall data by day
daily_rainfall_21 <- climate_21 |> 
  group_by(date_time, site) |> 
  summarize(total_rainfall = sum(Rainfall, na.rm = TRUE), .groups = 'drop')

# plot average Rainfall per treat
ggplot(daily_rainfall_21) +
  geom_col(aes(x = date_time, y = total_rainfall, fill = site)) +
  labs(title = "Daily Rainfall", x = "Date", y = "Sum rainfall per day (mm)")+
  scale_fill_manual(values = c("lo" = "orange", "hi" = "turquoise"))+
  scale_y_continuous(limits = c(0, 5.0))


# yearly average 21 -------------------------------------------------------
climate_21_yearly <- climate_21 |> 
  mutate(date_time = as.Date(date_time)) |> 
  group_by(site, year) |> 
  summarize(AirTemp = mean(AirTemp_Avg, na.rm = TRUE),
            Humidity = mean(Humidity_Avg, na.rm = TRUE),
            WindDir = mean(WindDir_Avg, na.rm = TRUE),
            WindSpd = mean(WindSpd_Avg, na.rm = TRUE),
            Radiation = mean(Radiation_Avg, na.rm = TRUE),.groups = 'drop')
climate_21_yearly


# 2022 --------------------------------------------------------------------
# import data hi 22 ----------------------------------------------------------
climate_hi_22 <- read_table("Data/Data_climate_station/2022/RangeX_HIGH_2022_fall.txt", col_names = FALSE)

# Remove the first two rows from the data
climate_hi_22 <- climate_hi_22[-c(1, 2), ]  

# combine first two colums date and time 
climate_hi_22 <- climate_hi_22 |> 
  mutate(date_time = paste(X1, X2)) |> 
  select(date_time, X3:X8)

# have colnames without units
column_names <- c("date_time", "AirTemp_Avg", "Humidity_Avg", "WindDir_Avg", "WindSpd_Avg", "Radiation_Avg", "Rainfall")

colnames(climate_hi_22) <- column_names
climate_hi_22

str(climate_hi_22)

# make date_time a date format
climate_hi_22 <- climate_hi_22 |> 
  mutate(date_time = dmy_hms(date_time))

# Replace commas with . and convert to numeric
climate_hi_22 <- climate_hi_22 |> 
  mutate(across(-date_time, ~as.numeric(str_replace_all(., ",", "."))))

# add column site and year ------------------------------------------------
climate_hi_22 <- climate_hi_22 |>
  mutate(site = "hi") |> 
  mutate(year = 2022)

# import data lo 22 ----------------------------------------------------------
climate_lo_22 <- read_table("Data/Data_climate_station/2022/RangeX_LOW_2022_fall.txt", col_names = FALSE)

# Remove the first two rows from the data
climate_lo_22 <- climate_lo_22[-c(1, 2), ]  

# combine first two colums date and time 
climate_lo_22 <- climate_lo_22 |> 
  mutate(date_time = paste(X1, X2)) |> 
  select(date_time, X3:X8)

# # have colnames without units
# column_names <- c("date_time", "AirTemp_Avg", "Humidity_Avg", "WindDir_Avg", "WindSpd_Avg", "Radiation_Avg", "Rainfall")

colnames(climate_lo_22) <- column_names
climate_lo_22

str(climate_lo_22)

# make date_time a date format
climate_lo_22 <- climate_lo_22 |> 
  mutate(date_time = dmy_hms(date_time))

# Replace commas with . and convert to numeric
climate_lo_22 <- climate_lo_22 |> 
  mutate(across(-date_time, ~as.numeric(str_replace_all(., ",", "."))))

# add column site and year ------------------------------------------------
climate_lo_22 <- climate_lo_22 |>
  mutate(site = "lo") |> 
  mutate(year = 2022)


# filter lo 22 season -----------------------------------------------------
# to match hi site
# we dont need 2021 here
start_date <- as.Date("2022-05-30") 
end_date <- as.Date("2022-10-28") 
# 
# Filter the data for the specified date range
climate_lo_22 <- climate_lo_22 |> 
  filter(between(date_time, left = start_date, right = end_date))

# combine 22 hi and lo ----------------------------------------------------
climate_22 <- bind_rows(climate_hi_22, climate_lo_22)

climate_22 <- climate_22 |> 
  mutate(region = "NOR") |> 
  select(region, site, year, date_time, AirTemp_Avg, Humidity_Avg, 
         WindDir_Avg, WindSpd_Avg, Radiation_Avg, Rainfall)

# save clean data 22 ------------------------------------------------------
# write.csv(climate_22, "Data/Data_climate_station/2022/RangeX_clean_climate_station_NOR_2022.csv")

# control plotting --------------------------------------------------------
climate_22_plot <- climate_22 |> 
  mutate(date_time = as.Date(date_time)) |> 
  group_by(date_time, site) |> 
  summarize(AirTemp = mean(AirTemp_Avg, na.rm = TRUE),
            Humidity = mean(Humidity_Avg, na.rm = TRUE),
            WindDir = mean(WindDir_Avg, na.rm = TRUE),
            WindSpd = mean(WindSpd_Avg, na.rm = TRUE),
            Radiation = mean(Radiation_Avg, na.rm = TRUE),.groups = 'drop')
climate_22_plot

# plot average temp per treat
temp_22 <- ggplot(climate_22_plot, aes(x = date_time, y = AirTemp, colour = site)) +
  geom_line()+
  labs(title = "Daily temperature 2m above ground 2022", y = "Temperature (°C)", x = "Date")+
  scale_color_manual(values = c("lo" = "orange", "hi" = "turquoise"))
temp_22

ggsave(filename = "RangeX_climate_station_temp_22.png", 
       plot = temp_22, 
       path = "Data/Data_climate_station/2022/Graphs", 
       width = 10, height = 6)

# plot average Humidity per treat
humidity_22 <- ggplot(climate_22_plot, aes(x = date_time, y = Humidity, colour = site)) +
  geom_line()+
  labs(title = "Daily average humidity 22", y = "Humidity (%)", x = "Date")+
  scale_color_manual(values = c("lo" = "orange", "hi" = "turquoise"))+
  scale_y_continuous(limits = c(25, 100))
humidity_22

ggsave(filename = "RangeX_climate_station_humidity_22.png", 
       plot = humidity_22, 
       path = "Data/Data_climate_station/2022/Graphs", 
       width = 10, height = 6)

# plot average Radiation per treat
radiation_22 <- ggplot(climate_22_plot, aes(x = date_time, y = Radiation, colour = site)) +
  geom_line()+
  labs(title = "Daily average radiation 22", y = "Radiation (W.m-2)", x = "Date")+
  scale_color_manual(values = c("lo" = "orange", "hi" = "turquoise"))
radiation_22

ggsave(filename = "RangeX_climate_station_radiation_22.png", 
       plot = radiation_22, 
       path = "Data/Data_climate_station/2022/Graphs", 
       width = 10, height = 6)

# rainfall 22 ----------------------------------------------------------------
# Aggregate rainfall data by day
daily_rainfall_22 <- climate_22 |> 
  group_by(date_time, site) |> 
  summarize(total_rainfall = sum(Rainfall, na.rm = TRUE), .groups = 'drop')

# plot average Rainfall per treat
ggplot(daily_rainfall_22) +
  geom_col(aes(x = date_time, y = total_rainfall, fill = site)) +
  labs(title = "Daily Rainfall 22", x = "Date", y = "Sum rainfall per day (mm)")+
  scale_fill_manual(values = c("lo" = "orange", "hi" = "turquoise"))+
  scale_y_continuous(limits = c(0, 5.0))


# yearly average 22 summer --------------------------------------------------
climate_22_summer_yearly <- climate_22 |> 
  mutate(date_time = as.Date(date_time)) |> 
  group_by(site, year) |> 
  summarize(AirTemp = mean(AirTemp_Avg, na.rm = TRUE),
            Humidity = mean(Humidity_Avg, na.rm = TRUE),
            WindDir = mean(WindDir_Avg, na.rm = TRUE),
            WindSpd = mean(WindSpd_Avg, na.rm = TRUE),
            Radiation = mean(Radiation_Avg, na.rm = TRUE),.groups = 'drop')
climate_22_summer_yearly

# 2022 spring ----------------------------------------------------------------
# import data hi 22 ----------------------------------------------------------
climate_hi_22_spring <- read_table("Data/Data_climate_station/2022/weather_high_mai_2022.txt", col_names = FALSE)

# Remove the first two rows from the data
climate_hi_22_spring <- climate_hi_22_spring[-c(1, 2), ]  

# combine first two colums date and time 
climate_hi_22_spring <- climate_hi_22_spring |> 
  mutate(date_time = paste(X1, X2)) |> 
  select(date_time, X3:X8)

# have colnames without units
column_names <- c("date_time", "AirTemp_Avg", "Humidity_Avg", "WindDir_Avg", "WindSpd_Avg", "Radiation_Avg", "Rainfall")

colnames(climate_hi_22_spring) <- column_names
climate_hi_22_spring

str(climate_hi_22_spring)

# make date_time a date format
climate_hi_22_spring <- climate_hi_22_spring |> 
  mutate(date_time = dmy_hms(date_time))

# Replace commas with . and convert to numeric
climate_hi_22_spring <- climate_hi_22_spring |> 
  mutate(across(-date_time, ~as.numeric(str_replace_all(., ",", "."))))

# add column site and year ------------------------------------------------
climate_hi_22_spring <- climate_hi_22_spring |>
  mutate(site = "hi") |> 
  mutate(year = 2022)

# import data lo 22 spring------------------------------------------------
climate_lo_22_spring <- read_table("Data/Data_climate_station/2022/RangeX_LOW_2022_fall.txt", col_names = FALSE)

# Remove the first two rows from the data
climate_lo_22_spring <- climate_lo_22_spring[-c(1, 2), ]  

# combine first two colums date and time 
climate_lo_22_spring <- climate_lo_22_spring |> 
  mutate(date_time = paste(X1, X2)) |> 
  select(date_time, X3:X8)

# # have colnames without units
# column_names <- c("date_time", "AirTemp_Avg", "Humidity_Avg", "WindDir_Avg", "WindSpd_Avg", "Radiation_Avg", "Rainfall")

colnames(climate_lo_22_spring) <- column_names
climate_lo_22_spring

str(climate_lo_22_spring)

# make date_time a date format
climate_lo_22_spring <- climate_lo_22_spring |> 
  mutate(date_time = dmy_hms(date_time))

# Replace commas with . and convert to numeric
climate_lo_22_spring <- climate_lo_22_spring |> 
  mutate(across(-date_time, ~as.numeric(str_replace_all(., ",", "."))))

# add column site and year ------------------------------------------------
climate_lo_22_spring <- climate_lo_22_spring |>
  mutate(site = "lo") |> 
  mutate(year = 2022)


# filter lo 22 spring -----------------------------------------------------
# to match hi site
# we dont need 2021 here
start_date <- as.Date("2021-10-06") 
end_date <- as.Date("2022-05-29") 
# 
# Filter the data for the specified date range
climate_lo_22_spring <- climate_lo_22_spring |> 
  filter(between(date_time, left = start_date, right = end_date))

# combine 22 hi and lo spring ------------------------------------------------
climate_22_spring <- bind_rows(climate_hi_22_spring, climate_lo_22_spring)

climate_22_spring <- climate_22_spring |> 
  mutate(region = "NOR") |> 
  select(region, site, year, date_time, AirTemp_Avg, Humidity_Avg, 
         WindDir_Avg, WindSpd_Avg, Radiation_Avg, Rainfall)

# save clean data 22 spring--------------------------------------------------
# write.csv(climate_22_spring, "Data/Data_climate_station/2022/RangeX_clean_climate_station_NOR_spring_2022.csv")

# control plotting --------------------------------------------------------
climate_22_spring_plot <- climate_22_spring |> 
  mutate(date_time = as.Date(date_time)) |> 
  group_by(date_time, site) |> 
  summarize(AirTemp = mean(AirTemp_Avg, na.rm = TRUE),
            Humidity = mean(Humidity_Avg, na.rm = TRUE),
            WindDir = mean(WindDir_Avg, na.rm = TRUE),
            WindSpd = mean(WindSpd_Avg, na.rm = TRUE),
            Radiation = mean(Radiation_Avg, na.rm = TRUE),.groups = 'drop')
climate_22_spring_plot

# plot average temp per treat
temp_22_spring <- ggplot(climate_22_spring_plot, aes(x = date_time, y = AirTemp, colour = site)) +
  geom_line()+
  labs(title = "Daily temperature 2m above ground 2022", y = "Temperature (°C)", x = "Date")+
  scale_color_manual(values = c("lo" = "orange", "hi" = "turquoise"))
temp_22_spring

ggsave(filename = "RangeX_climate_station_temp_spring_22.png", 
       plot = temp_22_spring, 
       path = "Data/Data_climate_station/2022/Graphs", 
       width = 10, height = 6)

# plot average Humidity per treat
humidity_22_spring <- ggplot(climate_22_spring_plot, aes(x = date_time, y = Humidity, colour = site)) +
  geom_line()+
  labs(title = "Daily average humidity 22", y = "Humidity (%)", x = "Date")+
  scale_color_manual(values = c("lo" = "orange", "hi" = "turquoise"))+
  scale_y_continuous(limits = c(25, 100))
humidity_22_spring

ggsave(filename = "RangeX_climate_station_humidity_spring_22.png", 
       plot = humidity_22_spring, 
       path = "Data/Data_climate_station/2022/Graphs", 
       width = 10, height = 6)

# plot average Radiation per treat
radiation_22_spring <- ggplot(climate_22_spring_plot, aes(x = date_time, y = Radiation, colour = site)) +
  geom_line()+
  labs(title = "Daily average radiation 22", y = "Radiation (W.m-2)", x = "Date")+
  scale_color_manual(values = c("lo" = "orange", "hi" = "turquoise"))
radiation_22_spring

ggsave(filename = "RangeX_climate_station_radiation_spring_22.png", 
       plot = radiation_22_spring, 
       path = "Data/Data_climate_station/2022/Graphs", 
       width = 10, height = 6)

# rainfall 22 spring----------------------------------------------------------
# Aggregate rainfall data by day
daily_rainfall_22_spring <- climate_22_spring |> 
  group_by(date_time, site) |> 
  summarize(total_rainfall = sum(Rainfall, na.rm = TRUE), .groups = 'drop')

# plot average Rainfall per treat
ggplot(daily_rainfall_22_spring) +
  geom_col(aes(x = date_time, y = total_rainfall, fill = site)) +
  labs(title = "Daily Rainfall 22", x = "Date", y = "Sum rainfall per day (mm)")+
  scale_fill_manual(values = c("lo" = "orange", "hi" = "turquoise"))+
  scale_y_continuous(limits = c(0, 5.0))

# yearly average 22 spring --------------------------------------------------
climate_22_spring_yearly <- climate_22_spring |> 
  mutate(date_time = as.Date(date_time)) |> 
  group_by(site, year) |> 
  summarize(AirTemp = mean(AirTemp_Avg, na.rm = TRUE),
            Humidity = mean(Humidity_Avg, na.rm = TRUE),
            WindDir = mean(WindDir_Avg, na.rm = TRUE),
            WindSpd = mean(WindSpd_Avg, na.rm = TRUE),
            Radiation = mean(Radiation_Avg, na.rm = TRUE),.groups = 'drop')
climate_22_spring_yearly


# 2023 --------------------------------------------------------------------
# import data hi 23 ----------------------------------------------------------
climate_hi_23 <- read_table("Data/Data_climate_station/2023/RangeX_HIGH_fall_2023.txt", col_names = FALSE)

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

# # filter time as for tomst loggers ----------------------------------------
# start_date <- as.Date("2023-06-21") 
# end_date <- as.Date("2023-10-23") # were collected on 24.10
# 
# # Filter the data for the specified date range
# climate_hi_23 <- climate_hi_23 |> 
#   filter(between(date_time, left = start_date, right = end_date))

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

# Replace commas with . and convert to numeric
climate_lo_23 <- climate_lo_23 |> 
  mutate(across(-date_time, ~as.numeric(str_replace_all(., ",", "."))))

# add column site and year ------------------------------------------------
climate_lo_23 <- climate_lo_23 |>
  mutate(site = "lo") |> 
  mutate(year = 2023)

# combine 23 hi and lo ----------------------------------------------------
climate_23 <- bind_rows(climate_hi_23, climate_lo_23)

climate_23 <- climate_23 |> 
  mutate(region = "NOR") |> 
  select(region, site, year, date_time, AirTemp_Avg, Humidity_Avg, 
         WindDir_Avg, WindSpd_Avg, Radiation_Avg, Rainfall)

# save clean data 23 ------------------------------------------------------
# write.csv(climate_23, "Data/Data_climate_station/2023/RangeX_clean_climate_station_NOR_2023.csv")


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
temp_23 <- ggplot(climate_23_plot, aes(x = date_time, y = AirTemp, colour = site)) +
  geom_line()+
  labs(title = "Daily temperature 2m above ground 2023", y = "Temperature (°C)", x = "Date")+
  scale_color_manual(values = c("lo" = "orange", "hi" = "turquoise"))
temp_23

ggsave(filename = "RangeX_climate_station_temp_23.png", 
       plot = temp_23, 
       path = "Data/Data_climate_station/2023/Graphs", 
       width = 10, height = 6)

# plot average Humidity per treat
humidity_23 <- ggplot(climate_23_plot, aes(x = date_time, y = Humidity, colour = site)) +
  geom_line()+
  labs(title = "Daily average humidity 23", y = "Humidity (%)", x = "Date")+
  scale_color_manual(values = c("lo" = "orange", "hi" = "turquoise"))+
  scale_y_continuous(limits = c(25, 100))
humidity_23

ggsave(filename = "RangeX_climate_station_humidity_23.png", 
       plot = humidity_23, 
       path = "Data/Data_climate_station/2023/Graphs", 
       width = 10, height = 6)

# plot average Radiation per treat
radiation_23 <- ggplot(climate_23_plot, aes(x = date_time, y = Radiation, colour = site)) +
  geom_line()+
  labs(title = "Daily average radiation 23", y = "Radiation (W.m-2)", x = "Date")+
  scale_color_manual(values = c("lo" = "orange", "hi" = "turquoise"))
radiation_23

ggsave(filename = "RangeX_climate_station_radiation_23.png", 
       plot = radiation_23, 
       path = "Data/Data_climate_station/2023/Graphs", 
       width = 10, height = 6)

# rainfall 23 ----------------------------------------------------------------
# Aggregate rainfall data by day
daily_rainfall_23 <- climate_23 |> 
  group_by(date_time, site) |> 
  summarize(total_rainfall = sum(Rainfall, na.rm = TRUE), .groups = 'drop')

# plot average Rainfall per treat
ggplot(daily_rainfall_23) +
  geom_col(aes(x = date_time, y = total_rainfall, fill = site)) +
  labs(title = "Daily Rainfall 23", x = "Date", y = "Sum rainfall per day (mm)")+
  scale_fill_manual(values = c("lo" = "orange", "hi" = "turquoise"))+
  scale_y_continuous(limits = c(0, 5.0))

# yearly average 23 summer --------------------------------------------------
climate_23_summer_yearly <- climate_23 |> 
  mutate(date_time = as.Date(date_time)) |> 
  group_by(site, year) |> 
  summarize(AirTemp = mean(AirTemp_Avg, na.rm = TRUE),
            Humidity = mean(Humidity_Avg, na.rm = TRUE),
            WindDir = mean(WindDir_Avg, na.rm = TRUE),
            WindSpd = mean(WindSpd_Avg, na.rm = TRUE),
            Radiation = mean(Radiation_Avg, na.rm = TRUE),.groups = 'drop')
climate_23_summer_yearly

# 2023 spring ----------------------------------------------------------------
# import data hi 23 ----------------------------------------------------------
climate_hi_23_spring <- read_table("Data/Data_climate_station/2023/RangeX_HIGH_Spring2023.txt", col_names = FALSE)

# Remove the first two rows from the data
climate_hi_23_spring <- climate_hi_23_spring[-c(1, 2), ]  

# combine first two colums date and time 
climate_hi_23_spring <- climate_hi_23_spring |> 
  mutate(date_time = paste(X1, X2)) |> 
  select(date_time, X3:X8)

# have colnames without units
column_names <- c("date_time", "AirTemp_Avg", "Humidity_Avg", "WindDir_Avg", "WindSpd_Avg", "Radiation_Avg", "Rainfall")

colnames(climate_hi_23_spring) <- column_names
climate_hi_23_spring

str(climate_hi_23_spring)

# make date_time a date format
climate_hi_23_spring <- climate_hi_23_spring |> 
  mutate(date_time = dmy_hms(date_time))


# Replace commas with . and convert to numeric
climate_hi_23_spring <- climate_hi_23_spring |> 
  mutate(across(-date_time, ~as.numeric(str_replace_all(., ",", "."))))

# add column site and year ------------------------------------------------
climate_hi_23_spring <- climate_hi_23_spring |>
  mutate(site = "hi") |> 
  mutate(year = 2023)

# import data lo 23 spring ----------------------------------------------------
climate_lo_23_spring <- read_table("Data/Data_climate_station/2023/RangeX_LOW_spring2023.txt", col_names = FALSE)

# Remove the first two rows from the data
climate_lo_23_spring <- climate_lo_23_spring[-c(1, 2), ]  

# combine first two colums date and time 
climate_lo_23_spring <- climate_lo_23_spring |> 
  mutate(date_time = paste(X1, X2)) |> 
  select(date_time, X3:X8)

# # have colnames without units
# column_names <- c("date_time", "AirTemp_Avg", "Humidity_Avg", "WindDir_Avg", "WindSpd_Avg", "Radiation_Avg", "Rainfall")

colnames(climate_lo_23_spring) <- column_names
climate_lo_23_spring

str(climate_lo_23_spring)

# make date_time a date format
climate_lo_23_spring <- climate_lo_23_spring |> 
  mutate(date_time = dmy_hms(date_time))

# Replace commas with . and convert to numeric
climate_lo_23_spring <- climate_lo_23_spring |> 
  mutate(across(-date_time, ~as.numeric(str_replace_all(., ",", "."))))

# add column site and year ------------------------------------------------
climate_lo_23_spring <- climate_lo_23_spring |>
  mutate(site = "lo") |> 
  mutate(year = 2023)

# combine 23 hi and lo ----------------------------------------------------
climate_23_spring <- bind_rows(climate_hi_23_spring, climate_lo_23_spring)

climate_23_spring <- climate_23_spring |> 
  mutate(region = "NOR") |> 
  select(region, site, year, date_time, AirTemp_Avg, Humidity_Avg, 
         WindDir_Avg, WindSpd_Avg, Radiation_Avg, Rainfall)

# save clean data 23 ------------------------------------------------------
write.csv(climate_23_spring, "Data/Data_climate_station/2023/RangeX_clean_climate_station_NOR_spring_2023.csv")


# control plotting --------------------------------------------------------
climate_23_spring_plot <- climate_23_spring |> 
  mutate(date_time = as.Date(date_time)) |> 
  group_by(date_time, site) |> 
  summarize(AirTemp = mean(AirTemp_Avg, na.rm = TRUE),
            Humidity = mean(Humidity_Avg, na.rm = TRUE),
            WindDir = mean(WindDir_Avg, na.rm = TRUE),
            WindSpd = mean(WindSpd_Avg, na.rm = TRUE),
            Radiation = mean(Radiation_Avg, na.rm = TRUE),.groups = 'drop')
climate_23_spring_plot

# plot average temp per treat
temp_23_spring <- ggplot(climate_23_spring_plot, aes(x = date_time, y = AirTemp, colour = site)) +
  geom_line()+
  labs(title = "Daily temperature 2m above ground 2023", y = "Temperature (°C)", x = "Date")+
  scale_color_manual(values = c("lo" = "orange", "hi" = "turquoise"))
temp_23_spring

ggsave(filename = "RangeX_climate_station_temp_spring_23.png", 
       plot = temp_23_spring, 
       path = "Data/Data_climate_station/2023/Graphs", 
       width = 10, height = 6)

# plot average Humidity per treat
humidity_23_spring <- ggplot(climate_23_spring_plot, aes(x = date_time, y = Humidity, colour = site)) +
  geom_line()+
  labs(title = "Daily average humidity 23", y = "Humidity (%)", x = "Date")+
  scale_color_manual(values = c("lo" = "orange", "hi" = "turquoise"))+
  scale_y_continuous(limits = c(25, 100))
humidity_23_spring

ggsave(filename = "RangeX_climate_station_humidity_spring_23.png", 
       plot = humidity_23_spring, 
       path = "Data/Data_climate_station/2023/Graphs", 
       width = 10, height = 6)

# plot average Radiation per treat
radiation_23_spring <- ggplot(climate_23_spring_plot, aes(x = date_time, y = Radiation, colour = site)) +
  geom_line()+
  labs(title = "Daily average radiation 23", y = "Radiation (W.m-2)", x = "Date")+
  scale_color_manual(values = c("lo" = "orange", "hi" = "turquoise"))
radiation_23_spring

ggsave(filename = "RangeX_climate_station_radiation_spring_23.png", 
       plot = radiation_23_spring, 
       path = "Data/Data_climate_station/2023/Graphs", 
       width = 10, height = 6)

# rainfall 23 spring -------------------------------------------------------
# Aggregate rainfall data by day
daily_rainfall_23_spring <- climate_23_spring |> 
  group_by(date_time, site) |> 
  summarize(total_rainfall = sum(Rainfall, na.rm = TRUE), .groups = 'drop')

# plot average Rainfall per treat
ggplot(daily_rainfall_23_spring) +
  geom_col(aes(x = date_time, y = total_rainfall, fill = site)) +
  labs(title = "Daily Rainfall 23", x = "Date", y = "Sum rainfall per day (mm)")+
  scale_fill_manual(values = c("lo" = "orange", "hi" = "turquoise"))+
  scale_y_continuous(limits = c(0, 5.0))

# yearly average 23 spring --------------------------------------------------
climate_23_spring_yearly <- climate_23_spring |> 
  mutate(date_time = as.Date(date_time)) |> 
  group_by(site, year) |> 
  summarize(AirTemp = mean(AirTemp_Avg, na.rm = TRUE),
            Humidity = mean(Humidity_Avg, na.rm = TRUE),
            WindDir = mean(WindDir_Avg, na.rm = TRUE),
            WindSpd = mean(WindSpd_Avg, na.rm = TRUE),
            Radiation = mean(Radiation_Avg, na.rm = TRUE),.groups = 'drop')
climate_23_spring_yearly

# 2024 --------------------------------------------------------------------
# import data hi 24 ----------------------------------------------------------
climate_hi_24 <- read_table("Data/Data_climate_station/2024/RangeX_HIGH_15.10.24.txt", col_names = FALSE)

# Remove the first two rows from the data
climate_hi_24 <- climate_hi_24[-c(1, 2), ]  

# combine first two colums date and time 
climate_hi_24 <- climate_hi_24 |> 
  mutate(date_time = paste(X1, X2)) |> 
  select(date_time, X3:X8)

# colnames same as in years before
colnames(climate_hi_24) <- column_names
climate_hi_24

str(climate_hi_24)

# make date_time a date format
climate_hi_24 <- climate_hi_24 |> 
  mutate(date_time = dmy_hms(date_time))

# # filter time as for tomst loggers ----------------------------------------
# start_date <- as.Date("2023-06-21") 
# end_date <- as.Date("2023-10-23") # were collected on 24.10
# 
# # Filter the data for the specified date range
# climate_hi_23 <- climate_hi_23 |> 
#   filter(between(date_time, left = start_date, right = end_date))

# Replace commas with . and convert to numeric
climate_hi_24 <- climate_hi_24 |> 
  mutate(across(-date_time, ~as.numeric(str_replace_all(., ",", "."))))

# add column site and year ------------------------------------------------
climate_hi_24 <- climate_hi_24 |>
  mutate(site = "hi") |> 
  mutate(year = 2024)

# import data lo 24 ----------------------------------------------------------
climate_lo_24 <- read_table("Data/Data_climate_station/2024/RangeX_LOW_15.10.24.txt", col_names = FALSE)

climate_lo_24 <- climate_lo_24[-c(1, 2), ]  

# combine first two colums date and time 
climate_lo_24 <- climate_lo_24 |> 
  mutate(date_time = paste(X1, X2)) |> 
  select(date_time, X3:X8)

colnames(climate_lo_24) <- column_names
climate_lo_24

str(climate_lo_24)

# make date_time a date format
climate_lo_24 <- climate_lo_24 |> 
  mutate(date_time = dmy_hms(date_time))

# Replace commas with . and convert to numeric
climate_lo_24 <- climate_lo_24 |> 
  mutate(across(-date_time, ~as.numeric(str_replace_all(., ",", "."))))

# add column site and year ------------------------------------------------
climate_lo_24 <- climate_lo_24 |>
  mutate(site = "lo") |> 
  mutate(year = 2024)


# combine 24 hi and lo ----------------------------------------------------
climate_24 <- bind_rows(climate_hi_24, climate_lo_24)

climate_24 <- climate_24 |> 
  mutate(region = "NOR") |> 
  select(region, site, year, date_time, AirTemp_Avg, Humidity_Avg, 
         WindDir_Avg, WindSpd_Avg, Radiation_Avg, Rainfall)

# save clean data 24 ------------------------------------------------------
# write.csv(climate_24, "Data/Data_climate_station/2024/RangeX_clean_climate_station_NOR_2024.csv")

# control plotting 24--------------------------------------------------------
climate_24_plot <- climate_24 |> 
  mutate(date_time = as.Date(date_time)) |> 
  group_by(date_time, site) |> 
  summarize(AirTemp = mean(AirTemp_Avg, na.rm = TRUE),
            Humidity = mean(Humidity_Avg, na.rm = TRUE),
            WindDir = mean(WindDir_Avg, na.rm = TRUE),
            WindSpd = mean(WindSpd_Avg, na.rm = TRUE),
            Radiation = mean(Radiation_Avg, na.rm = TRUE),.groups = 'drop')
climate_24_plot

# plot average temp per treat
temp_24 <- ggplot(climate_24_plot, aes(x = date_time, y = AirTemp, colour = site)) +
  geom_line()+
  labs(title = "Daily temperature 2m above ground 2024", y = "Temperature (°C)", x = "Date")+
  scale_color_manual(values = c("lo" = "orange", "hi" = "turquoise"))
temp_24

ggsave(filename = "RangeX_climate_station_temp_24.png", 
       plot = temp_24, 
       path = "Data/Data_climate_station/2024/Graphs", 
       width = 10, height = 6)

# plot average Humidity per treat
humidity_24 <- ggplot(climate_24_plot, aes(x = date_time, y = Humidity, colour = site)) +
  geom_line()+
  labs(title = "Daily average humidity 24", y = "Humidity (%)", x = "Date")+
  scale_color_manual(values = c("lo" = "orange", "hi" = "turquoise"))+
  scale_y_continuous(limits = c(25, 100))
humidity_24

ggsave(filename = "RangeX_climate_station_humidity_24.png", 
       plot = humidity_24, 
       path = "Data/Data_climate_station/2024/Graphs", 
       width = 10, height = 6)

# plot average Radiation per treat
radiation_24 <- ggplot(climate_24_plot, aes(x = date_time, y = Radiation, colour = site)) +
  geom_line()+
  labs(title = "Daily average radiation 24", y = "Radiation (W.m-2)", x = "Date")+
  scale_color_manual(values = c("lo" = "orange", "hi" = "turquoise"))
radiation_24

ggsave(filename = "RangeX_climate_station_radiation_24.png", 
       plot = radiation_24, 
       path = "Data/Data_climate_station/2024/Graphs", 
       width = 10, height = 6)

# rainfall 24 ----------------------------------------------------------------
# Aggregate rainfall data by day
daily_rainfall_24 <- climate_24 |> 
  group_by(date_time, site) |> 
  summarize(total_rainfall = sum(Rainfall, na.rm = TRUE), .groups = 'drop')

# plot average Rainfall per treat
ggplot(daily_rainfall_24) +
  geom_col(aes(x = date_time, y = total_rainfall, fill = site)) +
  labs(title = "Daily Rainfall 24", x = "Date", y = "Sum rainfall per day (mm)")+
  scale_fill_manual(values = c("lo" = "orange", "hi" = "turquoise"))+
  scale_y_continuous(limits = c(0, 5.0))

# yes, low site didn't record rainfall

# yearly average 24 summer --------------------------------------------------
climate_24_yearly <- climate_24 |> 
  mutate(date_time = as.Date(date_time)) |> 
  group_by(site, year) |> 
  summarize(AirTemp = mean(AirTemp_Avg, na.rm = TRUE),
            Humidity = mean(Humidity_Avg, na.rm = TRUE),
            WindDir = mean(WindDir_Avg, na.rm = TRUE),
            WindSpd = mean(WindSpd_Avg, na.rm = TRUE),
            Radiation = mean(Radiation_Avg, na.rm = TRUE),.groups = 'drop')
climate_24_yearly

# 2024 spring----------------------------------------------------------------
# import data hi 24 ----------------------------------------------------------
climate_hi_24_spring <- read_table("Data/Data_climate_station/2024/RangeX_HIGH_21.05.24.txt", col_names = FALSE)

# Remove the first two rows from the data
climate_hi_24_spring <- climate_hi_24_spring[-c(1, 2), ]  

# combine first two colums date and time 
climate_hi_24_spring <- climate_hi_24_spring |> 
  mutate(date_time = paste(X1, X2)) |> 
  select(date_time, X3:X8)

# colnames same as in years before
colnames(climate_hi_24_spring) <- column_names
climate_hi_24_spring

str(climate_hi_24_spring)

# make date_time a date format
climate_hi_24_spring <- climate_hi_24_spring |> 
  mutate(date_time = dmy_hms(date_time))

# Replace commas with . and convert to numeric
climate_hi_24_spring <- climate_hi_24_spring |> 
  mutate(across(-date_time, ~as.numeric(str_replace_all(., ",", "."))))

# add column site and year ------------------------------------------------
climate_hi_24_spring <- climate_hi_24_spring |>
  mutate(site = "hi") |> 
  mutate(year = 2024)

# import data lo 24 ----------------------------------------------------------
climate_lo_24_spring <- read_table("Data/Data_climate_station/2024/RangeX_LOW_15.05.24.txt", col_names = FALSE)

climate_lo_24_spring <- climate_lo_24_spring[-c(1, 2), ]  

# combine first two colums date and time 
climate_lo_24_spring <- climate_lo_24_spring |> 
  mutate(date_time = paste(X1, X2)) |> 
  select(date_time, X3:X8)

colnames(climate_lo_24_spring) <- column_names
climate_lo_24_spring

str(climate_lo_24_spring)

# make date_time a date format
climate_lo_24_spring <- climate_lo_24_spring |> 
  mutate(date_time = dmy_hms(date_time))

# Replace commas with . and convert to numeric
climate_lo_24_spring <- climate_lo_24_spring |> 
  mutate(across(-date_time, ~as.numeric(str_replace_all(., ",", "."))))

# add column site and year ------------------------------------------------
climate_lo_24_spring <- climate_lo_24_spring |>
  mutate(site = "lo") |> 
  mutate(year = 2024)


# combine 24 hi and lo ----------------------------------------------------
climate_24_spring <- bind_rows(climate_hi_24_spring, climate_lo_24_spring)

climate_24_spring <- climate_24_spring |> 
  mutate(region = "NOR") |> 
  select(region, site, year, date_time, AirTemp_Avg, Humidity_Avg, 
         WindDir_Avg, WindSpd_Avg, Radiation_Avg, Rainfall)

# save clean data 24 spring -------------------------------------------------
write.csv(climate_24_spring, "Data/Data_climate_station/2024/RangeX_clean_climate_station_NOR_spring_2024.csv")

# control plotting 24 spring--------------------------------------------------
climate_24_spring_plot <- climate_24_spring |> 
  mutate(date_time = as.Date(date_time)) |> 
  group_by(date_time, site) |> 
  summarize(AirTemp = mean(AirTemp_Avg, na.rm = TRUE),
            Humidity = mean(Humidity_Avg, na.rm = TRUE),
            WindDir = mean(WindDir_Avg, na.rm = TRUE),
            WindSpd = mean(WindSpd_Avg, na.rm = TRUE),
            Radiation = mean(Radiation_Avg, na.rm = TRUE),.groups = 'drop')
climate_24_spring_plot

# plot average temp per treat
temp_24_spring <- ggplot(climate_24_spring_plot, aes(x = date_time, y = AirTemp, colour = site)) +
  geom_line()+
  labs(title = "Daily temperature 2m above ground 2024", y = "Temperature (°C)", x = "Date")+
  scale_color_manual(values = c("lo" = "orange", "hi" = "turquoise"))
temp_24_spring

ggsave(filename = "RangeX_climate_station_temp_spring_24.png", 
        plot = temp_24_spring, 
        path = "Data/Data_climate_station/2024/Graphs", 
        width = 10, height = 6)

# plot average Humidity per treat
humidity_24_spring <- ggplot(climate_24_spring_plot, aes(x = date_time, y = Humidity, colour = site)) +
  geom_line()+
  labs(title = "Daily average humidity 24", y = "Humidity (%)", x = "Date")+
  scale_color_manual(values = c("lo" = "orange", "hi" = "turquoise"))+
  scale_y_continuous(limits = c(25, 100))
humidity_24_spring

ggsave(filename = "RangeX_climate_station_humidity_spring_24.png", 
       plot = humidity_24_spring, 
       path = "Data/Data_climate_station/2024/Graphs", 
       width = 10, height = 6)

# plot average Radiation per treat
radiation_24_spring <- ggplot(climate_24_spring_plot, aes(x = date_time, y = Radiation, colour = site)) +
  geom_line()+
  labs(title = "Daily average radiation 24", y = "Radiation (W.m-2)", x = "Date")+
  scale_color_manual(values = c("lo" = "orange", "hi" = "turquoise"))
radiation_24_spring

ggsave(filename = "RangeX_climate_station_radiation_spring_24.png", 
       plot = radiation_24_spring, 
       path = "Data/Data_climate_station/2024/Graphs", 
       width = 10, height = 6)

# rainfall 24 spring----------------------------------------------------------
# Aggregate rainfall data by day
daily_rainfall_24_spring <- climate_24_spring |> 
  group_by(date_time, site) |> 
  summarize(total_rainfall = sum(Rainfall, na.rm = TRUE), .groups = 'drop')

# plot average Rainfall per treat
ggplot(daily_rainfall_24_spring) +
  geom_col(aes(x = date_time, y = total_rainfall, fill = site)) +
  labs(title = "Daily Rainfall 24", x = "Date", y = "Sum rainfall per day (mm)")+
  scale_fill_manual(values = c("lo" = "orange", "hi" = "turquoise"))+
  scale_y_continuous(limits = c(0, 5.0))

# yearly average 24 spring --------------------------------------------------
climate_24_spring_yearly <- climate_24_spring |> 
  mutate(date_time = as.Date(date_time)) |> 
  group_by(site, year) |> 
  summarize(AirTemp = mean(AirTemp_Avg, na.rm = TRUE),
            Humidity = mean(Humidity_Avg, na.rm = TRUE),
            WindDir = mean(WindDir_Avg, na.rm = TRUE),
            WindSpd = mean(WindSpd_Avg, na.rm = TRUE),
            Radiation = mean(Radiation_Avg, na.rm = TRUE),.groups = 'drop')
climate_24_spring_yearly

# 2025 spring----------------------------------------------------------------
# import data hi 25 ----------------------------------------------------------
climate_hi_25_spring <- read_table("Data/Data_climate_station/2025/RangeX_HIGH_08.05.25.txt", col_names = FALSE)

# Remove the first two rows from the data
climate_hi_25_spring <- climate_hi_25_spring[-c(1, 2), ]  

# combine first two colums date and time 
climate_hi_25_spring <- climate_hi_25_spring |> 
  mutate(date_time = paste(X1, X2)) |> 
  select(date_time, X3:X8)

# colnames same as in years before
colnames(climate_hi_25_spring) <- column_names
climate_hi_25_spring

str(climate_hi_25_spring)

# make date_time a date format
climate_hi_25_spring <- climate_hi_25_spring |> 
  mutate(date_time = dmy_hms(date_time))

# Replace commas with . and convert to numeric
climate_hi_25_spring <- climate_hi_25_spring |> 
  mutate(across(-date_time, ~as.numeric(str_replace_all(., ",", "."))))

# add column site and year ------------------------------------------------
climate_hi_25_spring <- climate_hi_25_spring |>
  mutate(site = "hi") |> 
  mutate(year = 2025)

# import data lo 25 ----------------------------------------------------------
climate_lo_25_spring <- read_table("Data/Data_climate_station/2025/RangeX_LOW_16.04.25.txt", col_names = FALSE)

climate_lo_25_spring <- climate_lo_25_spring[-c(1, 2), ]  

# combine first two colums date and time 
climate_lo_25_spring <- climate_lo_25_spring |> 
  mutate(date_time = paste(X1, X2)) |> 
  select(date_time, X3:X8)

colnames(climate_lo_25_spring) <- column_names
climate_lo_25_spring

str(climate_lo_25_spring)

# make date_time a date format
climate_lo_25_spring <- climate_lo_25_spring |> 
  mutate(date_time = dmy_hms(date_time))

# Replace commas with . and convert to numeric
climate_lo_25_spring <- climate_lo_25_spring |> 
  mutate(across(-date_time, ~as.numeric(str_replace_all(., ",", "."))))

# add column site and year ------------------------------------------------
climate_lo_25_spring <- climate_lo_25_spring |>
  mutate(site = "lo") |> 
  mutate(year = 2025)


# combine 25 hi and lo ----------------------------------------------------
climate_25_spring <- bind_rows(climate_hi_25_spring, climate_lo_25_spring)

climate_25_spring <- climate_25_spring |> 
  mutate(region = "NOR") |> 
  select(region, site, year, date_time, AirTemp_Avg, Humidity_Avg, 
         WindDir_Avg, WindSpd_Avg, Radiation_Avg, Rainfall)

# save clean data 25 spring -------------------------------------------------
# write.csv(climate_25_spring, "Data/Data_climate_station/2025/RangeX_clean_climate_station_NOR_spring_2025.csv")

# control plotting 25 spring--------------------------------------------------
climate_25_spring_plot <- climate_25_spring |> 
  mutate(date_time = as.Date(date_time)) |> 
  group_by(date_time, site) |> 
  summarize(AirTemp = mean(AirTemp_Avg, na.rm = TRUE),
            Humidity = mean(Humidity_Avg, na.rm = TRUE),
            WindDir = mean(WindDir_Avg, na.rm = TRUE),
            WindSpd = mean(WindSpd_Avg, na.rm = TRUE),
            Radiation = mean(Radiation_Avg, na.rm = TRUE),.groups = 'drop')
climate_25_spring_plot

# plot average temp per treat
temp_25_spring <- ggplot(climate_25_spring_plot, aes(x = date_time, y = AirTemp, colour = site)) +
  geom_line()+
  labs(title = "Daily temperature 2m above ground 2025", y = "Temperature (°C)", x = "Date")+
  scale_color_manual(values = c("lo" = "orange", "hi" = "turquoise"))
temp_25_spring

ggsave(filename = "RangeX_climate_station_temp_spring_25.png", 
       plot = temp_25_spring, 
       path = "Data/Data_climate_station/2025/Graphs", 
       width = 10, height = 6)

# plot average Humidity per treat
humidity_25_spring <- ggplot(climate_25_spring_plot, aes(x = date_time, y = Humidity, colour = site)) +
  geom_line()+
  labs(title = "Daily average humidity 25", y = "Humidity (%)", x = "Date")+
  scale_color_manual(values = c("lo" = "orange", "hi" = "turquoise"))+
  scale_y_continuous(limits = c(25, 100))
humidity_25_spring

ggsave(filename = "RangeX_climate_station_humidity_spring_25.png", 
       plot = humidity_25_spring, 
       path = "Data/Data_climate_station/2025/Graphs", 
       width = 10, height = 6)

# plot average Radiation per treat
radiation_25_spring <- ggplot(climate_25_spring_plot, aes(x = date_time, y = Radiation, colour = site)) +
  geom_line()+
  labs(title = "Daily average radiation 25", y = "Radiation (W.m-2)", x = "Date")+
  scale_color_manual(values = c("lo" = "orange", "hi" = "turquoise"))
radiation_25_spring

ggsave(filename = "RangeX_climate_station_radiation_spring_25.png", 
       plot = radiation_25_spring, 
       path = "Data/Data_climate_station/2025/Graphs", 
       width = 10, height = 6)

# rainfall 25 spring----------------------------------------------------------
# Aggregate rainfall data by day
daily_rainfall_25_spring <- climate_25_spring |> 
  group_by(date_time, site) |> 
  summarize(total_rainfall = sum(Rainfall, na.rm = TRUE), .groups = 'drop')

# plot average Rainfall per treat
ggplot(daily_rainfall_25_spring) +
  geom_col(aes(x = date_time, y = total_rainfall, fill = site)) +
  labs(title = "Daily Rainfall 25", x = "Date", y = "Sum rainfall per day (mm)")+
  scale_fill_manual(values = c("lo" = "orange", "hi" = "turquoise"))+
  scale_y_continuous(limits = c(0, 5.0))

# yearly average 25 spring --------------------------------------------------
climate_25_spring_yearly <- climate_25_spring |> 
  mutate(date_time = as.Date(date_time)) |> 
  group_by(site, year) |> 
  summarize(AirTemp = mean(AirTemp_Avg, na.rm = TRUE),
            Humidity = mean(Humidity_Avg, na.rm = TRUE),
            WindDir = mean(WindDir_Avg, na.rm = TRUE),
            WindSpd = mean(WindSpd_Avg, na.rm = TRUE),
            Radiation = mean(Radiation_Avg, na.rm = TRUE),.groups = 'drop')
climate_25_spring_yearly

# 2021 - 2025 -------------------------------------------------------------
yearly_climate_all_years <- bind_rows(climate_21_yearly, climate_22_spring_yearly,
                               climate_22_summer_yearly, climate_23_spring_yearly,
                               climate_23_summer_yearly, climate_24_spring_yearly,
                               climate_24_yearly, climate_25_spring_yearly)
yearly_climate_all_years


# arrange plots of all years together -------------------------------------
climate_all_years <- bind_rows(climate_21, climate_22_spring,
                       climate_22, climate_23_spring,
                       climate_23, climate_24_spring,
                       climate_24, climate_25_spring)

# save clean data 21-25 -------------------------------------------------
# write.csv(climate_all_years, "Data/Data_climate_station/RangeX_clean_climate_station_NOR_2021-2025.csv")


# calculate monthly averages ----------------------------------------------
climate_monthly <- climate_all_years |> 
  mutate(date_time = as.Date(date_time),
         year = year(date_time),
         month = month(date_time)) |> 
  group_by(site, year, month) |> 
  summarize(
    AirTemp_mean = mean(AirTemp_Avg, na.rm = TRUE),
    AirTemp_min = min(AirTemp_Avg, na.rm = TRUE),
    AirTemp_max = max(AirTemp_Avg, na.rm = TRUE),
    
    Humidity_mean = mean(Humidity_Avg, na.rm = TRUE),
    Humidity_min = min(Humidity_Avg, na.rm = TRUE),
    Humidity_max = max(Humidity_Avg, na.rm = TRUE),
    
    WindDir_mean = mean(WindDir_Avg, na.rm = TRUE),
    WindDir_min = min(WindDir_Avg, na.rm = TRUE),
    WindDir_max = max(WindDir_Avg, na.rm = TRUE),
    
    WindSpd_mean = mean(WindSpd_Avg, na.rm = TRUE),
    WindSpd_min = min(WindSpd_Avg, na.rm = TRUE),
    WindSpd_max = max(WindSpd_Avg, na.rm = TRUE),
    
    Radiation_mean = mean(Radiation_Avg, na.rm = TRUE),
    Radiation_min = min(Radiation_Avg, na.rm = TRUE),
    Radiation_max = max(Radiation_Avg, na.rm = TRUE),
    
    .groups = 'drop'
  )
climate_monthly

ggplot(climate_monthly, aes(x = month, y = AirTemp_mean, color = factor(year), group = year)) +
  geom_line() +
  geom_ribbon(aes(ymin = AirTemp_min, ymax = AirTemp_max, fill = factor(year)), alpha = 0.2, color = NA) +
  facet_wrap(~ site) +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  labs(x = "Time", 
       y = expression("Temperature ("*~degree*C*")"), 
       color = "Year", 
       fill = "Year",
       title = "Monthly Air Temperature (mean, min, max)")


# calculate yearly averages -----------------------------------------------
climate_all_years_yearly_avg <- climate_all_years |> 
  group_by(year, site) |> 
  summarize(AirTemp = mean(AirTemp_Avg, na.rm = TRUE),
            Humidity = mean(Humidity_Avg, na.rm = TRUE),
            WindDir = mean(WindDir_Avg, na.rm = TRUE),
            WindSpd = mean(WindSpd_Avg, na.rm = TRUE),
            Radiation = mean(Radiation_Avg, na.rm = TRUE),.groups = 'drop')
climate_all_years_yearly_avg

# write.csv(climate_all_years_yearly_avg, "Data/Data_climate_station/RangeX_clean_climate_station_NOR_yearly_avg_2021-2025.csv")


# calculate site averages and sd --------------------------------------------
climate_site <- climate_all_years |> 
  group_by(site) |> 
  summarize(
    AirTemp_mean = mean(AirTemp_Avg, na.rm = TRUE),
    AirTemp_sd = sd(AirTemp_Avg, na.rm = TRUE),
    Humidity_mean = mean(Humidity_Avg, na.rm = TRUE),
    Humidity_sd = sd(Humidity_Avg, na.rm = TRUE),
    WindDir_mean = mean(WindDir_Avg, na.rm = TRUE),
    WindDir_sd = sd(WindDir_Avg, na.rm = TRUE),
    WindSpd_mean = mean(WindSpd_Avg, na.rm = TRUE),
    WindSpd_sd = sd(WindSpd_Avg, na.rm = TRUE),
    Radiation_mean = mean(Radiation_Avg, na.rm = TRUE),
    Radiation_sd = sd(Radiation_Avg, na.rm = TRUE),
    .groups = 'drop'
  )
climate_site
# is this biased by two winters without summer (21 and 25)?

# filter only 2022, 23 and 24
climate_site_22_23_24 <- climate_all_years |> 
  filter(year %in% c(2022, 2023, 2024)) |> 
  group_by(site) |> 
  summarize(
    AirTemp_mean = mean(AirTemp_Avg, na.rm = TRUE),
    AirTemp_sd = sd(AirTemp_Avg, na.rm = TRUE),
    Humidity_mean = mean(Humidity_Avg, na.rm = TRUE),
    Humidity_sd = sd(Humidity_Avg, na.rm = TRUE),
    WindDir_mean = mean(WindDir_Avg, na.rm = TRUE),
    WindDir_sd = sd(WindDir_Avg, na.rm = TRUE),
    WindSpd_mean = mean(WindSpd_Avg, na.rm = TRUE),
    WindSpd_sd = sd(WindSpd_Avg, na.rm = TRUE),
    Radiation_mean = mean(Radiation_Avg, na.rm = TRUE),
    Radiation_sd = sd(Radiation_Avg, na.rm = TRUE),
    .groups = 'drop'
  )
climate_site_22_23_24

write.csv(climate_site_22_23_24, "Data/Data_climate_station/RangeX_clean_climate_station_NOR_site_average.csv")


# yearly rain -------------------------------------------------------------
# Aggregate rainfall data by year
rainfall_year <- climate_all_years |>
  filter(year %in% c(2022, 2023, 2024)) |>
  group_by(year, site) |> 
  summarize(total_rainfall = sum(Rainfall, na.rm = TRUE), .groups = 'drop')
rainfall_year

rainfall_year_mean <- rainfall_year |> 
  group_by(site) |> 
  summarize(
    total_rainfall_mean = mean(total_rainfall, na.rm = TRUE),
    total_rainfall_sd = sd(total_rainfall, na.rm = TRUE), .groups = 'drop')
rainfall_year_mean

# plot climate all years daily average -----------------------------
climate_all_years_plot <- climate_all_years |> 
  mutate(date_time = as.Date(date_time)) |> 
  group_by(date_time, site) |> 
  summarize(AirTemp = mean(AirTemp_Avg, na.rm = TRUE),
            Humidity = mean(Humidity_Avg, na.rm = TRUE),
            WindDir = mean(WindDir_Avg, na.rm = TRUE),
            WindSpd = mean(WindSpd_Avg, na.rm = TRUE),
            Radiation = mean(Radiation_Avg, na.rm = TRUE),.groups = 'drop')
climate_all_years_plot

temp <- ggplot(climate_all_years_plot, aes(x = date_time, y = AirTemp, colour = site)) +
  geom_line()+
  labs(title = "Daily temperature 2m above ground", 
       y = expression("Temperature ("*~degree*C*")"),
       x = "Date")+
  scale_color_manual(values = c("lo" = "orange", "hi" = "turquoise"))
temp

ggsave(filename = "RangeX_climate_station_temp_21-25.png", 
       plot = temp, 
       path = "Data/Data_climate_station/Graphs/", 
       width = 10, height = 6)

# plot average Humidity per treat
humidity <- ggplot(climate_all_years_plot, aes(x = date_time, y = Humidity, colour = site)) +
  geom_line()+
  labs(title = "Daily average humidity ", y = "Humidity (%)", x = "Date")+
  scale_color_manual(values = c("lo" = "orange", "hi" = "turquoise"))+
  scale_y_continuous(limits = c(25, 100))
humidity

ggsave(filename = "RangeX_climate_station_humidity_21-25.png", 
       plot = humidity, 
       path = "Data/Data_climate_station/Graphs", 
       width = 10, height = 6)

# plot average Radiation per treat
radiation <- ggplot(climate_all_years_plot, aes(x = date_time, y = Radiation, colour = site)) +
  geom_line()+
  labs(title = "Daily average radiation", y = "Radiation (W.m-2)", x = "Date")+
  scale_color_manual(values = c("lo" = "orange", "hi" = "turquoise"))
radiation

ggsave(filename = "RangeX_climate_station_radiation_21-25.png", 
       plot = radiation, 
       path = "Data/Data_climate_station/Graphs", 
       width = 10, height = 6)

# rainfall ----------------------------------------------------------
# Aggregate rainfall data by day
daily_rainfall <- climate_all_years |> 
  group_by(date_time, site) |> 
  summarize(total_rainfall = sum(Rainfall, na.rm = TRUE), .groups = 'drop')

# plot average Rainfall per treat
ggplot(daily_rainfall) +
  geom_col(aes(x = date_time, y = total_rainfall, fill = site)) +
  labs(title = "Daily Rainfall", x = "Date", y = "Sum rainfall per day (mm)")+
  scale_fill_manual(values = c("lo" = "orange", "hi" = "turquoise"))+
  scale_y_continuous(limits = c(0, 5.0))




# individual plots 21-24 --------------------------------------------------
# temperature
temp <- grid.arrange(temp_21, temp_22, temp_23, temp_24, ncol = 3)

ggsave(filename = "RangeX_climate_station_temperature_21_22_23_24.png", 
       plot = temp, 
       path = "Data/Data_climate_station/Graphs", 
       width = 15, height = 6)

# humidity
humidity <- grid.arrange(humidity_21, humidity_22, humidity_23, humidity_24, ncol = 3)

ggsave(filename = "RangeX_climate_station_humidity_21_22_23_24.png", 
       plot = humidity, 
       path = "Data/Data_climate_station/Graphs", 
       width = 15, height = 6)

# radiation
radiation <- grid.arrange(radiation_21, radiation_22, radiation_23, radiation_24, ncol = 3)

ggsave(filename = "RangeX_climate_station_radiation_21_22_23_24.png", 
       plot = radiation, 
       path = "Data/Data_climate_station/Graphs", 
       width = 15, height = 6)




# sunniness level ---------------------------------------------------------
# Filter for dates between June 15 and September 15
# its from 20.06 then
climate_23_filtered <- climate_23 |> 
  filter(format(date_time, "%Y-%m-%d") >= "2023-06-15" & format(date_time, "%Y-%m-%d") <= "2023-09-15")  

# Summarize data to get one solar radiation value per hour
climate_23_filtered <- climate_23_filtered |> 
  distinct() |> 
  group_by(site, hour = hour(date_time), date = as.Date(date_time)) |> 
  summarise(value = mean(Radiation_Avg))

# Perform 95% quantile regression to determine sunniness for each site at each hour of the day
# # use sth like this: 
# dat %>% 
#   nest(data = -Species) %>%
#   mutate(model = map(data, ~lm(Flipper.Length..mm.~Body.Mass..g., data = .))
# 
# # instead of do
predictions <- climate_23_filtered |> 
  group_by(site, hour) |> 
  do({
    fit <- rq(value ~ splines::bs(date, df = 10), tau = 0.95, data = .)
    augment(fit, data = .)
  })

# Calculate sunniness proportion
# 0 = complete cloudy and 1 = clear sky
sunniness <- predictions |> 
  mutate(prop = value / .fitted) |> 
  group_by(site, date) |> 
  summarise(sunniness = weighted.mean(prop, w = .fitted)) |> 
  mutate(sunniness = if_else(sunniness > 1, 1, sunniness)) |> 
  ungroup() 
# what is dict_Site?
# do I need this part?
  # mutate(siteID = plyr::mapvalues(site, from = dict_Site$v3, to = dict_Site$new)) |> 
  # select(-site)


# assign sunny status -----------------------------------------------------
# top third is sunny
# lower third is cloudy
# intermediate is discarded
# Classify days into "sunny," "cloudy," and "intermediate"
sunniness <- sunniness |> 
  mutate(sun_status = case_when(
    sunniness >= 0.66 ~ "Sunny",
    sunniness <= 0.33 ~ "Cloudy",
    TRUE ~ "Intermediate"
  ))

# combine temp data with sunniness estimate -------------------------------
# use data with average values per day (climate_23_plot)
# filter same timeframe as sunniness (peak season)
# rename date column to match with sunniness
climate_23_filt <- climate_23_plot |> 
  filter(format(date_time, "%Y-%m-%d") >= ymd("2023-06-15"), format(date_time, "%Y-%m-%d") <= "2023-09-15") |> 
  rename(date = date_time)

climate_23_sun <- left_join(climate_23_filt, sunniness, 
                            by = c("date", "site")) 

# filter only hi site
climate_23_sun_hi <- climate_23_sun |> 
  filter(site == "hi")
  
# plot air temp
ggplot(climate_23_sun_hi, aes(x = date, y = AirTemp, colour = sun_status)) +
  geom_line()+
  labs(title = "Daily temperature 2m 2023", y = "Temperature (°C)", x = "Date")+
  scale_color_manual(values = c("Sunny" = "orange", "Cloudy" = "turquoise",
                                "Intermediate" = "grey88"))

# at least it makes sense that sunny days are warmer than cloudy days
# now this needs to be combined with the tomst logger data

# plot humidity
ggplot(climate_23_sun_hi, aes(x = date, y = Humidity, colour = sun_status)) +
  geom_line()+
  labs(title = "Humidity 2m 2023", y = "Temperature (°C)", x = "Date")+
  scale_color_manual(values = c("Sunny" = "orange", "Cloudy" = "turquoise",
                                "Intermediate" = "grey88"))










