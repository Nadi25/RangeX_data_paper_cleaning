
# Climate data weather station NOR 2021, 2023 --------------------------------------------

## Data used: RangeX_HIGH_fall_2023.txt
##            RangeX_LOW_fall_2023.txt,
##            weather_high_2021_fall.txt
##            weather_low_2021_fall.txt
## Date:      12.03.2025
## Author:    Nadine Arzt
## Purpose:   Clean weather station data 2021, 2023


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

# should I use all days that have been recorded or shorten it to same as 
# tomst logger time frame? Kept everything for now

# 2021 --------------------------------------------------------------------
# import data hi 21 ----------------------------------------------------------
climate_hi_21 <- read_table("Data/Data_climate_station/2021/weather_high_2021_fall.txt", col_names = FALSE)

# Extract labels and units ------------------------------------------------
# use for all years
labels <- climate_hi_21[1, -1] # take first row and delete first argument
labels
# AirTemp.Avg, Humidity.Avg, WindDir.Avg, WindSpd.Avg, Radiation.Avg, Rainfall

units <- climate_hi_21[2, -1]
units
# deg C, %, deg, m.s-1, W.m-2 (watts per square meter), mm

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
  scale_color_manual(values = c("lo" = "pink3", "hi" = "turquoise"))
temp_21

ggsave(filename = "RangeX_climate_station_temp_21.png", 
       plot = temp_21, 
       path = "Data/Data_climate_station/2021/Graphs", 
       width = 10, height = 6)

# plot average Humidity per treat
humidity_21 <- ggplot(climate_21_plot, aes(x = date_time, y = Humidity, colour = site)) +
  geom_line()+
  labs(title = "Daily average humidity 21", y = "Humidity (%)", x = "Date")+
  scale_color_manual(values = c("lo" = "pink3", "hi" = "turquoise"))+
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
  scale_color_manual(values = c("lo" = "pink3", "hi" = "turquoise"))
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
  scale_fill_manual(values = c("lo" = "pink3", "hi" = "turquoise"))+
  scale_y_continuous(limits = c(0, 5.0))


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

# # filter time as for tomst loggers ----------------------------------------
# start_date <- as.Date("2023-06-21") 
# end_date <- as.Date("2023-10-23") # were collected on 24.10
# 
# # Filter the data for the specified date range
# climate_lo_23 <- climate_lo_23 |> 
#   filter(between(date_time, left = start_date, right = end_date))

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
  scale_color_manual(values = c("lo" = "pink3", "hi" = "turquoise"))
temp_23

ggsave(filename = "RangeX_climate_station_temp_23.png", 
       plot = temp_23, 
       path = "Data/Data_climate_station/2023/Graphs", 
       width = 10, height = 6)

# plot average Humidity per treat
humidity_23 <- ggplot(climate_23_plot, aes(x = date_time, y = Humidity, colour = site)) +
  geom_line()+
  labs(title = "Daily average humidity 23", y = "Humidity (%)", x = "Date")+
  scale_color_manual(values = c("lo" = "pink3", "hi" = "turquoise"))+
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
  scale_color_manual(values = c("lo" = "pink3", "hi" = "turquoise"))
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
  scale_fill_manual(values = c("lo" = "pink3", "hi" = "turquoise"))+
  scale_y_continuous(limits = c(0, 5.0))


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

# # filter time as for tomst loggers ----------------------------------------
# start_date <- as.Date("2023-06-21") 
# end_date <- as.Date("2023-10-23") # were collected on 24.10
# 
# # Filter the data for the specified date range
# climate_lo_23 <- climate_lo_23 |> 
#   filter(between(date_time, left = start_date, right = end_date))

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
  scale_color_manual(values = c("lo" = "pink3", "hi" = "turquoise"))
temp_24

ggsave(filename = "RangeX_climate_station_temp_24.png", 
       plot = temp_24, 
       path = "Data/Data_climate_station/2024/Graphs", 
       width = 10, height = 6)

# plot average Humidity per treat
humidity_24 <- ggplot(climate_24_plot, aes(x = date_time, y = Humidity, colour = site)) +
  geom_line()+
  labs(title = "Daily average humidity 24", y = "Humidity (%)", x = "Date")+
  scale_color_manual(values = c("lo" = "pink3", "hi" = "turquoise"))+
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
  scale_color_manual(values = c("lo" = "pink3", "hi" = "turquoise"))
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
  scale_fill_manual(values = c("lo" = "pink3", "hi" = "turquoise"))+
  scale_y_continuous(limits = c(0, 5.0))

# yes, low site didn't record rainfall



# arrange plots of all years together -------------------------------------
# temperature
temp <- grid.arrange(temp_21, temp_23, temp_24, ncol = 3)

ggsave(filename = "RangeX_climate_station_temperature_21_23_24.png", 
       plot = temp, 
       path = "Data/Data_climate_station/Graphs", 
       width = 15, height = 6)

# humidity
humidity <- grid.arrange(humidity_21, humidity_23, humidity_24, ncol = 3)

ggsave(filename = "RangeX_climate_station_humidity_21_23_24.png", 
       plot = humidity, 
       path = "Data/Data_climate_station/Graphs", 
       width = 15, height = 6)

# radiation
radiation <- grid.arrange(radiation_21, radiation_23, radiation_24, ncol = 3)

ggsave(filename = "RangeX_climate_station_radiation_21_23_24.png", 
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
  filter(format(date_time, "%Y-%m-%d") >= "2023-06-15" & format(date_time, "%Y-%m-%d") <= "2023-09-15") |> 
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










