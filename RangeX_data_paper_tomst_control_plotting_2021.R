
# Climate data TOMST loggers NOR 2021 data exploration --------------------------------------------

## Data used: Data/Data_tomst_loggers/tomst_2021/,
##            tomst_plot_codes_2021.csv,
##            RangeX_metadata_plot_NOR.csv
##            Sunrise_sundown_Voss_2023.csv
## Date:      03.03.2025
## Author:    Nadine Arzt
## Purpose:   Explore TOMST logger data 2023

# load library ------------------------------------------------------------
library(conflicted)
conflict_prefer_all("dplyr", quiet = TRUE)
library(tidyverse)
#library(openxlsx)
#library(janitor)
#library(lubridate)
library(ggplot2)



# source clean functional trait data file from cleaning R script ---------------------------------
source("RangeX_data_paper_cleaning_tomst_2021.R")
head(tomst_21_raw_filtered)
names(tomst_21_raw_filtered)

ggplot(tomst_21_raw_filtered |> filter(tomst == 94201714), aes(x = date_time, y = TMS_T1)) +
  geom_point() +
  theme_minimal() +
  theme(legend.position = "none")

head(tomst_21_raw)
ggplot(tomst_21_raw |> filter(tomst == 94217317), aes(x = date_time, y = TMS_T1)) +
  geom_point() +
  theme_minimal() +
  theme(legend.position = "none")

# get temperature data for plotting ----------------------------------------------------
# Extract temperature columns into a new data frame
temperature <- tomst_21_raw_filtered |> 
  select(tomst, date_out, date_time, TMS_T1, TMS_T2, TMS_T3, block_ID_original, plot_ID_original, 
         treat_warming, treat_competition, treat_combined, site) 

# View the first few rows of the extracted temperature data
head(temperature)
summary(temperature)

# plot all treatments low and high ----------------------------------------
temp_average <- temperature |> 
  group_by(date_time, treat_combined) |> 
  summarize(avg_temp_1 = mean(TMS_T1, na.rm = TRUE),
            avg_temp_2 = mean(TMS_T2, na.rm = TRUE),
            avg_temp_3 = mean(TMS_T3, na.rm = TRUE),.groups = 'drop')
head(temp_average)

# plot average temp per treat
ggplot(temp_average, aes(x = date_time, y = avg_temp_1, color = treat_combined)) +
  geom_line() +
  theme_minimal() +
  theme(legend.position = "right")

# split low and high site -------------------------------------------------
temp_high <- temperature |> 
  filter(site == "hi") 

temp_low <- temperature |> 
  filter(site == "lo") 

# control plotting high --------------------------------------------------------
# temp1 all loggers -------------------------------------------------------
# Create the plot for Temp1 per logger with the filtered data
ggplot(temp_high, aes(x = date_time, y = TMS_T1, color = tomst)) +
  geom_point() +
  theme_minimal() +
  theme(legend.position = "right")

# temp2 -------------------------------------------------------------------
ggplot(temp_high, aes(x = date_time, y = TMS_T2, color = tomst)) +
  geom_point() +
  theme_minimal() +
  theme(legend.position = "right")

# temp3 -------------------------------------------------------------------
ggplot(temp_high, aes(x = date_time, y = TMS_T3, color = tomst)) +
  geom_point() +
  theme_minimal() +
  theme(legend.position = "none")

ggplot(temp_high, aes(x = date_time, y = TMS_T3, color = tomst)) +
  geom_line() +
  theme_minimal() +
  theme(legend.position = "none")

# Group by treatment wamr, ambi and calculate average temperature --------------------
temp_high_average <- temp_high |> 
  group_by(date_time, treat_combined) |> 
  summarize(avg_temp_1 = mean(TMS_T1, na.rm = TRUE),
            avg_temp_2 = mean(TMS_T2, na.rm = TRUE),
            avg_temp_3 = mean(TMS_T3, na.rm = TRUE),.groups = 'drop')
head(temp_high_average)

# plot average temp per treat
ggplot(temp_high_average, aes(x = date_time, y = avg_temp_1, color = treat_combined)) +
  geom_line() +
  theme_minimal() +
  theme(legend.position = "right")

ggplot(temp_high_average) +
  geom_line(aes(x = date_time, y = avg_temp_1, color = treat_combined)) +
  geom_line(aes(x = date_time, y = avg_temp_2, color = treat_combined)) +
  theme_minimal() +
  theme(legend.position = "right")


# average per temp --------------------------------------------------------
temp_high_avg <- temp_high |> 
  group_by(date_time) |> 
  summarize(avg_temp_soil = mean(TMS_T1, na.rm = TRUE),
            avg_temp_ground = mean(TMS_T2, na.rm = TRUE),
            avg_temp_air = mean(TMS_T3, na.rm = TRUE),.groups = 'drop')
head(temp_high_avg)

# pivot longer the data
temp_high_avg_long <- temp_high_avg |> 
  pivot_longer(cols = starts_with("avg_temp"), 
               names_to = "measurement_position", 
               values_to = "temperature")

ggplot(temp_high_avg_long, aes(x = date_time, y = temperature, color = measurement_position)) +
  geom_line() +
  theme_minimal() +
  labs(color = "measurement_position") 

# distribution as histogram -----------------------------------------------
ggplot(temp_high_avg_long, aes( x = temperature)) +
  geom_histogram() +
  theme_minimal()

# q-q plot
ggplot(temp_high_avg_long, aes(sample = temperature)) +
  stat_qq() +
  stat_qq_line() +
  theme_minimal()

# shapiro wil test
sample_data <- sample(temp_high_avg_long$temperature, size = 5000) 
shapiro.test(sample_data)

# Kolmogorov-Smirnov Test
ks.test(temp_high_avg_long$temperature, "pnorm", 
        mean(temp_high_avg_long$temperature), 
        sd(temp_high_avg_long$temperature))

# so data in hist looks normally distributed but based on the tests it is not

# now test if OTCs work ---------------------------------------------------
temp_high_avg_OTC <- temp_high |> 
  group_by(date_time, treat_warming) |> 
  summarize(avg_temp_soil = mean(TMS_T1, na.rm = TRUE),
            avg_temp_ground = mean(TMS_T2, na.rm = TRUE),
            avg_temp_air = mean(TMS_T3, na.rm = TRUE),.groups = 'drop')
head(temp_high_avg_OTC)

temp_high_avg_OTC_long <- temp_high_avg_OTC |> 
  pivot_longer(cols = starts_with("avg_temp"), 
               names_to = "measurement_position", 
               values_to = "temperature")

ggplot(temp_high_avg_OTC_long, aes(x = date_time, y = temperature, color = treat_warming)) +
  geom_line() +
  theme_minimal() +
  labs(color = "treat_warming")+
  scale_color_manual(values = c("warm" = "pink2", "ambi" = "turquoise3"))

# one plot per temp position
ggplot(temp_high_avg_OTC_long, aes(x = date_time, y = temperature, color = treat_warming)) +
  geom_line() +
  facet_wrap(~ measurement_position, scales = "free_y") + 
  theme_minimal() +
  labs(color = "Warming Treatment")+
  scale_color_manual(values = c("warm" = "pink2", "ambi" = "turquoise3"))

# daily temp --------------------------------------------------------------
# calculate a mean per day and treat_warming
temp_daily <- temp_high |> 
  mutate(date_time = as.Date(date_time)) |> # only keeps day, not time
  group_by(date_time, treat_warming) |> 
  summarize(
    avg_temp_soil = mean(TMS_T1, na.rm = TRUE),
    avg_temp_ground = mean(TMS_T2, na.rm = TRUE),
    avg_temp_air = mean(TMS_T3, na.rm = TRUE),
    .groups = 'drop'
  ) |> 
  pivot_longer(cols = starts_with("avg_temp"), 
               names_to = "measurement_position", 
               values_to = "temperature")


ggplot(temp_daily, aes(x = date_time, y = temperature, color = treat_warming)) +
  geom_line() +
  facet_wrap(~ measurement_position, scales = "free_y") +  # Separate panels for soil, ground, air
  #theme_minimal() +
  scale_color_manual(values = c("warm" = "pink3", "ambi" = "turquoise"))+
  labs(color = "Warming treatment", y = "Daily mean temperature")

# daily temp timeline competition ----------------------------------------
# include competition
temp_daily_comp <- temp_high |> 
  mutate(date_time = as.Date(date_time)) |> # only keeps day, not time
  group_by(date_time, treat_competition, treat_warming) |> 
  summarize(
    soil = mean(TMS_T1, na.rm = TRUE),
    surface = mean(TMS_T2, na.rm = TRUE),
    air = mean(TMS_T3, na.rm = TRUE),
    .groups = 'drop'
  ) |> 
  pivot_longer(cols = c(soil, surface, air), 
               names_to = "measurement_position", 
               values_to = "temperature")|> 
  mutate(measurement_position = factor(measurement_position,
                                       levels = c("air", "surface", "soil")))


timeline_warming <- ggplot(temp_daily_comp, aes(x = date_time, y = temperature, color = treat_warming)) +
  geom_line() +
  facet_grid(rows = vars(measurement_position), cols = vars(treat_competition))+
  scale_color_manual(values = c("warm" = "pink3", "ambi" = "turquoise"))+
  labs(color = "Warming treatment", 
       y = expression("Daily mean temperature ("*degree*C*")"), 
       x = "Time", title = "2021")
timeline_warming

ggsave(filename = "RangeX_tomst_timeline_warming_competition_21.png", 
       plot = timeline_warming, 
       path = "Data/Data_tomst_loggers/Graphs/", 
       width = 8, height = 6)

# min and max -------------------------------------------------------------
# calcualte max and min temp per day as well
temp_daily_high <- temp_high |> 
  mutate(date_time = as.Date(date_time)) |>  # Only keeps day, not time
  group_by(date_time, treat_warming) |> 
  summarize(
    avg_temp_soil = mean(TMS_T1, na.rm = TRUE),
    max_temp_soil = max(TMS_T1, na.rm = TRUE),
    min_temp_soil = min(TMS_T1, na.rm = TRUE),
    
    avg_temp_ground = mean(TMS_T2, na.rm = TRUE),
    max_temp_ground = max(TMS_T2, na.rm = TRUE),
    min_temp_ground = min(TMS_T2, na.rm = TRUE),
    
    avg_temp_air = mean(TMS_T3, na.rm = TRUE),
    max_temp_air = max(TMS_T3, na.rm = TRUE),
    min_temp_air = min(TMS_T3, na.rm = TRUE),
    
    .groups = 'drop'
  ) |> 
  pivot_longer(cols = -c(date_time, treat_warming),  # Keep Date & treat_warming fixed
               names_to = "measurement_type", 
               values_to = "temperature")



# ribbon plot -------------------------------------------------------------
temp_ribbon <- temp_daily_high |> 
  pivot_wider(names_from = measurement_type, values_from = temperature)

ggplot(temp_ribbon, aes(x = date_time)) +
  geom_ribbon(aes(ymin = min_temp_air, ymax = max_temp_air, fill = "Air"), alpha = 0.2) +
  geom_ribbon(aes(ymin = min_temp_soil, ymax = max_temp_soil, fill = "Soil"), alpha = 0.2) +
  geom_ribbon(aes(ymin = min_temp_ground, ymax = max_temp_ground, fill = "Ground"), alpha = 0.2) +
  geom_line(aes(y = avg_temp_air, color = "Air")) +
  geom_line(aes(y = avg_temp_soil, color = "Soil")) +
  geom_line(aes(y = avg_temp_ground, color = "Ground")) +
  facet_wrap(~ treat_warming) +
  labs(title = "Daily Temperature Range", y = "Temperature (°C)", x = "Date") +
  theme_minimal()



# delta temperature -------------------------------------------------------
avg_temp_daily_long_21 <- tomst_21_raw_filtered |>
  filter(site == "hi") |>
  mutate(date = as.Date(date_time)) |>
  pivot_longer(cols = starts_with("TMS_T"),
               names_to = "sensor",
               values_to = "temperature") |>
  mutate(sensor = recode(sensor,
                         "TMS_T1" = "avg_temp_soil",
                         "TMS_T2" = "avg_temp_surface",
                         "TMS_T3" = "avg_temp_air")) |> 
  group_by(date, treat_warming, sensor) |>
  summarise(mean_temp = mean(temperature, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = treat_warming, values_from = mean_temp) |>
  mutate(delta_temp = warm - ambi) |> 
  mutate(sensor = factor(sensor,
                         levels = c("avg_temp_air", "avg_temp_surface", "avg_temp_soil")))

delta_temp_points_21 <- ggplot(avg_temp_daily_long_21, aes(x = date, y = delta_temp, color = sensor)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Date", y = "Δ Temperature (warm - ambi)", 
       title = "Daily Temperature Difference sep - oct 21 (High Site)",
       color = "Sensor")
delta_temp_points_21

ggsave(filename = "RangeX_tomst_delta_temp_points_21.png", 
       plot = delta_temp_points_21, 
       path = "Data/Data_tomst_loggers/Graphs/", 
       width = 8, height = 6)

# boxplot
delta_temp_box_21 <- ggplot(avg_temp_daily_long_21, aes(x = sensor, y = delta_temp, fill = sensor)) +
  geom_boxplot(alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Sensor", y = "Δ Temperature (warm - ambi)", 
       title = "Daily Warming Effect sep - oct 21 (High Site)") 
delta_temp_box_21

ggsave(filename = "RangeX_tomst_delta_temp_box_21.png", 
       plot = delta_temp_box_21, 
       path = "Data/Data_tomst_loggers/Graphs/", 
       width = 8, height = 6)


# midday 8-15 day --------------------------------------------------------
# calculate means per day
# define day as 8-15 when solar radiation is strongest
tomst_midday_8_15 <- tomst_21_raw_filtered |> 
  mutate(hour = hour(date_time),  # Extract hour from datetime
         day_night = ifelse(hour >= 8 & hour <= 15, "day", "night"))

# filter only day ---------------------------------------------------------
tomst_midday_8_15 <- tomst_midday_8_15 |> 
  filter(day_night == "day")

avg_temp_day_long <- tomst_midday_8_15 |>
  filter(site == "hi") |>
  mutate(date = as.Date(date_time)) |>
  pivot_longer(cols = starts_with("TMS_T"),
               names_to = "sensor",
               values_to = "temperature") |>
  mutate(sensor = recode(sensor,
                         "TMS_T1" = "avg_temp_soil",
                         "TMS_T2" = "avg_temp_surface",
                         "TMS_T3" = "avg_temp_air")) |>
  group_by(date, treat_warming, sensor) |>
  summarise(mean_temp = mean(temperature, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = treat_warming, values_from = mean_temp) |>
  mutate(delta_temp = warm - ambi) |> 
  mutate(sensor = factor(sensor,
                         levels = c("avg_temp_air", "avg_temp_surface", "avg_temp_soil")))

ggplot(avg_temp_day_long, aes(x = date, y = delta_temp, color = sensor)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Date", y = "Δ Temperature (warm - ambi)", 
       title = "Daily Temperature Difference 8-15 sep - oct 21 (High Site)",
       color = "Sensor")


# boxplot delta temp------------------------------------------------------
delta_temp_box_21 <- ggplot(avg_temp_day_long, aes(x = sensor, y = delta_temp, fill = sensor)) +
  geom_boxplot(alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Sensor", y = "Δ Temperature (warm - ambi)", 
       title = "Daily Warming Effect 8-15 sep - oct 21 (High Site)") 
delta_temp_box_21

ggsave(filename = "RangeX_tomst_delta_temp_box_8_15_21.png", 
       plot = delta_temp_box_21, 
       path = "Data/Data_tomst_loggers/Graphs/", 
       width = 8, height = 6)













