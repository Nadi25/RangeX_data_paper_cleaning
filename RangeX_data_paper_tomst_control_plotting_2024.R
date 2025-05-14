
# Climate data TOMST loggers NOR 2024 data exploration --------------------------------------------

## Data used: Data/Data_tomst_loggers/tomst_2024/,
##            tomst_plot_codes_2024.csv,
##            RangeX_metadata_plot_NOR.csv
## Date:      13.05.2025
## Author:    Nadine Arzt
## Purpose:   Explore TOMST logger data 2024

# load library ------------------------------------------------------------
library(conflicted)
conflict_prefer_all("dplyr", quiet = TRUE)
library(tidyverse)
library(lme4)
library(lmerTest)
library(ggsignif)
library(ggpubr)

# source clean functional trait data file from cleaning R script ---------------------------------
source("RangeX_data_paper_cleaning_tomst_2024.R")
head(tomst_24_raw_filtered)
names(tomst_24_raw_filtered)

# plot all treatments low and high ----------------------------------------
temp_average <- tomst_24_raw_filtered |> 
  group_by(date_time, treat_combined) |> 
  summarize(avg_temp_1 = mean(TMS_T1, na.rm = TRUE),
            avg_temp_2 = mean(TMS_T2, na.rm = TRUE),
            avg_temp_3 = mean(TMS_T3, na.rm = TRUE),.groups = 'drop')
head(temp_average)

# plot average temp per treat
ggplot(temp_average, aes(x = date_time, y = avg_temp_1, color = treat_combined)) +
  geom_line() +
  theme(legend.position = "right")

# control plotting  --------------------------------------------------------
# Create the plot for Temp1 per logger with the filtered data
ggplot(tomst_24_raw_filtered, 
       aes(x = date_time, y = TMS_T1, color = tomst)) +
  geom_point() +
  theme(legend.position = "right")

# some very high values in September
# 94217305, 94217322
# plot separately


ggplot(tomst_24_raw_filtered, 
       aes(x = date_time, y = TMS_T1, color = tomst)) +
  geom_point(data = subset(tomst_24_raw_filtered, tomst == 94217305)) +
  theme(legend.position = "right")

ggplot(tomst_24_raw_filtered, 
       aes(x = date_time, y = TMS_T1, color = tomst)) +
  geom_point(data = subset(tomst_24_raw_filtered, tomst == 94217322)) +
  theme(legend.position = "right")

# can we just delete a few points when the rest looks good?
# e.g.

ggplot(tomst_24_raw_filtered, 
       aes(x = date_time, y = TMS_T2, color = tomst)) +
  geom_point() +
  theme(legend.position = "none")

ggplot(tomst_24_raw_filtered, 
       aes(x = date_time, y = TMS_T3, color = tomst)) +
  geom_point() +
  theme(legend.position = "none")


# delta temp midday 8-15 day --------------------------------------------------------
# calculate means per day
# define day as 8-15 when solar radiation is strongest
tomst_midday_8_15 <- tomst_24_raw_filtered |> 
  mutate(hour = hour(date_time),  # Extract hour from datetime
         day_night = ifelse(hour >= 8 & hour <= 15, "day", "night"))

# filter only day 
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
       title = "Daily Temperature Difference 8-15 24 (High Site)",
       color = "Sensor")


# boxplot delta temp ------------------------------------------------------
delta_temp_box_8_15 <- ggplot(avg_temp_day_long, aes(x = sensor, y = delta_temp, fill = sensor)) +
  geom_boxplot(alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Sensor", y = "Δ Temperature (warm - ambi)", 
       title = "Daily Warming Effect (8–15h) 24(High Site)") 
delta_temp_box_8_15

ggsave(filename = "RangeX_tomst_delta_temp_box_8_15_24.png", 
       plot = delta_temp_box_8_15, 
       path = "Data/Data_tomst_loggers/Graphs/", 
       width = 8, height = 6)




# delta temp peak season 8-15 --------------------------------------------------
start_date <- as.Date("2024-06-15")
end_date <- as.Date("2024-09-15")

# filter peak growing season
avg_temp_day_long_peak <- avg_temp_day_long |> 
  filter(between(date, left = start_date, right = end_date))

delta_temp_box_8_15_peak <- ggplot(avg_temp_day_long_peak, aes(x = sensor, y = delta_temp, fill = sensor)) +
  geom_boxplot(alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Sensor", y = "Δ Temperature (warm - ambi)", 
       title = "Daily Warming Effect (8–15h) peak season 24 (High Site)") 
delta_temp_box_8_15_peak

ggsave(filename = "RangeX_tomst_delta_temp_box_8_15_peak_24.png", 
       plot = delta_temp_box_8_15_peak, 
       path = "Data/Data_tomst_loggers/Graphs/", 
       width = 8, height = 6)



# delta temp day and night ------------------------------------------------
avg_temp_daily_long_24 <- tomst_24_raw_filtered |>
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

delta_temp <- ggplot(avg_temp_daily_long_24, aes(x = date, y = delta_temp, color = sensor)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Date", y = "Δ Temperature (warm - ambi)", 
       title = "Daily Temperature Difference 24 (High Site)",
       color = "Sensor")
delta_temp

ggsave(filename = "RangeX_tomst_delta_temp_24.png", 
       plot = delta_temp, 
       path = "Data/Data_tomst_loggers/Graphs/", 
       width = 8, height = 6)

delta_temp_box <- ggplot(avg_temp_daily_long_24, aes(x = sensor, y = delta_temp, fill = sensor)) +
  geom_boxplot(alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Sensor", y = "Δ Temperature (warm - ambi)", 
       title = "Daily Warming Effect 24 (High Site)") 
delta_temp_box

ggsave(filename = "RangeX_tomst_delta_temp_box_24.png", 
       plot = delta_temp_box, 
       path = "Data/Data_tomst_loggers/Graphs/", 
       width = 8, height = 6)


# delta temp peak season day and night -----------------------------------
start_date <- as.Date("2024-06-15")
end_date <- as.Date("2024-09-15")

# filter peak growing season
avg_temp_daily_long_24_peak <- avg_temp_daily_long_24 |> 
  filter(between(date, left = start_date, right = end_date))

delta_temp_peak <- ggplot(avg_temp_daily_long_24_peak, aes(x = date, y = delta_temp, color = sensor)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Date", y = "Δ Temperature (warm - ambi)", 
       title = "Daily Temperature Difference peak season 24 (High Site)",
       color = "Sensor")
delta_temp_peak

ggsave(filename = "RangeX_tomst_delta_temp_peak_24_points.png", 
       plot = delta_temp_peak, 
       path = "Data/Data_tomst_loggers/Graphs/", 
       width = 8, height = 6)

delta_temp_box_peak <- ggplot(avg_temp_daily_long_24_peak, aes(x = sensor, y = delta_temp, fill = sensor)) +
  geom_boxplot(alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Sensor", y = "Δ Temperature (warm - ambi)", 
       title = "Daily Warming Effect peak season 24 (High Site)") 
delta_temp_box_peak

ggsave(filename = "RangeX_tomst_delta_temp_box_peak_24.png", 
       plot = delta_temp_box_peak, 
       path = "Data/Data_tomst_loggers/Graphs/", 
       width = 8, height = 6)

# delta temperature peak day and night competition-------------------------
# keep treat competition
avg_temp_daily_long_comp_peak_24 <- tomst_24_raw_filtered |>
  filter(site == "hi") |>
  mutate(date = as.Date(date_time)) |>
  pivot_longer(cols = starts_with("TMS_T"),
               names_to = "sensor",
               values_to = "temperature") |>
  mutate(sensor = recode(sensor,
                         "TMS_T1" = "avg_temp_soil",
                         "TMS_T2" = "avg_temp_surface",
                         "TMS_T3" = "avg_temp_air")) |> 
  group_by(date, treat_warming, treat_competition, sensor) |>
  summarise(mean_temp = mean(temperature, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = treat_warming, values_from = mean_temp) |>
  mutate(delta_temp = warm - ambi) |> 
  mutate(sensor = factor(sensor,
                         levels = c("avg_temp_air", "avg_temp_surface", "avg_temp_soil"))) |> 
  filter(between(date, left = start_date, right = end_date))

delta_temp_comp <- ggplot(avg_temp_daily_long_comp_peak_24, aes(x = date, y = delta_temp, color = sensor)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(vars(treat_competition))+
  labs(x = "Date", y = "Δ Temperature (warm - ambi)", 
       title = "Daily Temperature Difference 24 (High Site)",
       color = "Sensor")
delta_temp_comp

delta_temp_box_comp_peak <- ggplot(avg_temp_daily_long_comp_peak_24, aes(x = sensor, y = delta_temp, fill = sensor)) +
  geom_boxplot(alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(vars(treat_competition))+
  labs(x = "", y = "Δ Temperature (warm - ambi)", 
       title = "Daily Warming Effect peak season competition 24 (High Site)") 
delta_temp_box_comp_peak

ggsave(filename = "RangeX_tomst_delta_temp_box_peak_comp_24.png", 
       plot = delta_temp_box_comp_peak, 
       path = "Data/Data_tomst_loggers/Graphs/", 
       width = 15, height = 6)
# in bare ground temp is warmer in the OTCs also for surface
# in vege temp is much colder in soil indicating shading effect of vegetation






















