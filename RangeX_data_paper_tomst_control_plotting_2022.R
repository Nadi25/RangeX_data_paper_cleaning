# Climate data TOMST loggers NOR 2022 data exploration --------------------------------------------

## Data used: Data/Data_tomst_loggers/tomst_2022/,
##            tomst_plot_codes_2022.csv,
##            RangeX_metadata_plot_NOR.csv
## Date:      03.03.2025
## Author:    Nadine Arzt
## Purpose:   Explore TOMST logger data 2022

# load library ------------------------------------------------------------
library(conflicted)
conflict_prefer_all("dplyr", quiet = TRUE)
library(tidyverse)
#library(openxlsx)
#library(janitor)
#library(lubridate)
library(ggplot2)



# source clean functional trait data file from cleaning R script ---------------------------------
source("RangeX_data_paper_cleaning_tomst_2022.R")
head(tomst_22_clean)
names(tomst_22_clean)

ggplot(tomst_22_clean |> filter(tomst == 94201714), aes(x = date_time, y = TMS_T1)) +
  geom_point() +
  theme_minimal() +
  theme(legend.position = "none")

head(tomst_22_clean)
ggplot(tomst_22_clean |> filter(tomst == 94217346), aes(x = date_time, y = TMS_T1)) +
  geom_point() +
  theme_minimal() +
  theme(legend.position = "none")

# get temperature data for plotting ----------------------------------------------------
# Extract temperature columns into a new data frame
temperature <- tomst_22_clean |> 
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
            avg_temp_surface = mean(TMS_T2, na.rm = TRUE),
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
            avg_temp_surface = mean(TMS_T2, na.rm = TRUE),
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
    avg_temp_surface = mean(TMS_T2, na.rm = TRUE),
    avg_temp_air = mean(TMS_T3, na.rm = TRUE),
    .groups = 'drop'
  ) |> 
  pivot_longer(cols = starts_with("avg_temp"), 
               names_to = "measurement_position", 
               values_to = "temperature")


ggplot(temp_daily, aes(x = date_time, y = temperature, color = treat_warming)) +
  geom_line() +
  facet_wrap(~ measurement_position, scales = "free_y") +  # Separate panels for soil, surface, air
  theme_minimal() +
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
       x = "Time", title = "2022")
timeline_warming

ggsave(filename = "RangeX_tomst_timeline_warming_competition_22.png", 
       plot = timeline_warming, 
       path = "Data/Data_tomst_loggers/Graphs/", 
       width = 8, height = 6)



# daily temp min max ------------------------------------------------------
# calcualte max and min temp per day as well
temp_daily_high <- temp_high |> 
  mutate(date_time = as.Date(date_time)) |>  # Only keeps day, not time
  group_by(date_time, treat_warming) |> 
  summarize(
    avg_temp_soil = mean(TMS_T1, na.rm = TRUE),
    max_temp_soil = max(TMS_T1, na.rm = TRUE),
    min_temp_soil = min(TMS_T1, na.rm = TRUE),
    
    avg_temp_surface = mean(TMS_T2, na.rm = TRUE),
    max_temp_surface = max(TMS_T2, na.rm = TRUE),
    min_temp_surface = min(TMS_T2, na.rm = TRUE),
    
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
  geom_ribbon(aes(ymin = min_temp_surface, ymax = max_temp_surface, fill = "Surface"), alpha = 0.2) +
  geom_line(aes(y = avg_temp_air, color = "Air")) +
  geom_line(aes(y = avg_temp_soil, color = "Soil")) +
  geom_line(aes(y = avg_temp_surface, color = "Surface")) +
  facet_wrap(~ treat_warming) +
  labs(title = "Daily Temperature Range", y = "Temperature (°C)", x = "Date") +
  theme_minimal()


# include block as random factor ------------------------------------------
temp_daily_high_comp_block <- tomst_22_clean |> 
  filter(site == "hi") |> 
  mutate(date_time = as.Date(date_time)) |> # only keeps day, not time
  group_by(date_time, treat_competition, block_ID_original) |> 
  summarize(
    avg_temp_soil = mean(TMS_T1, na.rm = TRUE),
    avg_temp_surface = mean(TMS_T2, na.rm = TRUE),
    avg_temp_air = mean(TMS_T3, na.rm = TRUE),
    .groups = 'drop'
  ) |> 
  pivot_longer(cols = starts_with("avg_temp"), 
               names_to = "measurement_position", 
               values_to = "temperature")

temp_daily_high_comp_block <- temp_daily_high_comp_block |> 
  mutate(measurement_position = factor(measurement_position, 
                                       levels = c("avg_temp_air", 
                                                  "avg_temp_surface", 
                                                  "avg_temp_soil")))


ggplot(temp_daily_high_comp_block, aes(x = date_time, y = temperature, color = treat_competition)) +
  geom_line() +
  facet_wrap(vars(measurement_position)) +  # Separate panels for soil, ground, air
  scale_color_manual(values = c("bare" = "blue", "vege" = "green", "control" = "pink3"))+
  labs(color = "Competition treatment", y = "Daily mean temperature")

lmm_soil_comp_b <- lmerTest::lmer(temperature ~ treat_competition + (1 | date_time)+ (1 | block_ID_original), data = temp_daily_high_comp_block[temp_daily_high_comp_block$measurement_position == "avg_temp_soil", ])
summary(lmm_soil_comp_b)
# 0.07089 warmer in vege

lmm_surface_comp_b <- lme4::lmer(temperature ~ treat_competition + (1 | date_time)+ (1 | block_ID_original), data = temp_daily_high_comp_block[temp_daily_high_comp_block$measurement_position == "avg_temp_surface", ])
summary(lmm_surface_comp_b)
# 0.52498 warmer in vege

lmm_air_comp_b <- lme4::lmer(temperature ~ treat_competition + (1 | date_time)+ (1 | block_ID_original), data = temp_daily_high_comp_block[temp_daily_high_comp_block$measurement_position == "avg_temp_air", ])
summary(lmm_air_comp_b)
# 0.15186 warmer in vege

ggplot(temp_daily_high_comp_block, aes(x = treat_competition, y = temperature, fill = treat_competition)) +
  geom_boxplot() +
  facet_wrap(vars(measurement_position)) +
  labs(title = "Temperature Comparison: Vege vs. bare",
       x = "Treatment Group",
       y = "Temperature (°C)") +
  scale_fill_manual(values = c("bare" = "blue", "vege" = "green", "control" = "pink3"))



# Delta temperature: warm-ambi --------------------------------------------
# Summarise temperature per time point and treatment
avg_temp <- tomst_22_clean |> 
  filter(site == "hi") |> # use only high site
  group_by(date_time, treat_warming) |> 
  summarise(mean_temp = mean(TMS_T1, na.rm = TRUE), .groups = "drop") |> 
  pivot_wider(names_from = treat_warming, values_from = mean_temp) |> 
  mutate(delta_temp = warm - ambi)

# Plot the delta over time
ggplot(avg_temp, aes(x = date_time, y = delta_temp)) +
  geom_line(color = "red") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Time", y = "Δ Temperature (warm - ambi)", 
       title = "Temperature Difference Between Warm and Ambient Treatments")

ggplot(avg_temp, aes(x = date_time, y = delta_temp)) +
  geom_point(color = "red") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Time", y = "Δ Temperature (warm - ambi)", 
       title = "Temperature Difference Between Warm and Ambient Treatments")

# daily average
avg_temp_daily <- tomst_22_clean |>
  filter(site == "hi") |>
  mutate(date = as.Date(date_time)) |>  # extract date only
  group_by(date, treat_warming) |>
  summarise(mean_temp = mean(TMS_T1, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = treat_warming, values_from = mean_temp) |>
  mutate(delta_temp = warm - ambi)

# Plot daily delta
ggplot(avg_temp_daily, aes(x = date, y = delta_temp)) +
  geom_point(color = "red") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Date", y = "Δ Temperature (warm - ambi)", 
       title = "Daily Temperature Difference (High Site)")

# ok this is the mean per day but you should maybe filter out night


# delta temp warm-ambi day and night all positions ------------------------
avg_temp_daily_long <- tomst_22_clean |>
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

ggplot(avg_temp_daily_long, aes(x = date, y = delta_temp, color = sensor)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Date", y = "Δ Temperature (warm - ambi)", 
       title = "Daily Temperature Difference day and night (High Site)",
       color = "Sensor")

# boxplot
delta_temp_box <- ggplot(avg_temp_daily_long, aes(x = sensor, y = delta_temp, fill = sensor)) +
  geom_boxplot(alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Sensor", y = "Δ Temperature (warm - ambi)", 
       title = "Daily Warming Effect 22 (High Site)") 
delta_temp_box

ggsave(filename = "RangeX_tomst_delta_temp_box_22.png", 
       plot = delta_temp_box, 
       path = "Data/Data_tomst_loggers/Graphs/", 
       width = 8, height = 6)

# delta temp peak season --------------------------------------------------
start_date <- as.Date("2022-06-15")
end_date <- as.Date("2022-09-15")

# filter peak growing season
avg_temp_daily_long_peak <- avg_temp_daily_long |> 
  filter(between(date, left = start_date, right = end_date))

delta_temp_box_peak <- ggplot(avg_temp_daily_long_peak, aes(x = sensor, y = delta_temp, fill = sensor)) +
  geom_boxplot(alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Sensor", y = "Δ Temperature (warm - ambi)", 
       title = "Daily Warming Effect peak season 22 (High Site)") 
delta_temp_box_peak

ggsave(filename = "RangeX_tomst_delta_temp_box_peak_22.png", 
       plot = delta_temp_box_peak, 
       path = "Data/Data_tomst_loggers/Graphs/", 
       width = 8, height = 6)

# delta temperature peak day and night competition-------------------------
# keep treat competition
avg_temp_daily_long_comp_peak_22 <- tomst_22_clean |>
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

delta_temp_comp <- ggplot(avg_temp_daily_long_comp_peak_22, aes(x = date, y = delta_temp, color = sensor)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(vars(treat_competition))+
  labs(x = "Date", y = "Δ Temperature (warm - ambi)", 
       title = "Daily Temperature Difference 22 (High Site)",
       color = "Sensor")
delta_temp_comp

delta_temp_box_comp_peak <- ggplot(avg_temp_daily_long_comp_peak_22, aes(x = sensor, y = delta_temp, fill = sensor)) +
  geom_boxplot(alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(vars(treat_competition))+
  labs(x = "", y = "Δ Temperature (warm - ambi)", 
       title = "Daily Warming Effect peak season competition 22 (High Site)") 
delta_temp_box_comp_peak

ggsave(filename = "RangeX_tomst_delta_temp_box_peak_comp_22.png", 
       plot = delta_temp_box_comp_peak, 
       path = "Data/Data_tomst_loggers/Graphs/", 
       width = 15, height = 6)
# there is no control
# is that all the corrupt loggers?


# midday 8-15 day --------------------------------------------------------
# calculate means per day
# define day as 8-15 when solar radiation is strongest
tomst_midday_8_15 <- tomst_22_clean |> 
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
       title = "Daily Temperature Difference 8-15 (High Site)",
       color = "Sensor")


# boxplot delta temp ------------------------------------------------------
delta_temp_box_8_15 <- ggplot(avg_temp_day_long, aes(x = sensor, y = delta_temp, fill = sensor)) +
  geom_boxplot(alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Sensor", y = "Δ Temperature (warm - ambi)", 
       title = "Daily Warming Effect (8–15h) (High Site)")
delta_temp_box_8_15

ggsave(filename = "RangeX_tomst_delta_temp_box_8_15_22.png", 
       plot = delta_temp_box_8_15, 
       path = "Data/Data_tomst_loggers/Graphs/", 
       width = 8, height = 6)


# delta temp peak season 8-15 --------------------------------------------------
start_date <- as.Date("2022-06-15")
end_date <- as.Date("2022-09-15")

# filter peak growing season
avg_temp_day_long_peak <- avg_temp_day_long |> 
  filter(between(date, left = start_date, right = end_date))

delta_temp_box_8_15_peak <- ggplot(avg_temp_day_long_peak, aes(x = sensor, y = delta_temp, fill = sensor)) +
  geom_boxplot(alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Sensor", y = "Δ Temperature (warm - ambi)", 
       title = "Daily Warming Effect (8–15h) peak season 22 (High Site)") 
delta_temp_box_8_15_peak

ggsave(filename = "RangeX_tomst_delta_temp_box_8_15_peak_22.png", 
       plot = delta_temp_box_8_15_peak, 
       path = "Data/Data_tomst_loggers/Graphs/", 
       width = 8, height = 6)

















