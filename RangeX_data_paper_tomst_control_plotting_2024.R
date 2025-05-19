
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
  geom_line() +
  theme(legend.position = "right")


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

# can we just delete a few points when the rest looks good

tomst_flagged <- tomst_24_raw_filtered |>
  arrange(tomst, date_time) |>  # important for time order
  group_by(tomst) |>  # do per logger
  mutate(
    diff_forward = abs(TMS_T1 - lead(TMS_T1)),
    temp_outlier = ifelse(diff_forward > 4, TRUE, FALSE)  # set threshold here (e.g. 2°C)
  ) |> 
  ungroup()


ggplot(tomst_flagged, aes(x = date_time, y = TMS_T1, color = temp_outlier)) +
  geom_point()


# in ground later
# filtered later start in cleaning script
# 94205708, 94205709, 94205722
ggplot(tomst_24_raw_filtered, 
       aes(x = date_time, y = TMS_T1)) +
  geom_point(data = subset(tomst_24_raw_filtered, tomst == 94205708))

ggplot(tomst_24_raw_filtered, 
       aes(x = date_time, y = TMS_T1)) +
  geom_point(data = subset(tomst_24_raw_filtered, tomst == 94205709))

ggplot(tomst_24_raw_filtered, 
       aes(x = date_time, y = TMS_T1)) +
  geom_point(data = subset(tomst_24_raw_filtered, tomst == 94205722))


ggplot(tomst_24_raw_filtered, 
       aes(x = date_time, y = TMS_T2, color = tomst)) +
  geom_point() +
  theme(legend.position = "none")

ggplot(tomst_24_raw_filtered, 
       aes(x = date_time, y = TMS_T3, color = tomst)) +
  geom_point() +
  theme(legend.position = "none")

# daily temp timeline competition ----------------------------------------
# include competition
temp_daily_comp <- tomst_24_raw_filtered |> 
  filter(site == "hi") |>
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
               values_to = "temperature") |> 
  mutate(measurement_position = factor(measurement_position,
                                       levels = c("air", "surface", "soil")))


timeline_warming <- ggplot(temp_daily_comp, aes(x = date_time, y = temperature, color = treat_warming)) +
  geom_line() +
  facet_grid(rows = vars(measurement_position), cols = vars(treat_competition))+
  scale_color_manual(values = c("warm" = "pink3", "ambi" = "turquoise"))+
  labs(color = "Warming treatment", 
       y = expression("Daily mean temperature ("*degree*C*")"), 
       x = "Time", title = "2024")
timeline_warming

ggsave(filename = "RangeX_tomst_timeline_warming_competition_24.png", 
       plot = timeline_warming, 
       path = "Data/Data_tomst_loggers/Graphs/", 
       width = 8, height = 6)


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

ggplot(avg_temp_day_long, aes(x = date, y = delta_temp, color = sensor)) +
  geom_line() +
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
  geom_line() +
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

delta_temp_peak_l <- ggplot(avg_temp_daily_long_24_peak, aes(x = date, y = delta_temp, color = sensor)) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Date", y = "Δ Temperature (warm - ambi)", 
       title = "Daily Temperature Difference peak season 24 (High Site)",
       color = "Sensor")
delta_temp_peak_l

ggsave(filename = "RangeX_tomst_delta_temp_peak_24_line.png", 
       plot = delta_temp_peak_l, 
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

delta_temp_viol_peak <- ggplot(avg_temp_daily_long_24_peak, aes(x = sensor, y = delta_temp, fill = sensor)) +
  geom_violin(alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "", y = "Δ Temperature (warm - ambi)", 
       title = "Daily Warming Effect peak season 24 (High Site)") 
delta_temp_viol_peak

delta_temp_viol_peak_ <- delta_temp_viol_peak + geom_boxplot(width=0.1)+
  scale_fill_manual(values = c("#999999", "#E69F00", "#56B4E9"))+
  theme(legend.position = "none")
delta_temp_viol_peak_

ggsave(filename = "RangeX_tomst_delta_temp_violin_peak_24.png", 
       plot = delta_temp_viol_peak_, 
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
  geom_line() +
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

# violin plot
delta_temp_viol_comp_peak <- ggplot(avg_temp_daily_long_comp_peak_24, aes(x = sensor, y = delta_temp, fill = sensor)) +
  geom_violin(alpha = 0.7) +
  geom_boxplot(width=0.1)+
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(vars(treat_competition))+
  labs(x = "", y = "Δ Temperature (warm - ambi)", 
       title = "Daily Warming Effect peak season competition 24 (High Site)")+
  scale_fill_manual(values = c("#999999", "#E69F00", "#56B4E9"))
delta_temp_viol_comp_peak

ggsave(filename = "RangeX_tomst_delta_temp_violoin_peak_comp_24.png", 
       plot = delta_temp_viol_comp_peak, 
       path = "Data/Data_tomst_loggers/Graphs/", 
       width = 15, height = 6)

delta_temp_viol_comp_peak + 
  stat_summary(fun.data=mean_sdl, mult=1,
               geom="pointrange", color="black")

delta_temp_viol_comp_peak +
  geom_boxplot(width=0.1, fill="white")

# try pairwise comparison of warm vs ambi first -------------------------------
avg_temp_pairwise <- tomst_24_raw_filtered |>
  filter(site == "hi") |>
  mutate(date = as.Date(date_time)) |>
  pivot_longer(cols = starts_with("TMS_T"),
               names_to = "sensor",
               values_to = "temperature") |>
  mutate(sensor = recode(sensor,
                         "TMS_T1" = "avg_temp_soil",
                         "TMS_T2" = "avg_temp_surface",
                         "TMS_T3" = "avg_temp_air")) |>
  group_by(date, block, treat_warming, treat_competition,
           sensor) |>
  summarise(mean_temp = mean(temperature, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = treat_warming, values_from = mean_temp) |>
  mutate(delta_temp = warm - ambi)

delta_summary <- avg_temp_pairwise |>
  group_by(date, sensor) |>
  summarise(mean_delta = mean(delta_temp, na.rm = TRUE),
            sd_delta = sd(delta_temp, na.rm = TRUE),
            .groups = "drop")

ggplot(delta_summary, aes(x = date, y = mean_delta, color = sensor)) +
  geom_line() +
  geom_ribbon(aes(ymin = mean_delta - sd_delta, ymax = mean_delta + sd_delta, fill = sensor), alpha = 0.2, color = NA) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Date", y = "Δ Temperature (warm - ambi)", title = "Mean Daily ΔT per Sensor (pairwise)", color = "Sensor", fill = "Sensor")

ggplot(avg_temp_pairwise, aes(x = sensor, y = delta_temp, fill = sensor)) +
  geom_violin() +
  facet_wrap(vars(treat_competition))+
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Sensor", y = "Δ Temperature (warm - ambi)", title = "Pairwise ΔT Distribution per Sensor")

ggplot(avg_temp_pairwise, aes(x = sensor, y = delta_temp, fill = sensor)) +
  geom_boxplot() +
  facet_wrap(vars(treat_competition))+
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Sensor", y = "Δ Temperature (warm - ambi)", 
       title = "Paired Daily ΔT per Sensor across Blocks")



# soil moisture -----------------------------------------------------------
soil_moisture_average <- tomst_24_raw_filtered |> 
  group_by(date_time, treat_combined) |> 
  summarize(soilmoisture = mean(TMS_moist, na.rm = TRUE),
            .groups = 'drop')
head(soil_moisture_average)

# plot average temp per treat
ggplot(soil_moisture_average, aes(x = date_time, y = soilmoisture, color = treat_combined)) +
  geom_line() +
  theme(legend.position = "right")


# daily soil moisture warming effect --------------------------------------------
soil_moisture_average_OTC <- tomst_24_raw_filtered |>
  filter(site == "hi") |> # low site has no OTCs
  group_by(date_time, treat_warming) |> 
  mutate(date_time = as.Date(date_time)) |>
  summarize(soilmoisture = mean(TMS_moist, na.rm = TRUE)
            ,.groups = 'drop')
soil_moisture_average_OTC

soil_moist_23 <- ggplot(soil_moisture_average_OTC, aes(x = date_time, y = soilmoisture, color = treat_warming)) +
  geom_line() +
  labs(color = "treat_warming")+
  labs(x = "Time", 
       title = "Mean daily soil moisture 24 (High Site)")+
  scale_color_manual(values = c("warm" = "pink2", "ambi" = "turquoise3"))
soil_moist_23

ggsave(filename = "RangeX_tomst_soil_moisture_23.png", 
       plot = soil_moist_23, 
       path = "Data/Data_tomst_loggers/Graphs/", 
       width = 8, height = 6)


# daily delta soil moisture peak season day and night -----------------------------------
start_date <- as.Date("2024-06-15")
end_date <- as.Date("2024-09-15")

# filter peak growing season
soil_moisture_average_OTC_peak <- soil_moisture_average_OTC |> 
  filter(between(date_time, left = start_date, right = end_date))

delta_soil_moisture_average_OTC_peak <- tomst_24_raw_filtered |>
  filter(site == "hi") |> # low site has no OTCs
  group_by(date_time, treat_warming, treat_competition) |> 
  mutate(date_time = as.Date(date_time)) |>
  summarize(soilmoisture = mean(TMS_moist, na.rm = TRUE)
            ,.groups = 'drop') |> 
  pivot_wider(names_from = treat_warming, values_from = soilmoisture) |>
  mutate(delta_s_moist = warm - ambi)

delta_soil_moist_peak <- ggplot(delta_soil_moisture_average_OTC_peak, aes(x = date_time, y = delta_s_moist, colour = treat_competition)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Date", y = "Δ soil moisture (warm - ambi)", 
       title = "Daily soil moisture difference peak season 24 (High Site)")
delta_soil_moist_peak

ggsave(filename = "RangeX_tomst_delta_soil_moisture_peak_24_points.png", 
       plot = delta_soil_moist_peak, 
       path = "Data/Data_tomst_loggers/Graphs/", 
       width = 8, height = 6)

delta_soil_moist_peak_box <- ggplot(delta_soil_moisture_average_OTC_peak, aes(x = treat_competition, y = delta_s_moist, colour = treat_competition)) +
  geom_boxplot() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Date", y = "Δ soil moisture (warm - ambi)", 
       title = "Daily soil moisture difference peak season 24 (High Site)")+
  theme(legend.position = "none")+
  scale_fill_manual(values = c("#999999", "#E69F00", "#56B4E9"))
delta_soil_moist_peak_box

delta_soil_moist_peak_violin <- ggplot(delta_soil_moisture_average_OTC_peak, aes(x = treat_competition, y = delta_s_moist, fill = treat_competition)) +
  geom_violin() +
  geom_boxplot(width=0.1)+
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Date", y = "Δ soil moisture (warm - ambi)", 
       title = "Daily soil moisture difference peak season 24 (High Site)")+
  theme(legend.position = "none")+
  scale_fill_manual(values = c("#999999", "#E69F00", "#56B4E9"))
delta_soil_moist_peak_violin

ggsave(filename = "RangeX_tomst_delta_soil_moisture_peak_24_comp_violin.png", 
       plot = delta_soil_moist_peak_violin, 
       path = "Data/Data_tomst_loggers/Graphs/", 
       width = 8, height = 6)




# plot data later in season ----------------------------------------------------
# to check if cutting the vegetated plots shows a difference in temp
# there seems to be a temp drop in Sep but hard to say if that is because of cutting or ecause it got colder
start_date <- as.Date("2024-09-08") # 3 loggers came in later
end_date <- as.Date("2024-09-18")

# Filter the data for the specified date range
tomst_24_raw_ <- tomst_24_raw |> 
  filter(between(date_time, left = start_date, right = end_date)) |> 
  filter(treat_competition == "vege")

ggplot(tomst_24_raw_, aes(x = date_time, y = TMS_T3, color = treat_warming)) +
  geom_point() +
  theme(legend.position = "none")


ggplot(tomst_24_raw_, aes(x = date_time, y = TMS_T2, color = tomst)) +
  geom_line() +
  theme(legend.position = "none")


ggplot(tomst_24_raw_, aes(x = date_time, y = TMS_T1, color = tomst)) +
  geom_point() +
  theme(legend.position = "none")








