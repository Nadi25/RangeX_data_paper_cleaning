
# Climate data TOMST loggers NOR 2021-2024 data exploration --------------------------------------------

## Data used: 
## Date:      14.05.2025
## Author:    Nadine Arzt
## Purpose:   Explore TOMST logger data 2021 - 2024

# load library ------------------------------------------------------------
library(conflicted)
conflict_prefer_all("dplyr", quiet = TRUE)
library(tidyverse)


# load all data -----------------------------------------------------------
source("RangeX_data_paper_cleaning_tomst_2021.R")
source("RangeX_data_paper_cleaning_tomst_2022.R")
source("RangeX_data_paper_cleaning_tomst_2023.R")
source("RangeX_data_paper_cleaning_tomst_2024.R")

head(tomst_21_raw_filtered)
head(tomst_22_clean)
head(tomst_23_raw_filtered)
head(tomst_24_raw_filtered)


# calculate daily mean comparing warm and ambi ----------------------------
temp_daily_comp_21 <- tomst_21_raw_filtered |> 
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

temp_daily_comp_22 <- tomst_22_clean |> 
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
temp_daily_comp_23 <- tomst_23_raw_filtered |> 
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
temp_daily_comp_24 <- tomst_24_raw_filtered |> 
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


# combine all 4 years -----------------------------------------------------
temp_all_years <- bind_rows(temp_daily_comp_21,
                            temp_daily_comp_22,
                            temp_daily_comp_23,
                            temp_daily_comp_24) |> 
  mutate(year = year(date_time),
         doy = yday(date_time))


# plot in separate plots --------------------------------------------------
ggplot(temp_all_years, aes(x = doy, y = temperature, color = treat_warming)) +
  geom_line() +
  facet_grid(rows = vars(measurement_position), cols = vars(treat_competition)) +
  scale_color_manual(values = c("warm" = "pink3", "ambi" = "turquoise")) +
  labs(color = "Warming treatment", 
       x = "Day of year", 
       y = expression("Daily mean temperature ("*degree*C*")"), 
       title = "Daily Temperature Patterns Across Years") +
  theme_minimal() +
  facet_wrap(~year)

#
temp_all_years <- bind_rows(temp_daily_comp_21,
                            temp_daily_comp_22,
                            temp_daily_comp_23,
                            temp_daily_comp_24) |> 
  mutate(year = as.factor(lubridate::year(date_time)),
         doy = lubridate::yday(date_time))

ggplot(temp_all_years, aes(x = doy, y = temperature, color = year)) +
  geom_line() +
  facet_grid(rows = vars(measurement_position), cols = vars(treat_competition)) +
  scale_color_brewer(palette = "Dark2") +
  labs(x = "Day of year", 
       y = expression("Daily mean temperature ("*degree*C*")"),
       color = "Year",
       title = "Seasonal Temperature Across Years (by Competition)") +
  theme_minimal()


# all years  in same plot-----------------------------------------------
temp_all_years <- bind_rows(temp_daily_comp_21,
                            temp_daily_comp_22,
                            temp_daily_comp_23,
                            temp_daily_comp_24) |> 
  mutate(measurement_position = factor(measurement_position,
                                       levels = c("air", "surface", "soil")))

ggplot(temp_all_years, aes(x = date_time, y = temperature, color = treat_warming)) +
  geom_line() +
  facet_grid(rows = vars(measurement_position), cols = vars(treat_competition)) +
  scale_color_manual(values = c("warm" = "pink3", "ambi" = "turquoise")) +
  labs(x = "Date", 
       y = expression("Daily mean temperature ("*degree*C*")"), 
       color = "Warming treatment",
       title = "Temperature 2021–2024")

# dots
ggplot(temp_all_years, aes(x = date_time, y = temperature, color = treat_warming)) +
  geom_point() +
  facet_grid(rows = vars(measurement_position), cols = vars(treat_competition)) +
  scale_color_manual(values = c("warm" = "pink3", "ambi" = "turquoise")) +
  labs(x = "Date", 
       y = expression("Daily mean temperature ("*degree*C*")"), 
       color = "Warming treatment",
       title = "Temperature 2021–2024")


# continues timeline in same plot -----------------------------------------
# Create a group ID per continuous period
temp_all_years_grouped <- temp_all_years |> 
  arrange(treat_competition, measurement_position, treat_warming, date_time) |> 
  group_by(treat_competition, measurement_position, treat_warming) |> 
  mutate(date_diff = as.numeric(difftime(date_time, lag(date_time), units = "days")),
         period_id = cumsum(if_else(is.na(date_diff) | date_diff > 7, 1, 0))) |> 
  ungroup()

timeline_21_24 <- ggplot(temp_all_years_grouped, aes(x = date_time, y = temperature, color = treat_warming, group = interaction(treat_warming, period_id))) +
  geom_line() +
  facet_grid(rows = vars(measurement_position), cols = vars(treat_competition)) +
  scale_color_manual(values = c("warm" = "pink3", "ambi" = "turquoise")) +
  labs(x = "Date", 
       y = expression("Daily mean temperature ("*degree*C*")"), 
       color = "Warming treatment",
       title = "Daily mean temperature (2021–2024, growing season)")
timeline_21_24

ggsave(filename = "RangeX_tomst_timeline_warming_competition_21-24.png", 
       plot = timeline_21_24, 
       path = "Data/Data_tomst_loggers/Graphs/", 
       width = 10, height = 6)

timeline_21_24_ <- ggplot(temp_all_years_grouped, aes(x = date_time, y = temperature, color = treat_warming, group = interaction(treat_warming, period_id))) +
  geom_line() +
  facet_grid(rows = vars(measurement_position)) +
  scale_color_manual(values = c("warm" = "pink3", "ambi" = "turquoise")) +
  labs(x = "Date", 
       y = expression("Daily mean temperature ("*degree*C*")"), 
       color = "Warming treatment",
       title = "Daily mean temperature (2021–2024, growing season)")
timeline_21_24_


# timeline all years all treat comp ---------------------------------------
temp_all_years_grouped <- temp_all_years |> 
  mutate(year = lubridate::year(date_time)) |> 
  arrange(treat_competition, measurement_position, treat_warming, date_time) |> 
  group_by(treat_competition, measurement_position, treat_warming) |> 
  mutate(date_diff = as.numeric(difftime(date_time, lag(date_time), units = "days")),
         period_id = cumsum(if_else(is.na(date_diff) | date_diff > 7, 1, 0))) |> 
  ungroup()
timeline_21_24_ <- ggplot(temp_all_years_grouped, 
                          aes(x = date_time, y = temperature, 
                              color = treat_warming, 
                              group = interaction(treat_warming, year, period_id))) +
  geom_line() +
  facet_grid(rows = vars(measurement_position)) +
  scale_color_manual(values = c("warm" = "pink3", "ambi" = "turquoise")) +
  labs(x = "Date", 
       y = expression("Daily mean temperature ("*degree*C*")"), 
       color = "Warming treatment",
       title = "Daily mean temperature (2021–2024, growing season)")
timeline_21_24_

ggsave(filename = "RangeX_tomst_timeline_warming_21-24.png", 
       plot = timeline_21_24_, 
       path = "Data/Data_tomst_loggers/Graphs/", 
       width = 10, height = 6)







