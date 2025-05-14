
# Climate data TOMST loggers NOR 2023 data exploration --------------------------------------------

## Data used: Data/Data_tomst_loggers/tomst_2023/,
##            tomst_plot_codes_2023.csv,
##            RangeX_metadata_plot_NOR.csv
##            Sunrise_sundown_Voss_2023.csv
## Date:      03.03.2025
## Author:    Nadine Arzt
## Purpose:   Explore TOMST logger data 2023


# comments ----------------------------------------------------------------
# take only peak growing season (15 June- 15 September) like in Francescas bryophyte paper?
# they defined daytime 10:00-18:00

# have block as random factor 
# plot deviations (delta) or boxplots

# load library ------------------------------------------------------------
library(conflicted)
conflict_prefer_all("dplyr", quiet = TRUE)
library(tidyverse)
library(lme4)
library(lmerTest)
library(ggsignif)
library(ggpubr)

# source clean functional trait data file from cleaning R script ---------------------------------
source("RangeX_data_paper_cleaning_tomst_2023.R")
head(tomst_23_raw_filtered)
names(tomst_23_raw_filtered)

# source climate station data script for sunny and cloudy -----------------
source("RangeX_data_paper_cleaning_weather_station.R")

# plot all treatments low and high ----------------------------------------
temp_average <- tomst_23_raw_filtered |> 
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
# temp1 all loggers -------------------------------------------------------
# Create the plot for Temp1 per logger with the filtered data
ggplot(tomst_23_raw_filtered, 
       aes(x = date_time, y = TMS_T1, color = tomst)) +
  geom_point() +
  theme(legend.position = "right")

# one logger seems very off - find out which
# 94201723 has impossible temp1 values --> delete in cleaning script above in tomst_23_raw

# temp2 -------------------------------------------------------------------
ggplot(tomst_23_raw_filtered, 
       aes(x = date_time, y = TMS_T2, color = tomst)) +
  geom_point() +
  theme(legend.position = "none")

# temp3 -------------------------------------------------------------------
ggplot(tomst_23_raw_filtered, 
       aes(x = date_time, y = TMS_T3, color = tomst)) +
  geom_point() +
  theme(legend.position = "none")


# Group by treatment wamr, ambi and calculate average temperature --------------------
# have low and high site together dont split up df and have site in group by
temp_average <- tomst_23_raw_filtered |> 
  group_by(date_time, treat_combined) |> 
  summarize(avg_temp_soil = mean(TMS_T1, na.rm = TRUE),
            avg_temp_surface = mean(TMS_T2, na.rm = TRUE),
            avg_temp_air = mean(TMS_T3, na.rm = TRUE),.groups = 'drop')
temp_average

# plot average temp per treat
ggplot(temp_average, aes(x = date_time, y = avg_temp_soil, color = treat_combined)) +
  geom_line() +
  theme(legend.position = "right")

ggplot(temp_average) +
  geom_line(aes(x = date_time, y = avg_temp_surface, color = treat_combined)) +
  theme(legend.position = "right")


# pivot longer the data ------------------------------------------
temp_avg_long <- temp_average |> 
  pivot_longer(cols = starts_with("avg_temp"), 
               names_to = "measurement_position", 
               values_to = "temperature")

ggplot(temp_avg_long, 
       aes(x = date_time, y = temperature, color = measurement_position)) +
  geom_line(alpha = 0.5) +
  labs(color = "measurement_position") 

# ok, it seems like it makes sense that temp1 - soil has the least variation
# more buffering effects than in the air

# distribution as histogram -----------------------------------------------
ggplot(temp_avg_long, aes( x = temperature)) +
  geom_histogram()

# q-q plot
ggplot(temp_avg_long, aes(sample = temperature)) +
  stat_qq() +
  stat_qq_line()
# assume that temp data is normally distributed

# now test if OTCs work ---------------------------------------------------
# makes no sense to calculate that including the low site, right?
temp_avg_OTC <- tomst_23_raw_filtered |>
  filter(site == "hi") |> # low site has no OTCs
  group_by(date_time, treat_warming) |> 
  summarize(avg_temp_soil = mean(TMS_T1, na.rm = TRUE),
            avg_temp_surface = mean(TMS_T2, na.rm = TRUE),
            avg_temp_air = mean(TMS_T3, na.rm = TRUE),.groups = 'drop')
temp_avg_OTC

temp_avg_OTC_long <- temp_avg_OTC |> 
  pivot_longer(cols = starts_with("avg_temp"), 
               names_to = "measurement_position", 
               values_to = "temperature")

ggplot(temp_avg_OTC_long, aes(x = date_time, y = temperature, color = treat_warming)) +
  geom_line() +
  labs(color = "treat_warming")+
  scale_color_manual(values = c("warm" = "pink2", "ambi" = "turquoise3"))

# one plot per temp position
ggplot(temp_avg_OTC_long, aes(x = date_time, y = temperature, color = treat_warming)) +
  geom_line() +
  facet_wrap(vars(measurement_position), scales = "free_y") +
  labs(color = "Warming Treatment")+
  scale_color_manual(values = c("warm" = "pink2", "ambi" = "turquoise3"))


# lmer test ---------------------------------------------------------------
lmm_air <- lmerTest::lmer(avg_temp_air ~ treat_warming + (1 | date_time), data = temp_avg_OTC)
summary(lmm_air)
# looks like warming leads on average to 3.356e-01 degree warmer temp
# in the air (15 cm)
lmm_soil <- lmerTest::lmer(avg_temp_soil ~ treat_warming + (1 | date_time), data = temp_avg_OTC)
summary(lmm_soil)
# not in the soil: -8.656e-02 colder in OTCs
# lets check daily means

# daily temp high site ---------------------------------------------------
# calculate a mean per day and treat_warming
temp_daily_high <-  tomst_23_raw_filtered |> 
  filter(site == "hi") |> 
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

temp_daily_high <- temp_daily_high |> 
  mutate(measurement_position = factor(measurement_position, 
                                       levels = c("avg_temp_air", 
                                                  "avg_temp_surface", 
                                                  "avg_temp_soil")))


temp_day_high <- ggplot(temp_daily_high, aes(x = date_time, y = temperature, color = treat_warming)) +
  geom_line() +
  facet_wrap(vars(measurement_position)) +  # Separate panels for soil, ground, air
  scale_color_manual(values = c("warm" = "pink3", "ambi" = "turquoise"))+
  labs(color = "Warming treatment", y = "Daily mean temperature")
temp_day_high

ggsave(filename = "RangeX_temp_avg_daily_high_23.png", 
       plot = temp_day_high, 
       path = "Data/Data_tomst_loggers/Graphs/", 
       width = 10, height = 6)
#

# test for significance with lmer ---------------------------------------------------
# Soil temperature
lmm_soil_daily_hi <- lme4::lmer(temperature ~ treat_warming + (1 | date_time), data = temp_daily_high, subset = temp_daily_high$measurement_position == "avg_temp_soil")
summary(lmm_soil_daily_hi)
# warm -0.08655 colder then ambi in soil
# t-value: -3.462 so significant

lmm_soil_daily_hi <- lmerTest::lmer(temperature ~ treat_warming + (1 | date_time), data = temp_daily_high[temp_daily_high$measurement_position == "avg_temp_soil", ])
summary(lmm_soil_daily_hi)
# p-value: 0.000737 ***

# Surface temperature
lmm_surface_daily_hi <- lme4::lmer(temperature ~ treat_warming + (1 | date_time), data = temp_daily_high[temp_daily_high$measurement_position == "avg_temp_surface", ])
summary(lmm_surface_daily_hi)
# warm 0.23643 warmer at surface
# t-value: 7.417 so also significant
lmm_surface_daily_hi <- lmerTest::lmer(temperature ~ treat_warming + (1 | date_time), data = temp_daily_high[temp_daily_high$measurement_position == "avg_temp_surface", ])
summary(lmm_surface_daily_hi)
# p-value: 1.7e-11 ***

# Air temperature
lmm_air_daily_hi <- lmerTest::lmer(temperature ~ treat_warming + (1 | date_time), data = temp_daily_high[temp_daily_high$measurement_position == "avg_temp_air", ])
summary(lmm_air_daily_hi)
# warm 0.33555 warmer at surface
# t-value: 14.31 also significant
# p-value: <2e-16 ***

# could it be that OTcs have higher condenstaion --> higher moisture
# higher buffer capapcity of soil
# colder soil in OTCs?

# daily temp timeline competition ----------------------------------------
# include competition
temp_daily_comp <- tomst_23_raw_filtered |> 
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
       x = "Time", title = "2023")
timeline_warming

ggsave(filename = "RangeX_tomst_timeline_warming_competition_23.png", 
       plot = timeline_warming, 
       path = "Data/Data_tomst_loggers/Graphs/", 
       width = 8, height = 6)

# calculate max and min temp per day --------------------------------------
temp_daily_high_date <- tomst_23_raw_filtered |>
  filter(site == "hi") |> 
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
temp_ribbon <- temp_daily_high_date |> 
  pivot_wider(names_from = measurement_type, values_from = temperature)

temp_daily_min_max <- ggplot(temp_ribbon, aes(x = date_time)) +
  geom_ribbon(aes(ymin = min_temp_air, ymax = max_temp_air, fill = "Air"), alpha = 0.2) +
  geom_ribbon(aes(ymin = min_temp_soil, ymax = max_temp_soil, fill = "Soil"), alpha = 0.2) +
  geom_ribbon(aes(ymin = min_temp_surface, ymax = max_temp_surface, fill = "Surface"), alpha = 0.2) +
  geom_line(aes(y = avg_temp_air, color = "Air")) +
  geom_line(aes(y = avg_temp_soil, color = "Soil")) +
  geom_line(aes(y = avg_temp_surface, color = "Surface")) +
  facet_wrap(~ treat_warming) +
  labs(title = "Daily temperature min and max", y = "Temperature (°C)", x = "Date")
temp_daily_min_max

ggsave(filename = "RangeX_temp_daily_high_min_max_23.png", 
       plot = temp_daily_min_max, 
       path = "Data/Data_tomst_loggers/Graphs/", 
       width = 10, height = 6)

# separate day and night --------------------------------------------------
# define what is day and night in Voss in 2023
# https://dateandtime.info/citysunrisesunset.php?id=3131329&month=8&year=2023

sunrise_down <- read.csv2("Data/Data_tomst_loggers/Sunrise_sundown_Voss_2023.csv")

# fix date
# separate_wider_delim doesnt work
sunrise_down <- sunrise_down |> 
  separate(X...Date, c("Day", "Date"), sep = ",") 

sunrise_down <- sunrise_down |> 
  mutate(Date = str_trim(Date),  # Remove extra spaces
         Date = paste(Date, "2023"),  # Add year
         Date = mdy(Date))  # Convert to Date format

colnames(sunrise_down) <- sunrise_down[1, ]
sunrise_down <- sunrise_down[c(-1), ]

names(sunrise_down)

colnames(sunrise_down) <- c("day", "Date", "Sunrise", "Sunset", "Solar_Noon", "Day_Length")

head(sunrise_down)


# fix date ----------------------------------------------------------------
sunrise_down <- sunrise_down |> 
  mutate(
    # Ensure Date is in proper format
    Date = as.Date(Date, format="%Y-%m-%d"),
    
    # Convert Sunrise (AM times)
    Sunrise = str_replace(Sunrise, " a.m.", ""),
    Sunrise = parse_date_time(paste(Date, Sunrise), orders = "ymd I:M"),
    Sunrise = format(Sunrise, "%H:%M:%S"),  # Convert to HH:MM:SS format
    
    # Convert Sunset (PM times)
    Sunset = str_replace(Sunset, " p.m.", ""),
    Sunset = parse_date_time(paste(Date, Sunset), orders = "ymd I:M") + hours(12), # Add 12 hours to PM times
    Sunset = format(Sunset, "%H:%M:%S")  # Convert to HH:MM:SS format
  )
head(sunrise_down)

# combine temp high long otc with sunrise data to get day and night -------
temp_avg_OTC_long_day_night <- temp_avg_OTC_long |> 
  mutate(Date_only = as.Date(date_time)) |>   
  left_join(sunrise_down |> 
              select(Date, Sunrise, Sunset), 
            by = c("Date_only" = "Date")) |> 
  mutate(# Convert Sunrise and Sunset to full datetime
    Sunrise = ymd_hms(paste(Date_only, Sunrise)),
    Sunset = ymd_hms(paste(Date_only, Sunset)),
    
    # Define day or night
    day_night = ifelse(date_time >= Sunrise & date_time <= Sunset, "day", "night")
  ) |> 
  select(-Date_only)


# filter only day ---------------------------------------------------------
temp_avg_OTC_long_day <- temp_avg_OTC_long_day_night |> 
  filter(day_night == "day")

temp_avg_OTC_long_day <- temp_avg_OTC_long_day |> 
  mutate(measurement_position = factor(measurement_position, 
                                       levels = c("avg_temp_air", 
                                                  "avg_temp_surface", 
                                                  "avg_temp_soil")))

# plot only day warm ambi all 3 temp --------------------------------------
ggplot(temp_avg_OTC_long_day, aes(x = date_time, y = temperature, color = treat_warming)) +
  geom_line() +
  facet_wrap(vars(measurement_position)) + 
  scale_color_manual(values = c("warm" = "pink3", "ambi" = "turquoise"))+
  labs(color = "Warming treatment", y = "Daily mean temperature")


# test for significance ---------------------------------------------------
# soil
lmm_soil_day <- lmerTest::lmer(temperature ~ treat_warming + (1 | date_time), data = temp_avg_OTC_long[temp_avg_OTC_long$measurement_position == "avg_temp_soil", ])
summary(lmm_soil_day)

lmm_soil_day <- lme4::lmer(temperature ~ treat_warming + (1 | date_time), data = temp_avg_OTC_long[temp_avg_OTC_long$measurement_position == "avg_temp_soil", ])
summary(lmm_soil_day)
# -0.086558 colder in OTCs
# but how can it be basically the same as with the night together?

# surface
lmm_surface_day <- lme4::lmer(temperature ~ treat_warming + (1 | date_time), data = temp_avg_OTC_long[temp_avg_OTC_long$measurement_position == "avg_temp_surface", ])
summary(lmm_surface_day)
# 0.236429 warmer in OTCs

# air
lmm_air_day <- lme4::lmer(temperature ~ treat_warming + (1 | date_time), data = temp_avg_OTC_long[temp_avg_OTC_long$measurement_position == "avg_temp_air", ])
summary(lmm_air_day)
# 0.335553 warmer in OTC

# ok, so there is almost no difference to with including the night time
# maybe we should look at the midday (11-15)

# plot it
ggplot(temp_avg_OTC_long_day, aes(x = treat_warming, y = temperature, fill = treat_warming)) +
  geom_boxplot() +
  facet_wrap(vars(measurement_position)) +
  labs(title = "Temperature Comparison: Warm vs. Ambi",
       x = "Treatment Group",
       y = "Temperature (°C)") +
  scale_fill_manual(values = c("warm" = "pink3", "ambi" = "turquoise"))


# midday 11-15 day --------------------------------------------------------
# calculate means per day
# define day as 11-15 when solar radiation is strongest
temp_avg_OTC_long_midday <- temp_avg_OTC_long |> 
  mutate(hour = hour(date_time),  # Extract hour from datetime
         day_night = ifelse(hour >= 11 & hour <= 15, "day", "night"))

# filter only midday
temp_avg_OTC_long_midday_day <- temp_avg_OTC_long_midday |> 
  filter(day_night == "day")

temp_avg_OTC_long_midday_day <- temp_avg_OTC_long_midday_day |> 
  mutate(measurement_position = factor(measurement_position, 
                                       levels = c("avg_temp_air", 
                                                  "avg_temp_surface", 
                                                  "avg_temp_soil")))

# plot only day warm ambi all 3 temp --------------------------------------
ggplot(temp_avg_OTC_long_midday_day, aes(x = date_time, y = temperature, color = treat_warming)) +
  geom_line() +
  facet_wrap(vars(measurement_position)) + 
  scale_color_manual(values = c("warm" = "pink3", "ambi" = "turquoise"))+
  labs(color = "Warming treatment", y = "Daily mean temperature")


# test for significance ---------------------------------------------------
# soil
lmm_soil_midday <- lmerTest::lmer(temperature ~ treat_warming + (1 | date_time), data = temp_avg_OTC_long_midday_day[temp_avg_OTC_long_midday_day$measurement_position == "avg_temp_soil", ])
summary(lmm_soil_midday)
# <2e-16 ***

lmm_soil_midday <- lme4::lmer(temperature ~ treat_warming + (1 | date_time), data = temp_avg_OTC_long_midday_day[temp_avg_OTC_long_midday_day$measurement_position == "avg_temp_soil", ])
summary(lmm_soil_midday)
# -0.299585 colder in OTCs

# surface
lmm_surface_midday <- lme4::lmer(temperature ~ treat_warming + (1 | date_time), data = temp_avg_OTC_long_midday_day[temp_avg_OTC_long_midday_day$measurement_position == "avg_temp_surface", ])
summary(lmm_surface_midday)
# -0.26378 colder in OTCs???
# what??? why?
lmm_surface_midday <- lmerTest::lmer(temperature ~ treat_warming + (1 | date_time), data = temp_avg_OTC_long_midday_day[temp_avg_OTC_long_midday_day$measurement_position == "avg_temp_surface", ])
summary(lmm_surface_midday)

# air
lmm_air_midday <- lme4::lmer(temperature ~ treat_warming + (1 | date_time), data = temp_avg_OTC_long_midday_day[temp_avg_OTC_long_midday_day$measurement_position == "avg_temp_air", ])
summary(lmm_air_midday)
# 0.71119 warmer in OTC

# plot it
ggplot(temp_avg_OTC_long_midday_day, aes(x = treat_warming, y = temperature, fill = treat_warming)) +
  geom_boxplot() +
  facet_wrap(vars(measurement_position)) +
  labs(title = "Temperature Comparison: Warm vs. Ambi",
       x = "Treatment Group",
       y = "Temperature (°C)") +
  scale_fill_manual(values = c("warm" = "pink3", "ambi" = "turquoise"))

# not sure to be honest
# now the air is warmer but surface and soil are colder in the OTCs
# but with geom_signif surface is not sign, but it uses t.test



# night time --------------------------------------------------------------
# midday 11-15
# filter only night
temp_avg_OTC_long_midday_night <- temp_avg_OTC_long_midday |> 
  filter(day_night == "night")

temp_avg_OTC_long_midday_night <- temp_avg_OTC_long_midday_night |> 
  mutate(measurement_position = factor(measurement_position, 
                                       levels = c("avg_temp_air", 
                                                  "avg_temp_surface", 
                                                  "avg_temp_soil")))

# plot only day warm ambi all 3 temp --------------------------------------
ggplot(temp_avg_OTC_long_midday_night, aes(x = date_time, y = temperature, color = treat_warming)) +
  geom_line() +
  facet_wrap(vars(measurement_position)) + 
  scale_color_manual(values = c("warm" = "pink3", "ambi" = "turquoise"))+
  labs(color = "Warming treatment", y = "Daily mean temperature")


# test for significance ---------------------------------------------------
# soil
lmm_soil_night <- lmerTest::lmer(temperature ~ treat_warming + (1 | date_time), data = temp_avg_OTC_long_midday_night[temp_avg_OTC_long_midday_night$measurement_position == "avg_temp_soil", ])
summary(lmm_soil_night)

lmm_soil_night <- lme4::lmer(temperature ~ treat_warming + (1 | date_time), data = temp_avg_OTC_long_midday_night[temp_avg_OTC_long_midday_night$measurement_position == "avg_temp_soil", ])
summary(lmm_soil_night)
# -0.029751 colder in OTCs
# less

# surface
lmm_surface_night <- lme4::lmer(temperature ~ treat_warming + (1 | date_time), data = temp_avg_OTC_long_midday_night[temp_avg_OTC_long_midday_night$measurement_position == "avg_temp_surface", ])
summary(lmm_surface_night)
# 0.369818 warmer in OTCs - aha
lmm_surface_night <- lmerTest::lmer(temperature ~ treat_warming + (1 | date_time), data = temp_avg_OTC_long_midday_night[temp_avg_OTC_long_midday_night$measurement_position == "avg_temp_surface", ])
summary(lmm_surface_night)

# air
lmm_air_night <- lme4::lmer(temperature ~ treat_warming + (1 | date_time), data = temp_avg_OTC_long_midday_night[temp_avg_OTC_long_midday_night$measurement_position == "avg_temp_air", ])
summary(lmm_air_night)
# 0.235383 warmer in OTC

# plot it
ggplot(temp_avg_OTC_long_midday_night, aes(x = treat_warming, y = temperature, fill = treat_warming)) +
  geom_boxplot() +
  facet_wrap(vars(measurement_position)) +
  labs(title = "Temperature Comparison: Warm vs. Ambi",
       x = "Treatment Group",
       y = "Temperature (°C)") +
  scale_fill_manual(values = c("warm" = "pink3", "ambi" = "turquoise"))



# compare with and without competition ------------------------------------
# calculate a mean per day and treat_competition
temp_daily_high_comp <- tomst_23_raw_filtered |> 
  filter(site == "hi") |> 
  mutate(date_time = as.Date(date_time)) |> # only keeps day, not time
  group_by(date_time, treat_competition) |> 
  summarize(
    avg_temp_soil = mean(TMS_T1, na.rm = TRUE),
    avg_temp_surface = mean(TMS_T2, na.rm = TRUE),
    avg_temp_air = mean(TMS_T3, na.rm = TRUE),
    .groups = 'drop'
  ) |> 
  pivot_longer(cols = starts_with("avg_temp"), 
               names_to = "measurement_position", 
               values_to = "temperature")

temp_daily_high_comp <- temp_daily_high_comp |> 
  mutate(measurement_position = factor(measurement_position, 
                                       levels = c("avg_temp_air", 
                                                  "avg_temp_surface", 
                                                  "avg_temp_soil")))


temp_day_high_comp <- ggplot(temp_daily_high_comp, aes(x = date_time, y = temperature, color = treat_competition)) +
  geom_line() +
  facet_wrap(vars(measurement_position)) +  # Separate panels for soil, ground, air
  scale_color_manual(values = c("bare" = "blue", "vege" = "green", "control" = "pink3"))+
  labs(color = "Warming treatment", y = "Daily mean temperature")
temp_day_high_comp

ggsave(filename = "RangeX_temp_avg_daily_high_23.png", 
       plot = temp_day_high, 
       path = "Data/Data_tomst_loggers/Graphs/", 
       width = 10, height = 6)



# test for significance ---------------------------------------------------
# soil
lmm_soil_comp <- lmerTest::lmer(temperature ~ treat_competition + (1 | date_time), data = temp_daily_high_comp[temp_daily_high_comp$measurement_position == "avg_temp_soil", ])
summary(lmm_soil_comp)
# vege is 0.06 degrees warmer

# surface
lmm_surface_comp <- lme4::lmer(temperature ~ treat_competition + (1 | date_time), data = temp_daily_high_comp[temp_daily_high_comp$measurement_position == "avg_temp_surface", ])
summary(lmm_surface_comp)
# 0.5091 warmer in vege

# air
lmm_air_comp <- lme4::lmer(temperature ~ treat_competition + (1 | date_time), data = temp_daily_high_comp[temp_daily_high_comp$measurement_position == "avg_temp_air", ])
summary(lmm_air_comp)
# 0.5091 warmer in OTC
# why is it exactly the same as surface?


# plot it
ggplot(temp_daily_high_comp, aes(x = treat_competition, y = temperature, fill = treat_competition)) +
  geom_boxplot() +
  facet_wrap(vars(measurement_position)) +
  labs(title = "Temperature Comparison: Vege vs. bare",
       x = "Treatment Group",
       y = "Temperature (°C)") +
  scale_fill_manual(values = c("bare" = "blue", "vege" = "green", "control" = "pink3"))



# include block as random factor ------------------------------------------
temp_daily_high_comp_block <- tomst_23_raw_filtered |> 
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


# 24 hours plot -----------------------------------------------------------
# use all treatments and get new column with only the hour
temp_avg_long_24h <- temp_average |> 
  pivot_longer(cols = starts_with("avg_temp"), 
               names_to = "measurement_position", 
               values_to = "temperature") |> 
  mutate(hour = hour(date_time))

# Calculate average temperature for each hour across all days for each treatment
temp_hourly_avg <- temp_avg_long_24h |> 
  group_by(hour, treat_combined, measurement_position) |> 
  summarize(avg_temperature = mean(temperature, na.rm = TRUE), .groups = 'drop')

# factor to have the positions in right order
temp_hourly_avg <- temp_hourly_avg |> 
  mutate(measurement_position = factor(measurement_position, 
                                       levels = c("avg_temp_air", 
                                                  "avg_temp_surface", 
                                                  "avg_temp_soil")))
# plot 24 hours
hour24 <- ggplot(temp_hourly_avg, aes(x = hour, y = avg_temperature, color = treat_combined)) +
  geom_line() +
  facet_wrap(vars(measurement_position)) +
  labs(title = "Average temperature over 24 hours",
       x = "Hour of day",
       y = "Average Temperature (°C)",
       color = "Treatment") +
  scale_x_continuous(breaks = seq(0, 23, by = 1)) +
  theme( legend.position = "bottom")
hour24

ggsave(filename = "RangeX_temp_avg_24_hours_23.png", 
       plot = hour24, 
       path = "Data/Data_tomst_loggers/Graphs/", 
       width = 16, height = 7)

# looking at that, maybe we should use 8-15 as midday?

# midday 8-15 day --------------------------------------------------------
# calculate means per day
# define day as 8-15 when solar radiation is strongest
temp_avg_OTC_long_midday_8_15 <- temp_avg_OTC_long |> 
  mutate(hour = hour(date_time),  # Extract hour from datetime
         day_night = ifelse(hour >= 8 & hour <= 15, "day", "night"))

# filter only midday
temp_avg_OTC_long_midday_8_15 <- temp_avg_OTC_long_midday_8_15 |> 
  filter(day_night == "day")

temp_avg_OTC_long_midday_8_15 <- temp_avg_OTC_long_midday_8_15 |> 
  mutate(measurement_position = factor(measurement_position, 
                                       levels = c("avg_temp_air", 
                                                  "avg_temp_surface", 
                                                  "avg_temp_soil")))

# plot only day warm ambi all 3 temp --------------------------------------
ggplot(temp_avg_OTC_long_midday_8_15, aes(x = date_time, y = temperature, color = treat_warming)) +
  geom_line() +
  facet_wrap(vars(measurement_position)) + 
  scale_color_manual(values = c("warm" = "pink3", "ambi" = "turquoise"))+
  labs(color = "Warming treatment", y = "Daily mean temperature")


# test for significance ---------------------------------------------------
# soil
soil_midday <- lmerTest::lmer(temperature ~ treat_warming + (1 | date_time), data = temp_avg_OTC_long_midday_8_15[temp_avg_OTC_long_midday_8_15$measurement_position == "avg_temp_soil", ])
summary(soil_midday)
# <2e-16 ***
  
soil_midday <- lme4::lmer(temperature ~ treat_warming + (1 | date_time), data = temp_avg_OTC_long_midday_8_15[temp_avg_OTC_long_midday_8_15$measurement_position == "avg_temp_soil", ])
summary(soil_midday)
# -0.203043 colder in OTCs

# surface
surface_midday <- lme4::lmer(temperature ~ treat_warming + (1 | date_time), data = temp_avg_OTC_long_midday_8_15[temp_avg_OTC_long_midday_8_15$measurement_position == "avg_temp_surface", ])
summary(surface_midday)
# -0.25177 colder in OTCs

surface_midday <- lmerTest::lmer(temperature ~ treat_warming + (1 | date_time), data = temp_avg_OTC_long_midday_8_15[temp_avg_OTC_long_midday_8_15$measurement_position == "avg_temp_surface", ])
summary(surface_midday)

# air
air_midday <- lme4::lmer(temperature ~ treat_warming + (1 | date_time), data = temp_avg_OTC_long_midday_8_15[temp_avg_OTC_long_midday_8_15$measurement_position == "avg_temp_air", ])
summary(air_midday)
# 0.71599 warmer in OTC
air_midday <- lmerTest::lmer(temperature ~ treat_warming + (1 | date_time), data = temp_avg_OTC_long_midday_8_15[temp_avg_OTC_long_midday_8_15$measurement_position == "avg_temp_air", ])
summary(air_midday)
# <2e-16 ***

# plot it
ggplot(temp_avg_OTC_long_midday_8_15, aes(x = treat_warming, y = temperature, fill = treat_warming)) +
  geom_boxplot() +
  facet_wrap(vars(measurement_position)) +
  labs(title = "Temperature Comparison: Warm vs. Ambi",
       x = "Treatment Group",
       y = "Temperature (°C)") +
  scale_fill_manual(values = c("warm" = "pink3", "ambi" = "turquoise"))

# hm, that also doesn't change so much


# sunny cloudy ------------------------------------------------------------
temp_avg_OTC_long_midday_8_15
# 2023-06-21 - 2023-10-22
# is already only day so 8-15 filtered here
# but still with every 15 min data
# site is only high here

start_date <- as.Date("2023-06-15") # but it will only take it from 06-21 since we dont have earlier
end_date <- as.Date("2023-09-15")

# Summarize temperature data to get daily averages
temp_daily_avg_23 <- temp_avg_OTC_long_midday_8_15 |> 
  mutate(date = as.Date(date_time)) |> # keep only date
  group_by(date, treat_warming, measurement_position) |> 
  summarise(daily_avg_temp = mean(temperature), .groups = 'drop') 

# filter peak growing season
temp_daily_avg_23 <- temp_daily_avg_23 |> 
  filter(between(date, left = start_date, right = end_date))
#
climate_23_sun_hi
# "2023-06-20" "2023-09-15"
# only hi site

# combine temp_avg_OTC_long_midday_8_15 with sunny - cloudy catego --------
temp_daily_avg_23_sun <- left_join(temp_daily_avg_23, climate_23_sun_hi,
                               by = "date")

# How many cloudy and sunny days do we have in 23
sunny <- sum(temp_daily_avg_23_sun$sun_status == "Sunny")
sunny # 126

cloudy <- sum(temp_daily_avg_23_sun$sun_status == "Cloudy")
cloudy # 168

intermediate <- sum(temp_daily_avg_23_sun$sun_status == "Intermediate")
intermediate # 228

days_total <- length(temp_daily_avg_23_sun$sun_status)
days_total # 522

# plot facetted by sunniness and all 3 temp positions
temp_sun_cloud <- ggplot(temp_daily_avg_23_sun, aes(x = date, y = daily_avg_temp, color = treat_warming)) +
  geom_line() +
  facet_grid(measurement_position ~ sun_status) +
  labs(title = "Daily Average Temperature by Treatment and Sunniness",
       x = "Date",
       y = "Daily Average Temperature (°C)",
       color = "Treatment")+
  scale_color_manual(values = c("warm" = "pink3", "ambi" = "turquoise"))
temp_sun_cloud

ggsave(filename = "RangeX_tomst_temp_sunny_cloudy_23.png", 
       plot = temp_sun_cloud, 
       path = "Data/Data_tomst_loggers/Graphs/", 
       width = 15, height = 6)

# still, OTC only has positive effect on temp in air, 
# surface and soil temp are colder in OTC
# makes no sense!!!

# delta temp sun cloud ------------------------------
temp_delta_plot_sun <- temp_daily_avg_23_sun |>
  filter(site == "hi") |> 
  select(date, treat_warming, measurement_position, daily_avg_temp, sun_status) |>
  pivot_wider(names_from = treat_warming, values_from = daily_avg_temp) |>
  mutate(delta_temp = warm - ambi)


ggplot(temp_delta_plot_sun, aes(x = date, y = delta_temp, color = measurement_position)) +
  geom_point() +
  facet_grid(measurement_position ~ sun_status) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Date", y = "Δ Temperature (warm - ambi)", 
       title = "Daily Temperature Difference 8-15 (High Site)",
       color = "Sensor")

# boxplot delta temp sun cloud ------------------------------------------------------
sun_23_boxplot_delta_temp <- ggplot(temp_delta_plot_sun, aes(x = measurement_position, y = delta_temp, fill = measurement_position)) +
  facet_grid(~ sun_status)+
  geom_boxplot(alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "", y = "Δ Temperature (warm - ambi)", 
       title = "Daily Warming Effect (8–15h) (High Site)") 
sun_23_boxplot_delta_temp

ggsave(filename = "RangeX_tomst_delta_temp_box_sunny_cloudy_23.png", 
       plot = sun_23_boxplot_delta_temp, 
       path = "Data/Data_tomst_loggers/Graphs/", 
       width = 15, height = 6)
#

# look at soil moisture and humidity next
humidity_plot <- ggplot(temp_daily_avg_23_sun, aes(x = date, y = Humidity, color = treat_warming)) +
  geom_line() +
  facet_grid( ~ sun_status) +
  labs(y = "Average Humidity (%)") +
  scale_color_manual(values = c("warm" = "pink3", "ambi" = "turquoise"))
humidity_plot

combined_plot <- ggarrange(temp_sun_cloud, humidity_plot, ncol = 1, nrow = 2, align = "v")
combined_plot


# delta temperature day and night -------------------------------------------------------
avg_temp_daily_long_23 <- tomst_23_raw_filtered |>
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

delta_temp <- ggplot(avg_temp_daily_long_23, aes(x = date, y = delta_temp, color = sensor)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Date", y = "Δ Temperature (warm - ambi)", 
       title = "Daily Temperature Difference (High Site)",
       color = "Sensor")
delta_temp

ggsave(filename = "RangeX_tomst_delta_temp_23.png", 
       plot = delta_temp, 
       path = "Data/Data_tomst_loggers/Graphs/", 
       width = 8, height = 6)

delta_temp_box <- ggplot(avg_temp_daily_long_23, aes(x = sensor, y = delta_temp, fill = sensor)) +
  geom_boxplot(alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Sensor", y = "Δ Temperature (warm - ambi)", 
       title = "Daily Warming Effect 23 (High Site)") 
delta_temp_box

ggsave(filename = "RangeX_tomst_delta_temp_box_23.png", 
       plot = delta_temp_box, 
       path = "Data/Data_tomst_loggers/Graphs/", 
       width = 8, height = 6)

# delta temp peak season day and night -----------------------------------
start_date <- as.Date("2023-06-15")
end_date <- as.Date("2023-09-15")

# filter peak growing season
avg_temp_daily_long_23_peak <- avg_temp_daily_long_23 |> 
  filter(between(date, left = start_date, right = end_date))

delta_temp_peak <- ggplot(avg_temp_daily_long_23_peak, aes(x = date, y = delta_temp, color = sensor)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Date", y = "Δ Temperature (warm - ambi)", 
       title = "Daily Temperature Difference peak season 23 (High Site)",
       color = "Sensor")
delta_temp_peak

ggsave(filename = "RangeX_tomst_delta_temp_peak_23_points.png", 
       plot = delta_temp_peak, 
       path = "Data/Data_tomst_loggers/Graphs/", 
       width = 8, height = 6)

delta_temp_box_peak <- ggplot(avg_temp_daily_long_23_peak, aes(x = sensor, y = delta_temp, fill = sensor)) +
  geom_boxplot(alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Sensor", y = "Δ Temperature (warm - ambi)", 
       title = "Daily Warming Effect peak season 23 (High Site)") 
delta_temp_box_peak

ggsave(filename = "RangeX_tomst_delta_temp_box_peak_23.png", 
       plot = delta_temp_box_peak, 
       path = "Data/Data_tomst_loggers/Graphs/", 
       width = 8, height = 6)


# delta temperature peak day and night competition-------------------------
# keep treat competition
avg_temp_daily_long_comp_peak_23 <- tomst_23_raw_filtered |>
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

delta_temp_comp <- ggplot(avg_temp_daily_long_comp_peak_23, aes(x = date, y = delta_temp, color = sensor)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(vars(treat_competition))+
  labs(x = "Date", y = "Δ Temperature (warm - ambi)", 
       title = "Daily Temperature Difference 23 (High Site)",
       color = "Sensor")
delta_temp_comp

delta_temp_box_comp_peak <- ggplot(avg_temp_daily_long_comp_peak_23, aes(x = sensor, y = delta_temp, fill = sensor)) +
  geom_boxplot(alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(vars(treat_competition))+
  labs(x = "", y = "Δ Temperature (warm - ambi)", 
       title = "Daily Warming Effect peak season competition 23 (High Site)") 
delta_temp_box_comp_peak

ggsave(filename = "RangeX_tomst_delta_temp_box_peak_comp_23.png", 
       plot = delta_temp_box_comp_peak, 
       path = "Data/Data_tomst_loggers/Graphs/", 
       width = 15, height = 6)
# in bare ground temp is warmer in the OTCs also for surface
# in vege temp is much colder in soil indicating shading effect of vegetation


# midday 8-15 day --------------------------------------------------------
# calculate means per day
# define day as 8-15 when solar radiation is strongest
tomst_midday_8_15 <- tomst_23_raw_filtered |> 
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
       title = "Daily Warming Effect (8–15h)(High Site)") 
delta_temp_box_8_15

ggsave(filename = "RangeX_tomst_delta_temp_box_8_15_23.png", 
       plot = delta_temp_box_8_15, 
       path = "Data/Data_tomst_loggers/Graphs/", 
       width = 8, height = 6)



# delta temp peak season 8-15 --------------------------------------------------
start_date <- as.Date("2023-06-15")
end_date <- as.Date("2023-09-15")

# filter peak growing season
avg_temp_day_long_peak <- avg_temp_day_long |> 
  filter(between(date, left = start_date, right = end_date))

delta_temp_box_8_15_peak <- ggplot(avg_temp_day_long_peak, aes(x = sensor, y = delta_temp, fill = sensor)) +
  geom_boxplot(alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Sensor", y = "Δ Temperature (warm - ambi)", 
       title = "Daily Warming Effect (8–15h) peak season (High Site)") 
delta_temp_box_8_15_peak

ggsave(filename = "RangeX_tomst_delta_temp_box_8_15_peak_23.png", 
       plot = delta_temp_box_8_15_peak, 
       path = "Data/Data_tomst_loggers/Graphs/", 
       width = 8, height = 6)



# delta temp peak season day and night -----------------------------------
start_date <- as.Date("2023-06-15")
end_date <- as.Date("2023-09-15")

# filter peak growing season
avg_temp_daily_long_23_peak <- avg_temp_daily_long_23 |> 
  filter(between(date, left = start_date, right = end_date))

delta_temp_peak <- ggplot(avg_temp_daily_long_23_peak, aes(x = date, y = delta_temp, color = sensor)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Date", y = "Δ Temperature (warm - ambi)", 
       title = "Daily Temperature Difference peak season (High Site)",
       color = "Sensor")
delta_temp_peak

ggsave(filename = "RangeX_tomst_delta_temp_peak_23_points.png", 
       plot = delta_temp_peak, 
       path = "Data/Data_tomst_loggers/Graphs/", 
       width = 8, height = 6)

delta_temp_box_peak <- ggplot(avg_temp_daily_long_23_peak, aes(x = sensor, y = delta_temp, fill = sensor)) +
  geom_boxplot(alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Sensor", y = "Δ Temperature (warm - ambi)", 
       title = "Daily Warming Effect peak season 23 (High Site)") 
delta_temp_box_peak

ggsave(filename = "RangeX_tomst_delta_temp_box_peak_23.png", 
       plot = delta_temp_box_peak, 
       path = "Data/Data_tomst_loggers/Graphs/", 
       width = 8, height = 6)










