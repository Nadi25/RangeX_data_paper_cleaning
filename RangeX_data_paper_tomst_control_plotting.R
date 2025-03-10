
# Climate data TOMST loggers NOR 2023 data exploration --------------------------------------------

## Data used: Data/Data_tomst_loggers/tomst_2023/,
##            tomst_plot_codes_2023.csv,
##            RangeX_metadata_plot_NOR.csv
##            Sunrise_sundown_Voss_2023.csv
## Date:      03.03.2025
## Author:    Nadine Arzt
## Purpose:   Explore TOMST logger data 2023

# load library ------------------------------------------------------------
library(conflicted)
conflict_prefer_all("dplyr", quiet = TRUE)
library(tidyverse)
library(lme4)
library(lmerTest)

# source clean functional trait data file from cleaning R script ---------------------------------
source("RangeX_data_paper_cleaning_tomst_2023.R")

head(tomst_23_raw_filtered)
names(tomst_23_raw_filtered)


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

ggplot(temp_high, aes(x = date_time, y = TMS_T3, color = tomst)) +
  geom_line() +
  theme(legend.position = "none")


# Group by treatment wamr, ambi and calculate average temperature --------------------
# have low and high site together dont split up df and have site in group by
temp_average <- tomst_23_raw_filtered |> 
  group_by(date_time, treat_combined) |> 
  summarize(avg_temp_1 = mean(TMS_T1, na.rm = TRUE),
            avg_temp_2 = mean(TMS_T2, na.rm = TRUE),
            avg_temp_3 = mean(TMS_T3, na.rm = TRUE),.groups = 'drop')
temp_average

# plot average temp per treat
ggplot(temp_average, aes(x = date_time, y = avg_temp_1, color = treat_combined)) +
  geom_line() +
  theme(legend.position = "right")

ggplot(temp_average) +
  geom_line(aes(x = date_time, y = avg_temp_2, color = treat_combined)) +
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
temp_daily_high <- tomst_23_raw_filtered |> 
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
       path = "Data/Data_tomst_loggers/", 
       width = 10, height = 6)
#

# test for significance with lmer ---------------------------------------------------
# Soil temperature
lmm_soil_daily_hi <- lme4::lmer(temperature ~ treat_warming + (1 | date_time), data = temp_daily_high[temp_daily_high$measurement_position == "avg_temp_soil", ])
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
       path = "Data/Data_tomst_loggers/", 
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


# midday 11-15 ------------------------------------------------------------
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

lmm_soil_midday <- lme4::lmer(temperature ~ treat_warming + (1 | date_time), data = temp_avg_OTC_long_midday_day[temp_avg_OTC_long_midday_day$measurement_position == "avg_temp_soil", ])
summary(lmm_soil_midday)
# -0.299585 colder in OTCs

# surface
lmm_surface_midday <- lme4::lmer(temperature ~ treat_warming + (1 | date_time), data = temp_avg_OTC_long_midday_day[temp_avg_OTC_long_midday_day$measurement_position == "avg_temp_surface", ])
summary(lmm_surface_midday)
# -0.26378 colder in OTCs???
# what??? why?

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



