
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


# source clean functional trait data file from cleaning R script ---------------------------------
source("RangeX_data_paper_cleaning_tomst_2023.R")

head(tomst_23_raw_filtered)
names(tomst_23_raw_filtered)

# get temperature data for plotting ----------------------------------------------------
# # Extract temperature columns into a new data frame
# temperature <- tomst_23_raw_filtered |> 
#   select(tomst, date_out, date_time, TMS_T1, TMS_T2, TMS_T3, block_ID_original, plot_ID_original, 
#          treat_warming, treat_competition, treat_combined, site) 
# 
# # View the first few rows of the extracted temperature data
# head(temperature)
# summary(temperature)
# temperature <- temperature |> 
#   mutate(date_time = as.Date(date_time, format = "%d.%m.%Y"))


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

# # split low and high site -------------------------------------------------
# temp_high <- temperature |> 
#   filter(site == "hi") 
# 
# temp_low <- temperature |> 
#   filter(site == "lo") 

# control plotting high --------------------------------------------------------
# temp1 all loggers -------------------------------------------------------
# Create the plot for Temp1 per logger with the filtered data
ggplot(tomst_23_raw_filtered, 
       aes(x = date_time, y = TMS_T1, color = tomst)) +
  geom_point() +
  theme(legend.position = "right")

# one logger seems very off - find out which
# 94201723 has impossible temp1 values --> delete above in tomst_23_raw

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


# average per temp --------------------------------------------------------
# temp_high_avg <- ttomst_23_raw_filtered |> 
#   group_by(date_time) |> 
#   summarize(avg_temp_soil = mean(TMS_T1, na.rm = TRUE),
#             avg_temp_ground = mean(TMS_T2, na.rm = TRUE),
#             avg_temp_air = mean(TMS_T3, na.rm = TRUE),.groups = 'drop')
# head(temp_high_avg)

# pivot longer the data
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
temp_avg_OTC <- tomst_23_raw_filtered |> 
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



# daily temp --------------------------------------------------------------
# calculate a mean per day and treat_warming
temp_daily <- tomst_23_raw_filtered |> 
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


temp_day <- ggplot(temp_daily, aes(x = date_time, y = temperature, color = treat_warming)) +
  geom_line() +
  facet_wrap(~ measurement_position, scales = "free_y") +  # Separate panels for soil, ground, air
  scale_color_manual(values = c("warm" = "pink3", "ambi" = "turquoise"))+
  labs(color = "Warming treatment", y = "Daily mean temperature")
temp_day

ggsave(filename = "RangeX_temp_daily_23.png", 
       plot = temp_day, 
       path = "Data/Data_tomst_loggers/", 
       width = 10, height = 6)

# calcualte max and min temp per day as well
temp_daily_date <- tomst_23_raw_filtered |> 
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
temp_ribbon <- temp_daily_date |> 
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

ggsave(filename = "RangeX_temp_daily_min_max_23.png", 
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


# fix date -----------------------------------------------------------------
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

# sunrise_down <- sunrise_down |> 
#  rename(date_time = Date)

# combine temp high long otc with sunrise data to get day and night -------
temp_avg_OTC_long_day_night <- temp_avg_OTC_long |> 
  mutate(Date_only = as.Date(date_time)) |>   
  left_join(sunrise_down |> 
              select(Date, Sunrise, Sunset), 
            by = c("Date_only" = "Date")) |> 
  mutate(
    # Convert Sunrise and Sunset to full datetime
    Sunrise = ymd_hms(paste(Date_only, Sunrise)),
    Sunset = ymd_hms(paste(Date_only, Sunset)),
    
    # Define day or night
    day_night = ifelse(date_time >= Sunrise & date_time <= Sunset, "day", "night")
  ) |> 
  select(-Date_only)


# filter only day ---------------------------------------------------------
temp_avg_OTC_long_day <- temp_avg_OTC_long_day_night |> 
  filter(day_night == "day")


# plot only day warm ambi all 3 temp --------------------------------------
ggplot(temp_avg_OTC_long_day, aes(x = date_time, y = temperature, color = treat_warming)) +
  geom_line() +
  facet_wrap(~ measurement_position, scales = "free_y") +  # Separate panels for soil, ground, air
  scale_color_manual(values = c("warm" = "pink3", "ambi" = "turquoise"))+
  labs(color = "Warming treatment", y = "Daily mean temperature")


# test for significance ---------------------------------------------------
ggplot(temp_avg_OTC_long_day, aes( x = temperature)) +
  geom_histogram()

# q-q plot
ggplot(temp_avg_OTC_long_day, aes(sample = temperature)) +
  stat_qq() +
  stat_qq_line()

t_test_result <- t.test(temperature ~ treat_warming, data = temp_avg_OTC_long_day, var.equal = TRUE)
print(t_test_result)
# p-value = 1.535e-14

# instead use wilcox test
wilcox.test(temperature ~ treat_warming, data = temp_avg_OTC_long_day)
# p-value < 2.2e-16
# there is a significant effect between warm and ambi


# plot it
ggplot(temp_avg_OTC_long_day, aes(x = treat_warming, y = temperature, fill = treat_warming)) +
  geom_boxplot() +
  labs(title = "Temperature Comparison: Warm vs. Ambi",
       x = "Treatment Group",
       y = "Temperature (°C)") +
  scale_fill_manual(values = c("warm" = "red", "ambi" = "blue"))








