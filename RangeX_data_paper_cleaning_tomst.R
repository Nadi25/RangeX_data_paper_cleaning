
# Climate data TOMST loggers NOR --------------------------------------------

## Data used: Data/Data_tomst_loggers/tomst_2023/,
##            tomst_plot_codes_2023.csv,
##            Sunrise_sundown_Voss_2023.csv
## Date:      03.03.2025
## Author:    Nadine Arzt
## Purpose:   Clean TOMST logger data 2023

# load library ------------------------------------------------------------
library(conflicted)
conflict_prefer_all("dplyr", quiet = TRUE)
library(tidyverse)
library(openxlsx)
library(janitor)
library(lubridate)
library(ggplot2)

# comments ----------------------------------------------------------------
# temp1 is in soil, temp2 at 0cm and temp3 20cm above ground

# 2023: high out: "08.06.2023" and "20.06.2023" - "23.10.2023"
# 2023: low out: "12.05.2023" and "19.06.2023" - "24.10.2023"
# so take the later one each?

# deleted tomst 94201723 and 94217314 because of impossible values

# calculate max, min and daily amplitude
# calculate rolling average with rollmean()

# calculate soil moisture: https://github.com/audhalbritter/Three-D/blob/master/R/functions/soilmoisture_correction.R


# import data 2023 --------------------------------------------------------
# List all files in the 'Data_tomst_loggers' folder that start with 'data'
tomst_23 <- list.files(path = "Data/Data_tomst_loggers/tomst_2023/", pattern = "^data_\\d+.*\\.csv$", full.names = TRUE)

# test to see structure of files
test_file <- read_delim(tomst_23[1], delim = ";", skip = 1)
head(test_file)
# has , 

# define colnames as header
column_names <- c("number", "Date", "Column1", "Temp1", "Temp2", "Temp3", "Soilmoisture", "Column6", "Column7")

# define coltypes to have the values correct later
column_types <- cols(
  number = col_double(),
  Date = col_character(),   # Read as character first, convert to datetime later
  Column1 = col_double(),
  Temp1 = col_character(),  # Read as character to handle commas, convert later
  Temp2 = col_character(),
  Temp3 = col_character(),
  Soilmoisture = col_double(),
  Column6 = col_double(),
  Column7 = col_double()
)

# function to extract tomst logger number
extract_number <- function(file) {
  str_extract(basename(file), "\\d+")
}

# function to read a bunch of files at the same time
read_tomst_file <- function(file) {
  read_delim(file, delim = ";", skip = 1, col_names = column_names, col_types = column_types, 
             locale = locale(decimal_mark = ","), show_col_types = FALSE) |>
    mutate(Date = dmy_hms(Date),  # Convert Date column to datetime
      across(c(Temp1, Temp2, Temp3), ~ as.numeric(str_replace(.x, ",", "."))), # Convert temps to numeric
      tomst = extract_number(file)  # Add the tomst logger number
    )
}

# get one dataframe with data from all files using the list of files (tomst_23) with a loop
tomst_data_23 <- map_dfr(tomst_23, read_tomst_file)
head(tomst_data_23)

# get plot codes 23 ------------------------------------------------------
plot_codes_23 <- read.csv2("Data/Data_tomst_loggers/tomst_plot_codes_2023.csv", header =  FALSE)
plot_codes_23

# split dataset into low and high 
# high
plot_high <- plot_codes_23 |> 
  select(V1:V5)
# low
plot_low <- plot_codes_23 |> 
  select(V7:V11)

plot_low <- plot_low |> 
  filter(if_any(everything(), ~ !is.na(.) & . != ""))

# row 2 as header
colnames(plot_high) <- plot_high[2, ]
plot_high <- plot_high[c(-1,-2), ]

colnames(plot_low) <- plot_low[2, ]
plot_low <- plot_low[c(-1,-2), ]

# add column with site
plot_high <- plot_high |> 
  mutate(site = "high")

plot_low <- plot_low |> 
  mutate(site = "low")

# combine again under each other
plot_codes_clean <- bind_rows(plot_high, plot_low)
head(plot_codes_clean)
str(plot_codes_clean) # tomst = chr 

# combine tomst data with plot labels -------------------------------------
tomst_23_raw <- left_join(plot_codes_clean, tomst_data_23, by = "tomst")
head(tomst_23_raw)


# Add treat_warming and treat_competition columns based on treat ----------
tomst_23_raw <- tomst_23_raw |> 
  mutate(
    treat_warming = case_when(
      site == "high" & treat %in% c("A", "C", "E") ~ "warm",
      site == "high" & treat %in% c("B", "D", "F") ~ "ambi",
      site == "low" & treat %in% c("A", "B") ~ "ambi",  
      TRUE ~ NA_character_  # Assign NA to unexpected cases
    ),
    treat_competition = case_when(
      site == "high" & treat %in% c("A", "B") ~ "vege",
      site == "high" & treat %in% c("C", "D") ~ "control",
      site == "high" & treat %in% c("E", "F") ~ "bare",
      site == "low" & treat == "A" ~ "vege",  
      site == "low" & treat == "B" ~ "bare",
      TRUE ~ NA_character_
    )
  )

# delete empty columns -----------------------------------------------------
tomst_23_raw <- tomst_23_raw |> 
  select(where(~ !all(is.na(.))))

# delete tomst 94201723  --------------------------------------------------
tomst_23_raw <- tomst_23_raw |> 
  filter(tomst != "94201723")

# delete 94217314 ---------------------------------------------------------
tomst_23_raw <- tomst_23_raw |> 
  filter(tomst != "94217314")


# rename soil moisture ----------------------------------------------------
tomst_23_raw <- tomst_23_raw |> 
  rename(Soilmoisture_raw = Soilmoisture)


# calculate soil moisture -----------------------------------------------------------
# function to calculate soil moisture from raw values
# https://github.com/audhalbritter/Three-D/blob/master/R/functions/soilmoisture_correction.R
soil.moist <- function(rawsoilmoist, soil_temp, soilclass){
  
  # creating df with parameters for each soil type
  soilclass.df <- tibble(
    soil = c("sand", "loamy_sand_A", "loamy_sand_B", "sandy_loam_A", "sandy_loam_B", "loam", "silt_loam", "peat"),
    a = c(-3E-9, -1.9e-8, -2.3e-8, -3.8e-8, -9e-10, -5.1e-8, 1.7e-8, 1.23e-7),
    b = c(1.61192e-4, 2.6561e-4, 2.82473e-4, 3.39449e-4, 2.61847e-4, 3.97984e-4, 1.18119e-4, 1.44644e-4),
    c = c(-0.109956505, -0.154089291, -0.167211156, -0.214921782, -0.158618303, 0.291046437, -0.101168511, 0.202927906),
    AirCalib = rep(57.64530756, 8), # a constant across all soil types, don't know exactly what this does
    AirPuls = rep(56.88867311, 8), # a constant across all soil types, don't know exactly what this does
    DilVol = rep(-59.72975311, 8) # a constant across all soil types, don't know exactly what this does
  )
  
  #filtering soilclass.df based on which soilclass was entered in the function
  soilclass.df <- soilclass.df %>%
    filter(
      soil == soilclass
    )
  
  #calculating the volumetric soil moisture with the parameters corresponding to the soil class and the raw soil moisture from the logger
  volmoist = (soilclass.df$a * rawsoilmoist^2) + (soilclass.df$b * rawsoilmoist) + soilclass.df$c
  
  #temperature correction
  temp_ref <- 24
  delta_air <- 1.91132689118083
  delta_water <- 0.64108
  delta_dil <- -1.270246891 # this is delta-water - delta_air
  # we don't know what this does or what the variables do, but the result is the same as in excel
  temp_corr <- rawsoilmoist + ((temp_ref-soil_temp) * (delta_air + delta_dil * volmoist))
  # volumetric soil moisture with temperatue correction
  volmoistcorr <- with(soilclass.df,
                       ifelse(rawsoilmoist>AirCalib,
                              (temp_corr+AirPuls+DilVol*volmoist)^2*a+(temp_corr+AirPuls+DilVol*volmoist)*b+c,
                              NA))
  return(volmoistcorr)
  # return(volmoist) #let's just use the soil moisture without temperature correction for now
}

# apply the soil moisture function
tomst_23_raw <- tomst_23_raw |> 
  mutate(TMS_moist = soil.moist(rawsoilmoist = Soilmoisture_raw, 
                                    soil_temp = Temp1, 
                                    soilclass ="silt_loam"))


# combined treatment column -----------------------------------------------
tomst_23_raw <- tomst_23_raw |> 
  mutate(treat_combined = paste(site, treat_warming, treat_competition, sep = "_"))



# filter field season already here ----------------------------------------
#high site
# some were in already on 08.06 but others only 20.06, so decided to take later date for all 
# and take away one day for handling
# low site
# 20.06 but rather do 21.06 then to match with high site
start_date <- as.Date("2023-06-21") 
end_date <- as.Date("2023-10-23") # were collected on 24.10

# Filter the data for the specified date range
tomst_23_raw_filtered <- tomst_23_raw |> 
  filter(Date >= start_date & Date <= end_date)





# plot soil moisture ------------------------------------------------------
# calculate average per date per treatment
# it's still every 15 min
tomst_23_raw_average <- tomst_23_raw_filtered |> 
  group_by(Date, treat_combined) |> 
  summarize(avg_soil_moist = mean(TMS_moist, na.rm = TRUE),,.groups = 'drop')
head(tomst_23_raw_average)

# plot all treatments
ggplot(tomst_23_raw_average, aes(x = Date, y = avg_soil_moist, color = treat_combined)) +
  geom_line() +
  theme_minimal() +
  theme(legend.position = "right")

# it doesn't look like there is a drying effect of the OTCs here
# warm has higher soil moist values
# less transpiration due to OTCs?

# get temperature data ----------------------------------------------------
# Extract temperature columns into a new data frame
temperature <- tomst_23_raw |> 
  select(tomst, date_out, Date, Temp1, Temp2, Temp3, block, treat, treat_warming, treat_competition, site) 

# View the first few rows of the extracted temperature data
head(temperature)
summary(temperature)

# fix date
# temperature <- temperature |> 
#   mutate(Date = as.POSIXct(Date, format = "%d.%m.%Y %H:%M:%S"))

temperature <- temperature |> 
  mutate(date_out = as.Date(date_out, format = "%d.%m.%Y"))


# split low and high site -------------------------------------------------
temp_high <- temperature |> 
  filter(site == "high") 

temp_low <- temperature |> 
  filter(site == "low") 

# temp high: -----------------------------------------
# define time period field season -----------------------------------------
start_date <- as.Date("2023-06-21") 
# some were in already on 08.06 but others only 20.06, so decided to take later date for all 
# and take away one day for handling
end_date <- as.Date("2023-10-23") # were collected on 24.10

# Filter the data for the specified date range
filtered_temp_high <- temp_high |> 
  filter(Date >= start_date & Date <= end_date)

head(filtered_temp_high)

# control plotting --------------------------------------------------------
# temp1 all loggers -------------------------------------------------------
# Create the plot for Temp1 per logger with the filtered data
ggplot(filtered_temp_high, aes(x = Date, y = Temp1, color = tomst)) +
  geom_point() +
  theme_minimal() +
  theme(legend.position = "right")

# one logger seems very off - find out which
# 94201723 has impossible temp1 values --> delete above in tomst_23_raw

# temp2 -------------------------------------------------------------------
ggplot(filtered_temp_high, aes(x = Date, y = Temp2, color = tomst)) +
  geom_point() +
  theme_minimal() +
  theme(legend.position = "right")

# temp3 -------------------------------------------------------------------
ggplot(filtered_temp_high, aes(x = Date, y = Temp3, color = tomst)) +
  geom_point() +
  theme_minimal() +
  theme(legend.position = "none")

ggplot(filtered_temp_high, aes(x = Date, y = Temp3, color = tomst)) +
  geom_line() +
  theme_minimal() +
  theme(legend.position = "none")


# Group by treatment wamr, ambi and calculate average temperature --------------------
temp_high_average <- filtered_temp_high |> 
  group_by(Date, treat) |> 
  summarize(avg_temp_1 = mean(Temp1, na.rm = TRUE),
            avg_temp_2 = mean(Temp2, na.rm = TRUE),
            avg_temp_3 = mean(Temp3, na.rm = TRUE),.groups = 'drop')
head(temp_high_average)

# plot average temp per treat
ggplot(temp_high_average, aes(x = Date, y = avg_temp_1, color = treat)) +
  geom_line() +
  theme_minimal() +
  theme(legend.position = "right")

ggplot(temp_high_average) +
  geom_line(aes(x = Date, y = avg_temp_1, color = treat)) +
  geom_line(aes(x = Date, y = avg_temp_2, color = treat)) +
  theme_minimal() +
  theme(legend.position = "right")


# average per temp --------------------------------------------------------
temp_high_avg <- filtered_temp_high |> 
  group_by(Date) |> 
  summarize(avg_temp_soil = mean(Temp1, na.rm = TRUE),
            avg_temp_ground = mean(Temp2, na.rm = TRUE),
            avg_temp_air = mean(Temp3, na.rm = TRUE),.groups = 'drop')
head(temp_high_avg)

# pivot longer the data
temp_high_avg_long <- temp_high_avg |> 
  pivot_longer(cols = starts_with("avg_temp"), 
               names_to = "measurement_position", 
               values_to = "temperature")

ggplot(temp_high_avg_long, aes(x = Date, y = temperature, color = measurement_position)) +
  geom_line() +
  theme_minimal() +
  labs(color = "measurement_position") 

# ok, it seems like it makes sense that temp1 - soil has the least variation
# more buffering effects than in the air

# distribution as histogram -----------------------------------------------
ggplot(temp_high_avg_long, aes( x = temperature)) +
  geom_histogram() +
  theme_minimal()

# q-q plot
ggplot(temp_high_avg_OTC_long, aes(sample = temperature, color = treat_warming)) +
  stat_qq() +
  stat_qq_line() +
  theme_minimal()

# shapiro wil test
sample_data <- sample(temp_high_avg_OTC_long$temperature, 5000) 
shapiro.test(sample_data)

# Kolmogorov-Smirnov Test
ks.test(temp_high_avg_OTC_long$temperature, "pnorm", 
        mean(temp_high_avg_OTC_long$temperature), 
        sd(temp_high_avg_OTC_long$temperature))

# so data in hist looks normally distributed but based on the tests it is not

# now test if OTCs work ---------------------------------------------------
temp_high_avg_OTC <- filtered_temp_high |> 
  group_by(Date, treat_warming) |> 
  summarize(avg_temp_soil = mean(Temp1, na.rm = TRUE),
            avg_temp_ground = mean(Temp2, na.rm = TRUE),
            avg_temp_air = mean(Temp3, na.rm = TRUE),.groups = 'drop')
head(temp_high_avg_OTC)

temp_high_avg_OTC_long <- temp_high_avg_OTC |> 
  pivot_longer(cols = starts_with("avg_temp"), 
               names_to = "measurement_position", 
               values_to = "temperature")

ggplot(temp_high_avg_OTC_long, aes(x = Date, y = temperature, color = treat_warming)) +
  geom_line() +
  theme_minimal() +
  labs(color = "treat_warming") 

# one plot per temp position
ggplot(temp_high_avg_OTC_long, aes(x = Date, y = temperature, color = treat_warming)) +
  geom_line() +
  facet_wrap(~ measurement_position, scales = "free_y") + 
  theme_minimal() +
  labs(color = "Warming Treatment")



# daily temp --------------------------------------------------------------
# calculate a mean per day and treat_warming
temp_daily <- filtered_temp_high |> 
  mutate(Date = as.Date(Date)) |> # only keeps day, not time
  group_by(Date, treat_warming) |> 
  summarize(
    avg_temp_soil = mean(Temp1, na.rm = TRUE),
    avg_temp_ground = mean(Temp2, na.rm = TRUE),
    avg_temp_air = mean(Temp3, na.rm = TRUE),
    .groups = 'drop'
  ) |> 
  pivot_longer(cols = starts_with("avg_temp"), 
               names_to = "measurement_position", 
               values_to = "temperature")


ggplot(temp_daily, aes(x = Date, y = temperature, color = treat_warming)) +
  geom_line() +
  facet_wrap(~ measurement_position, scales = "free_y") +  # Separate panels for soil, ground, air
  theme_minimal() +
  scale_color_manual(values = c("warm" = "pink3", "ambi" = "turquoise"))+
  labs(color = "Warming treatment", y = "Daily mean temperature")

# calcualte max and min temp per day as well
temp_daily_high <- filtered_temp_high |> 
  mutate(Date = as.Date(Date)) |>  # Only keeps day, not time
  group_by(Date, treat_warming) |> 
  summarize(
    avg_temp_soil = mean(Temp1, na.rm = TRUE),
    max_temp_soil = max(Temp1, na.rm = TRUE),
    min_temp_soil = min(Temp1, na.rm = TRUE),
    
    avg_temp_ground = mean(Temp2, na.rm = TRUE),
    max_temp_ground = max(Temp2, na.rm = TRUE),
    min_temp_ground = min(Temp2, na.rm = TRUE),
    
    avg_temp_air = mean(Temp3, na.rm = TRUE),
    max_temp_air = max(Temp3, na.rm = TRUE),
    min_temp_air = min(Temp3, na.rm = TRUE),
    
    .groups = 'drop'
  ) |> 
  pivot_longer(cols = -c(Date, treat_warming),  # Keep Date & treat_warming fixed
               names_to = "measurement_type", 
               values_to = "temperature")



# ribbon plot -------------------------------------------------------------
temp_ribbon <- temp_daily_high |> 
  pivot_wider(names_from = measurement_type, values_from = temperature)

ggplot(temp_ribbon, aes(x = Date)) +
  geom_ribbon(aes(ymin = min_temp_air, ymax = max_temp_air, fill = "Air"), alpha = 0.2) +
  geom_ribbon(aes(ymin = min_temp_soil, ymax = max_temp_soil, fill = "Soil"), alpha = 0.2) +
  geom_ribbon(aes(ymin = min_temp_ground, ymax = max_temp_ground, fill = "Ground"), alpha = 0.2) +
  geom_line(aes(y = avg_temp_air, color = "Air")) +
  geom_line(aes(y = avg_temp_soil, color = "Soil")) +
  geom_line(aes(y = avg_temp_ground, color = "Ground")) +
  facet_wrap(~ treat_warming) +
  labs(title = "Daily Temperature Range", y = "Temperature (°C)", x = "Date") +
  theme_minimal()


# separate day and night --------------------------------------------------
# define what is day and night in Voss in 2023
# https://dateandtime.info/citysunrisesunset.php?id=3131329&month=8&year=2023

sunrise_down <- read.csv2("Data/Data_tomst_loggers/Sunrise_sundown_Voss_2023.csv")

# fix date
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



# combine temp high long otc with sunrise data to get day and night -------
temp_high_avg_OTC_long_day_night <- temp_high_avg_OTC_long |> 
  mutate(Date_only = as.Date(Date)) |>   
  left_join(sunrise_down |> 
              select(Date, Sunrise, Sunset), 
            by = c("Date_only" = "Date")) |> 
  mutate(
    # Convert Sunrise and Sunset to full datetime
    Sunrise = ymd_hms(paste(Date_only, Sunrise)),
    Sunset = ymd_hms(paste(Date_only, Sunset)),
    
    # Define day or night
    day_night = ifelse(Date >= Sunrise & Date <= Sunset, "day", "night")
  ) |> 
  select(-Date_only)


# filter only day ---------------------------------------------------------
temp_high_avg_OTC_long_day <- temp_high_avg_OTC_long_day_night |> 
  filter(day_night == "day")


# plot only day warm ambi all 3 temp --------------------------------------
ggplot(temp_high_avg_OTC_long_day, aes(x = Date, y = temperature, color = treat_warming)) +
  geom_line() +
  facet_wrap(~ measurement_position, scales = "free_y") +  # Separate panels for soil, ground, air
  theme_minimal() +
  scale_color_manual(values = c("warm" = "pink3", "ambi" = "turquoise"))+
  labs(color = "Warming treatment", y = "Daily mean temperature")


# test for significance ---------------------------------------------------
ggplot(temp_high_avg_OTC_long_day, aes( x = temperature)) +
  geom_histogram() +
  theme_minimal()

# q-q plot
ggplot(temp_high_avg_OTC_long_day, aes(sample = temperature)) +
  stat_qq() +
  stat_qq_line() +
  theme_minimal()

# shapiro wilk test
sample_data <- sample(temp_high_avg_OTC_long_day$temperature, 5000) 
shapiro.test(sample_data)

# Kolmogorov-Smirnov Test
ks.test(temp_high_avg_OTC_long_day$temperature, "pnorm", 
        mean(temp_high_avg_OTC_long_day$temperature), 
        sd(temp_high_avg_OTC_long_day$temperature))

# this means that if the data is not normally distributed, t-test doesnt work
t_test_result <- t.test(temperature ~ treat_warming, data = temp_high_avg_OTC_long_day, var.equal = TRUE)
print(t_test_result)
# p-value = 0.01358

# instead use wilcox test
wilcox.test(temperature ~ treat_warming, data = temp_high_avg_OTC_long_day)
# p-value = 0.04174
# there is a significant effect between warm and ambi
# not very strong though

# plot it
ggplot(temp_high_avg_OTC_long_day, aes(x = treat_warming, y = temperature, fill = treat_warming)) +
  geom_boxplot() +
  labs(title = "Temperature Comparison: Warm vs. Ambi",
       x = "Treatment Group",
       y = "Temperature (°C)") +
  scale_fill_manual(values = c("warm" = "red", "ambi" = "blue")) +
  theme_bw()



# temp low: -----------------------------------------
# define time period field season -----------------------------------------
start_date_lo <- as.Date("2023-06-20") 
end_date_lo <- as.Date("2023-10-23") # were collected on 24.10

# Filter the data for the specified date range
filtered_temp_low <- temp_low |> 
  filter(Date >= start_date_lo & Date <= end_date_lo)

head(filtered_temp_low)


# control plotting --------------------------------------------------------
# temp1 all loggers -------------------------------------------------------
# Create the plot for Temp1 per logger with the filtered data
ggplot(filtered_temp_low, aes(x = Date, y = Temp1, color = tomst)) +
  geom_point() +
  theme_minimal() +
  theme(legend.position = "right")

# temp2 -------------------------------------------------------------------
ggplot(filtered_temp_low, aes(x = Date, y = Temp2, color = tomst)) +
  geom_point() +
  theme_minimal() +
  theme(legend.position = "right")

# temp3 -------------------------------------------------------------------
ggplot(filtered_temp_low, aes(x = Date, y = Temp3, color = tomst)) +
  geom_point() +
  theme_minimal() +
  theme(legend.position = "none")

ggplot(filtered_temp_low, aes(x = Date, y = Temp3, color = tomst)) +
  geom_line() +
  theme_minimal() +
  theme(legend.position = "none")


# pivot longer the data ---------------------------------------------------
temp_low_avg <- filtered_temp_low |> 
  group_by(Date, treat_combined) |> 
  summarize(avg_temp_soil = mean(Temp1, na.rm = TRUE),
            avg_temp_ground = mean(Temp2, na.rm = TRUE),
            avg_temp_air = mean(Temp3, na.rm = TRUE),.groups = 'drop')
head(temp_low_avg)

temp_low_avg_long <- temp_low_avg |> 
  pivot_longer(cols = starts_with("avg_temp"), 
               names_to = "measurement_position", 
               values_to = "temperature")

# plot
ggplot(temp_low_avg_long, aes(x = Date, y = temperature, color = measurement_position)) +
  geom_line() +
  theme_minimal() +
  labs(color = "measurement_position") 

# daily temp --------------------------------------------------------------
# calculate a mean per day and treat_competition
temp_daily_low <- filtered_temp_low |> 
  mutate(Date = as.Date(Date)) |> # only keeps day, not time
  group_by(Date, treat_combined) |> 
  summarize(
    avg_temp_soil = mean(Temp1, na.rm = TRUE),
    avg_temp_ground = mean(Temp2, na.rm = TRUE),
    avg_temp_air = mean(Temp3, na.rm = TRUE),
    .groups = 'drop'
  ) |> 
  pivot_longer(cols = starts_with("avg_temp"), 
               names_to = "measurement_position", 
               values_to = "temperature")


ggplot(temp_daily_low, aes(x = Date, y = temperature, color = treat_combined)) +
  geom_line() +
  facet_wrap(~ measurement_position, scales = "free_y") + 
  theme_minimal() +
  scale_color_manual(values = c("low_ambi_vege" = "pink4", "low_ambi_bare" = "turquoise3"))+
  labs(color = "treatment combind", y = "Daily mean temperature")


# combine filtered low and filtered high and soil moisture ------------------























