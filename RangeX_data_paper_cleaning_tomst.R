
# Climate data TOMST loggers NOR --------------------------------------------

## Data used: ,
##            
## Date:      03.03.2025
## Author:    Nadine Arzt
## Purpose:   Clean TOMST logger data 

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

# deleted tomst 94201723 due to impossible values

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
      treat == "A" ~ "warm",
      treat == "B" ~ "ambi",
      treat == "C" ~ "warm",
      treat == "D" ~ "ambi",
      treat == "E" ~ "warm",
      treat == "F" ~ "ambi"
    ),
    treat_competition = case_when(
      treat == "A" ~ "vege",
      treat == "B" ~ "vege",
      treat == "C" ~ "control",
      treat == "D" ~ "control",
      treat == "E" ~ "bare",
      treat == "F" ~ "bare"
    )
  )

# delete empty columns -----------------------------------------------------
tomst_23_raw <- tomst_23_raw |> 
  select(where(~ !all(is.na(.))))

# delete tomst 94201723  --------------------------------------------------
tomst_23_raw <- tomst_23_raw |> 
  filter(tomst != "94201723")


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
start_date <- as.Date("2023-06-21") # some were in already on 08.06 but others only 20.06, so decided to take later date
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

ggplot(temp_high_avg_long, aes(x = Date, y = temperature, color = temperature_position)) +
  geom_line() +
  theme_minimal() +
  labs(color = "temperature_position") 

# ok, it seems like it makes sense that temp1 - soil has the least variation
# more buffering effects then in the air


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


sunrise_down <- sunrise_down |> 
  mutate(
    # Convert Sunrise to 24-hour format
    Sunrise = mdy_hms(paste(Date, Sunrise)),  # Combines date and time for conversion
    Sunrise = format(Sunrise, "%H:%M:%S")
  )




