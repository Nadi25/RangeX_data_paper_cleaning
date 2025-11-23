
# Climate data TOMST loggers NOR 2021 --------------------------------------------

## Data used: Data/Data_tomst_loggers/tomst_2021/,
##            tomst_plot_codes_2021.csv,
##            RangeX_metadata_plot_NOR.csv
## Date:      06.03.2025
## Author:    Nadine Arzt
## Purpose:   Clean TOMST logger data 2021

# load library ------------------------------------------------------------
library(conflicted)
conflict_prefer_all("dplyr", quiet = TRUE)
library(tidyverse)
library(janitor)

theme_set(theme_bw())
# comments ---------------------------------------------------------------
# temp1 is in soil, temp2 at 0cm and temp3 20cm above ground

# calculate soil moisture: https://github.com/audhalbritter/Three-D/blob/master/R/functions/soilmoisture_correction.R


# Source script with functions --------------------------------------------
# for extracting tomst number
# reading in multiple files
# calculating soil moisture
source("RangeX_data_paper_functions.R")

# import data 2021 ------------------------------------------------------
# List all files in the 'Data_tomst_loggers' folder that start with 'data'
tomst_21 <- list.files(path = "Data/Data_tomst_loggers/tomst_2021/", 
                       pattern = "^(high|low).*\\.csv$", 
                       full.names = TRUE)

test_file <- read_delim(tomst_21[2], delim = ";", skip = 1)
head(test_file)
# 2021.07.26 20:30 - time format deletes the seconds

# define colnames as header
column_names <- c("number", "date_time", "Column1", "TMS_T1", "TMS_T2", "TMS_T3", "Soilmoisture_raw", "Column6", "Column7")

# define coltypes to have the values correct later
column_types <- cols(
  number = col_double(),
  date_time = col_character(),   # Read as character first, convert to datetime later
  Column1 = col_double(),
  TMS_T1 = col_character(),  # Read as character to handle commas, convert later
  TMS_T2 = col_character(),
  TMS_T3 = col_character(),
  Soilmoisture_raw = col_double(),
  Column6 = col_double(),
  Column7 = col_double()
)

# get the data ----------------------------------------------------------
# get one dataframe with data from all files using the list of files (tomst_21) with a loop
tomst_data_21 <- map_dfr(tomst_21, read_tomst_file_21)
head(tomst_data_21)

# get plot codes 21 ------------------------------------------------------
plot_codes_21 <- read.csv2("Data/Data_tomst_loggers/tomst_plot_codes_2021.csv", skip = 1)
plot_codes_21

# split dataset into low and high 
# high
plot_high <- plot_codes_21 |> 
  select(block:date_out)
# low
plot_low <- plot_codes_21 |> 
  select(block.1:date_out.1)

plot_low <- plot_low |> 
  filter(if_any(everything(), ~ !is.na(.) & . != "")) |> 
  rename(block = block.1,
         treat = treat.1,
         tomst = tomst.1,
         date_out = date_out.1)

# combine again under each other
plot_codes_clean <- bind_rows(hi = plot_high, lo = plot_low, .id = "site")
head(plot_codes_clean)
str(plot_codes_clean) # tomst = chr 

# delete rows with no logger
plot_codes_clean <- plot_codes_clean |> 
  filter(date_out != "")

# combine tomst data with plot labels -------------------------------------
tomst_21_raw <- left_join(plot_codes_clean, tomst_data_21, by = "tomst")
head(tomst_21_raw)

# Add treat_warming and treat_competition columns based on treat ----------
tomst_21_raw <- tomst_21_raw |> 
  mutate(
    treat_warming = case_when(
      site == "hi" & treat %in% c("A", "C", "E") ~ "warm",
      site == "hi" & treat %in% c("B", "D", "F") ~ "ambi",
      site == "lo" & treat %in% c("A", "B") ~ "ambi",  
      TRUE ~ NA_character_  # Assign NA to unexpected cases
    ),
    treat_competition = case_when(
      site == "hi" & treat %in% c("A", "B") ~ "vege",
      site == "hi" & treat %in% c("C", "D") ~ "control",
      site == "hi" & treat %in% c("E", "F") ~ "bare",
      site == "lo" & treat == "A" ~ "vege",  
      site == "lo" & treat == "B" ~ "bare",
      TRUE ~ NA_character_
    )
  )

# calculate soil moisture -----------------------------------------------------------
# apply the soil moisture function
tomst_21_raw <- tomst_21_raw |> 
  mutate(TMS_moist = calc_soil_moist(rawsoilmoist = Soilmoisture_raw, 
                                soil_temp = TMS_T1, 
                                soilclass ="silt_loam"))
# using silt loam even though this might not be correct but comes closest


# combined treatment column -----------------------------------------------
tomst_21_raw <- tomst_21_raw |> 
  mutate(treat_combined = paste(site, treat_warming, treat_competition, sep = "_"))


# filter field season already here ----------------------------------------
unique(plot_codes_clean$date_out)
# in the field "15.08.2021" "26.07.2021" "30.08.2021"
# out maybe: 06.10.21

start_date <- as.Date("2021-08-31") 
end_date <- as.Date("2021-10-05")

# Filter the data for the specified date range
tomst_21_raw_filtered <- tomst_21_raw |> 
  filter(between(date_time, left = start_date, right = end_date))

# plot soil moisture ------------------------------------------------------
# one line per logger
ggplot(tomst_21_raw_filtered, aes(x = date_time, y = TMS_moist, color = tomst)) +
  geom_line() +
  theme(legend.position = "none")
#

# calculate average per date per treatment
# it's still every 15 min
tomst_21_raw_average <- tomst_21_raw_filtered |> 
  group_by(date_time, treat_combined) |> 
  summarize(avg_soil_moist = mean(TMS_moist, na.rm = TRUE),,.groups = 'drop')
head(tomst_21_raw_average)

# plot all treatments
soil_moist <- ggplot(tomst_21_raw_average, aes(x = date_time, y = avg_soil_moist, color = treat_combined)) +
  geom_line() +
  theme_bw() +
  theme(legend.position = "right")
soil_moist


# add column VWC --------------------------------------------------------
tomst_21_raw_filtered <- tomst_21_raw_filtered |> 
  mutate(VWC = NA)


# import metadata -------------------------------------------------------
metadata <- read.csv("Data/RangeX_metadata_plot_NOR.csv")
metadata <- metadata |> 
  select(-"X")


# fix col names --------------------------------------------------------
names(metadata)
names(tomst_21_raw_filtered)

tomst_21_raw_filtered <- tomst_21_raw_filtered |> 
  rename("block_ID_original" = "block",
         "plot_ID_original" = "treat")

# to match plot_ID_original
tomst_21_raw_filtered <- tomst_21_raw_filtered |> 
  mutate(plot_ID_original = case_when(
    plot_ID_original == "A" ~ "a",
    plot_ID_original == "B" ~ "b",
    plot_ID_original == "C" ~ "c",
    plot_ID_original == "D" ~ "d",
    plot_ID_original == "E" ~ "e",
    plot_ID_original == "F" ~ "f"
  ))


# merge metadata with tomst_21_raw_filtered -------------------------------
metadata <- metadata |> 
  mutate(across(c(block_ID_original, plot_ID_original), as.character))

tomst_21_raw_filtered <- tomst_21_raw_filtered |> 
  mutate(across(c(block_ID_original, plot_ID_original), as.character))

tomst_21_clean<- left_join(metadata, tomst_21_raw_filtered, 
                           by = c( "site", "block_ID_original",
                                   "plot_ID_original",
                                   "treat_warming", "treat_competition"))


# select only columns needed for clean data on OSF ------------------------
rx_tomst_21_clean <- tomst_21_clean |> 
  select(unique_plot_ID, date_time, 
         TMS_T1, TMS_T2, TMS_T3, 
         TMS_moist, VWC)


# save clean data ---------------------------------------------------------
write.csv(rx_tomst_21_clean, "Data/Data_tomst_loggers/CleanEnvTMS4/RangeX_clean_EnvTMS4_2021_NOR.csv", row.names = FALSE)

tms21 <- read.csv("Data/Data_tomst_loggers/CleanEnvTMS4/RangeX_clean_EnvTMS4_2021_NOR.csv")

