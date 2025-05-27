
# Climate data TOMST loggers NOR 2024 --------------------------------------------

## Data used: Data/Data_tomst_loggers/tomst_2024/,
##            tomst_plot_codes_2024.csv,
##            RangeX_metadata_plot_NOR.csv
## Date:      14.03.2025
## Author:    Nadine Arzt
## Purpose:   Clean TOMST logger data 2024

# load library ------------------------------------------------------------
library(tidyverse)
library(conflicted)
conflict_prefer_all("dplyr", quiet = TRUE)
library(janitor)

# default theme
theme_set(theme_bw())

# comments --------------------------------------------------------------
# https://tomst.com/web/en/systems/tms/software/
# temp1 is in soil, temp2 at 0cm and temp3 20cm above ground

# how to deal with the loggers that we took out after digging roots?
# filter shorter time period for them?

# source script with functions --------------------------------------------
source("RangeX_data_paper_functions.R")

# import data 2024 --------------------------------------------------------
# List all files in the 'Data_tomst_loggers' folder that start with 'data'
tomst_24 <- list.files(path = "Data/Data_tomst_loggers/tomst_2024/", pattern = "^data_\\d+.*\\.csv$", full.names = TRUE)

# test to see structure of files
test_file <- read_delim(tomst_24[1], delim = ";", skip = 1)
head(test_file)
# has , 

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

# get one dataframe with data from all files using the list of files (tomst_24) with a loop
# use same function as for 21 data with several date formats
tomst_data_24 <- map(tomst_24, read_tomst_file_21) |> 
  list_rbind()
head(tomst_data_24)

# get plot codes 24 ------------------------------------------------------
plot_codes_24 <- read.csv2("Data/Data_tomst_loggers/tomst_plot_codes_2024.csv", skip = 1)
plot_codes_24

# split dataset into low and high 
# high
plot_high_24 <- plot_codes_24 |> 
  select(block:comment)
# low
plot_low_24 <- plot_codes_24 |> 
  select(block.1:comment.1)

plot_low_24 <- plot_low_24 |> 
  filter(if_any(everything(), ~ !is.na(.) & . != "")) |> 
  rename(block = block.1,
         treat = treat.1,
         tomst = tomst.1,
         date_out = date_out.1,
         date_home = date_home.1, # column wasn't in 23
         comment = comment.1)

# combine again under each other
plot_codes_clean_24 <- bind_rows(hi = plot_high_24, lo = plot_low_24, .id = "site")
head(plot_codes_clean_24)
str(plot_codes_clean_24) # tomst = int 

# tomst into character
plot_codes_clean_24 <- plot_codes_clean_24 |> 
  mutate(tomst = as.character(tomst))

# combine tomst data with plot labels -------------------------------------
tomst_24_raw <- left_join(plot_codes_clean_24, tomst_data_24, by = "tomst")
head(tomst_24_raw)

# Add treat_warming and treat_competition columns based on treat ----------
# why does it not work with .default ~ NA
tomst_24_raw <- tomst_24_raw |> 
  mutate(
    treat_warming = case_when(
      site == "hi" & treat %in% c("A", "C", "E") ~ "warm",
      site == "hi" & treat %in% c("B", "D", "F") ~ "ambi",
      site == "lo" & treat %in% c("A", "B") ~ "ambi",  
      TRUE ~ NA  # Assign NA to unexpected cases
    ),
    treat_competition = case_when(
      site == "hi" & treat %in% c("A", "B") ~ "vege",
      site == "hi" & treat %in% c("C", "D") ~ "control",
      site == "hi" & treat %in% c("E", "F") ~ "bare",
      site == "lo" & treat == "A" ~ "vege",  
      site == "lo" & treat == "B" ~ "bare",
      TRUE ~ NA
    )
  )

# delete empty columns -----------------------------------------------------
tomst_24_raw <- tomst_24_raw |> 
  select(where(~ !all(is.na(.))))

# delete loggers due to very low soil moisture in sep ---------------------
tomst_24_raw <- tomst_24_raw |> 
  filter(tomst != "94217327") |> 
  filter(tomst != "94201716") |> 
  filter(tomst != "94217340") |> 
  filter(tomst != "94217338")


# calculate soil moisture ----------------------------------------------------------
# function to calculate soil moisture from raw values in function script
# RangeX_data_paper_functions.R
# apply the soil moisture function here
tomst_24_raw <- tomst_24_raw |> 
  mutate(TMS_moist = calc_soil_moist(rawsoilmoist = Soilmoisture_raw, 
                                     soil_temp = TMS_T1, 
                                     soilclass ="silt_loam"))

# combined treatment column -----------------------------------------------
tomst_24_raw <- tomst_24_raw |> 
  mutate(treat_combined = paste(site, treat_warming, treat_competition, sep = "_"))

# filter field season already here ----------------------------------------
# high: 30.05 - 15.10.24
# low: 12.05 - 15.10.24
start_date <- as.Date("2024-06-06") # 3 loggers came in later
end_date <- as.Date("2024-10-14")

# Filter the data for the specified date range
tomst_24_raw_filtered <- tomst_24_raw |> 
  filter(between(date_time, left = start_date, right = end_date))

# plot soil moisture ------------------------------------------------------
# one line per logger
s <- ggplot(tomst_24_raw_filtered, aes(x = date_time, y = TMS_moist, color = tomst)) +
  geom_line() +
  theme(legend.position = "none")
s
# ok, what we see is some loggers with very low soil moisture in sep
# maybe when we took out some loggers after digging out roots?

# check if you can identify loggers that have these low values---------------------------------
negative_moisture_loggers <- tomst_24_raw_filtered |> 
  filter(between(date_time, as.Date("2024-09-01"), as.Date("2024-09-20"))) |> 
  filter(TMS_moist < 0) |> 
  distinct(tomst)
# but its 18 loggers, we didn't remove that many
# something is wrong with them

# plot only negative_moisture_loggers
filtered_loggers <- tomst_24_raw_filtered |> 
  filter(tomst %in% negative_moisture_loggers$tomst) |> 
  filter(between(date_time, as.Date("2024-09-01"), as.Date("2024-09-20")))

s %+% filtered_loggers+
  theme(legend.position = "right")

# plot one logger
one_logger <- filtered_loggers |> 
  filter(tomst == 94217338)

s %+% one_logger

#15 94217327, 94201716, 94217340 are very weird
# what if you take these 3 out

# we still have 15 loggers that have a drop around sep


# label outlier --------------------------------------------
# flag data points that have a temp dif of > 4 degree within 15 min
tomst_24_raw_filtered_flagged <- tomst_24_raw_filtered |>
  arrange(tomst, date_time) |>  # important for time order
  group_by(tomst) |>  # do per logger
  mutate(
    diff_forward = abs(TMS_T1 - lead(TMS_T1)),
    temp_outlier = ifelse(diff_forward > 4, TRUE, FALSE)  # set threshold here
  ) |> 
  ungroup()

ggplot(tomst_24_raw_filtered_flagged, aes(x = date_time, y = TMS_T1, color = temp_outlier)) +
  geom_point()

# delete flagged outlier
# Remove only the flagged outlier rows
tomst_24_raw_filtered <- tomst_24_raw_filtered_flagged |>
  filter(!temp_outlier)


# delete everything > 28 --------------------------------------------------
tomst_24_raw_filtered <- tomst_24_raw_filtered |>
  filter(TMS_T1 <= 28)


# flag more ----------------------------------------------
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

tomst_24_raw_filtered <- tomst_flagged |>
  filter(!temp_outlier)

# flag again  --------------------------------------------
tomst_flagged_2 <- tomst_24_raw_filtered |>
  arrange(tomst, date_time) |>  # important for time order
  group_by(tomst) |>  # do per logger
  mutate(
    diff_forward = abs(TMS_T1 - lead(TMS_T1)),
    temp_outlier = ifelse(diff_forward > 4, TRUE, FALSE)  
  ) |> 
  ungroup()

ggplot(tomst_flagged_2, aes(x = date_time, y = TMS_T1, color = temp_outlier)) +
  geom_point()

tomst_24_raw_filtered <- tomst_flagged_2 |>
  filter(!temp_outlier)

# add column VWC ----------------------------------------------------------
tomst_24_raw_filtered <- tomst_24_raw_filtered |> 
  mutate(VWC = NA)

# import metadata -------------------------------------------------------
metadata <- read.csv("Data/RangeX_metadata_plot_NOR.csv")
metadata <- metadata |> 
  select(-"X")

# fix col names --------------------------------------------------------
names(metadata)
names(tomst_24_raw_filtered)

tomst_24_raw_filtered <- tomst_24_raw_filtered |> 
  rename("block_ID_original" = "block",
         "plot_ID_original" = "treat")

# to match plot_ID_original
tomst_24_raw_filtered <- tomst_24_raw_filtered |> 
  mutate(plot_ID_original = case_when(
    plot_ID_original == "A" ~ "a",
    plot_ID_original == "B" ~ "b",
    plot_ID_original == "C" ~ "c",
    plot_ID_original == "D" ~ "d",
    plot_ID_original == "E" ~ "e",
    plot_ID_original == "F" ~ "f"
  ))


# merge metadata with tomst_24_raw_filtered -------------------------------
metadata <- metadata |> 
  mutate(across(c(block_ID_original, plot_ID_original), as.character))

tomst_24_raw_filtered <- tomst_24_raw_filtered |> 
  mutate(across(c(block_ID_original, plot_ID_original), as.character))

tomst_24_clean<- left_join(metadata, tomst_24_raw_filtered, 
                           by = c( "site", "block_ID_original",
                                   "plot_ID_original",
                                   "treat_warming", "treat_competition"))


# delete empty rows -------------------------------------------------------
tomst_24_clean <- tomst_24_clean |> 
  filter(!is.na(tomst))

# select only columns needed for clean data on OSF ------------------------
rx_tomst_24_clean <- tomst_24_clean |> 
  select(unique_plot_ID, date_time, 
         TMS_T1, TMS_T2, TMS_T3, 
         TMS_moist, VWC)


# save clean data ---------------------------------------------------------
# write.csv(rx_tomst_24_clean, "Data/Data_tomst_loggers/RangeX_clean_tomst_NOR_2024.csv")

tomst <- read_csv("Data/Data_tomst_loggers/RangeX_clean_tomst_NOR_2024.csv")









