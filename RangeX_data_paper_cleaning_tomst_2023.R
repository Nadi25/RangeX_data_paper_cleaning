
# Climate data TOMST loggers NOR 2023 --------------------------------------------

## Data used: Data/Data_tomst_loggers/tomst_2023/,
##            tomst_plot_codes_2023.csv,
##            RangeX_metadata_plot_NOR.csv
##            Sunrise_sundown_Voss_2023.csv
## Date:      03.03.2025
## Author:    Nadine Arzt
## Purpose:   Clean TOMST logger data 2023

# load library ------------------------------------------------------------
library(tidyverse)
library(conflicted)
conflict_prefer_all("dplyr", quiet = TRUE)
library(janitor)

# default theme
theme_set(theme_bw())

# comments ----------------------------------------------------------------
# temp1 is in soil, temp2 at 0cm and temp3 20cm above ground

# 2023: high out: "08.06.2023" and "20.06.2023" - "23.10.2023"
# 2023: low out: "12.05.2023" and "19.06.2023" - "24.10.2023"
# so take the later one each?

# deleted tomst 94201723 and 94217314 because of impossible values
# should I delete 94217320? it has a drop in early August: 2023-08-19 11:15:00

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
tomst_data_23 <- map(tomst_23, read_tomst_file) |> 
  list_rbind()
head(tomst_data_23)

# get plot codes 23 ------------------------------------------------------
plot_codes_23 <- read.csv2("Data/Data_tomst_loggers/tomst_plot_codes_2023.csv", skip = 1)
plot_codes_23

# split dataset into low and high 
# high
plot_high <- plot_codes_23 |> 
  select(block:comment)
# low
plot_low <- plot_codes_23 |> 
  select(block.1:comment.1)

plot_low <- plot_low |> 
  filter(if_any(everything(), ~ !is.na(.) & . != "")) |> 
  rename(block = block.1,
        treat = treat.1,
        tomst = tomst.1,
        date_out = date_out.1,
        comment = comment.1)

# combine again under each other
plot_codes_clean <- bind_rows(hi = plot_high, lo = plot_low, .id = "site")
head(plot_codes_clean)
str(plot_codes_clean) # tomst = chr 

# tomst into character
plot_codes_clean <- plot_codes_clean |> 
  mutate(tomst = as.character(tomst))

# combine tomst data with plot labels -------------------------------------
tomst_23_raw <- left_join(plot_codes_clean, tomst_data_23, by = "tomst")
head(tomst_23_raw)


# Add treat_warming and treat_competition columns based on treat ----------
# why does it not work with .default ~ NA
tomst_23_raw <- tomst_23_raw |> 
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
tomst_23_raw <- tomst_23_raw |> 
  select(where(~ !all(is.na(.))))

# delete tomst 94201723 because of impossible values --------------------------------------------------
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
calc_soil_moist <- function(rawsoilmoist, soil_temp, soilclass){
  
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
  soilclass.df <- soilclass.df |> 
    filter(
      soil == {{soilclass}}
    )
  
  #calculating the volumetric soil moisture with the parameters corresponding to the soil class and the raw soil moisture from the logger
  volmoist = with(soilclass.df, {(a * rawsoilmoist^2) + (b * rawsoilmoist) + c})
  
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
  
  volmoistcorr
  # return(volmoist) #let's just use the soil moisture without temperature correction for now
}

# apply the soil moisture function
tomst_23_raw <- tomst_23_raw |> 
  mutate(TMS_moist = calc_soil_moist(rawsoilmoist = Soilmoisture_raw, 
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
  filter(between(Date, left = start_date, right = end_date))


# plot soil moisture ------------------------------------------------------
# one line per logger
p <- ggplot(tomst_23_raw_filtered, aes(x = Date, y = TMS_moist, color = tomst)) +
  geom_line() +
  theme(legend.position = "none")
p 
# is there one outlier in the middle?

# check if the drop in the middle is an outlier
# by finding min in August-September
out <- tomst_23_raw_filtered |> 
  filter(Date >= as.Date("2023-08-01") & Date <= as.Date("2023-09-20")) |> 
  filter(TMS_moist == min(TMS_moist, na.rm = TRUE)) 

# plot only 94217320
one_logger <- tomst_23_raw_filtered |> 
  filter(tomst == 94217320)

p %+% one_logger
# ok, it has a drop in early August: 2023-08-19 11:15:00
# should we delete the whole logger?


# calculate average per date per treatment
# it's still every 15 min
tomst_23_raw_average <- tomst_23_raw_filtered |> 
  group_by(Date, treat_combined) |> 
  summarize(avg_soil_moist = mean(TMS_moist, na.rm = TRUE), .groups = 'drop')
head(tomst_23_raw_average)

# plot all treatments
soil_moist <- ggplot(tomst_23_raw_average, aes(x = Date, y = avg_soil_moist, color = treat_combined)) +
  geom_line() +
  theme_bw() +
  theme(legend.position = "right")
soil_moist

ggsave(filename = "RangeX_soil_moisture_23.png", 
       plot = soil_moist, 
       path = "Data/Data_tomst_loggers/", 
       width = 10, height = 6)

# it doesn't look like there is a drying effect of the OTCs here
# warm has higher soil moist values
# less transpiration due to OTCs?


# fix colnames ------------------------------------------------------------
names(tomst_23_raw_filtered)

tomst_23_raw_filtered <- tomst_23_raw_filtered |> 
  rename(date_time = Date,
         TMS_T1 = Temp1,
         TMS_T2 = Temp2,
         TMS_T3 = Temp3)

# add column VWC
tomst_23_raw_filtered <- tomst_23_raw_filtered |> 
  mutate(VWC = NA)


# import metadata -------------------------------------------------------
metadata <- read.csv("Data/RangeX_metadata_plot_NOR.csv")
metadata <- metadata |> 
  select(-"X")


# fix col names --------------------------------------------------------
names(metadata)
names(tomst_23_raw_filtered)

tomst_23_raw_filtered <- tomst_23_raw_filtered |> 
  rename("block_ID_original" = "block",
         "plot_ID_original" = "treat")

# to match plot_ID_original
tomst_23_raw_filtered <- tomst_23_raw_filtered |> 
  mutate(plot_ID_original = case_when(
    plot_ID_original == "A" ~ "a",
    plot_ID_original == "B" ~ "b",
    plot_ID_original == "C" ~ "c",
    plot_ID_original == "D" ~ "d",
    plot_ID_original == "E" ~ "e",
    plot_ID_original == "F" ~ "f"
  ))


# merge metadata with tomst_23_raw_filtered -------------------------------
metadata <- metadata |> 
  mutate(across(c(block_ID_original, plot_ID_original), as.character))

tomst_23_raw_filtered <- tomst_23_raw_filtered |> 
  mutate(across(c(block_ID_original, plot_ID_original), as.character))

tomst_23_clean<- left_join(metadata, tomst_23_raw_filtered, 
                         by = c( "site", "block_ID_original",
                                 "plot_ID_original",
                                 "treat_warming", "treat_competition"))


# select only columns needed for clean data on OSF ------------------------
rx_tomst_23_clean <- tomst_23_clean |> 
  select(unique_plot_ID, date_time, 
         TMS_T1, TMS_T2, TMS_T3, 
         TMS_moist, VWC)


# save clean data ---------------------------------------------------------
# write.csv(rx_tomst_23_clean, "Data/Data_tomst_loggers/RangeX_clean_tomst_NOR_2023.csv")

tomst <- read_csv("Data/Data_tomst_loggers/RangeX_clean_tomst_NOR_2023.csv")



