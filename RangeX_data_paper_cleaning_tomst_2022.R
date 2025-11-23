
# Climate data TOMST loggers NOR 2022 --------------------------------------------

## Data used: Data/Data_tomst_loggers/tomst_2022/,
##            tomst_plot_codes_2022.csv,
##            RangeX_metadata_plot_NOR.csv
## Date:      09.05.2025
## Author:    Nadine Arzt
## Purpose:   Clean TOMST logger data 2022

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

# import data 2022 ------------------------------------------------------
# List all files in the 'Data_tomst_loggers' folder that start with 'data'
tomst_22 <- list.files(path = "Data/Data_tomst_loggers/tomst_2022/", pattern = "^data_\\d+.*\\.csv$", full.names = TRUE)

test_file <- read_delim(tomst_22[2], delim = ";", skip = 1)
head(test_file)
# 2020.02.24 15:45 - time format deletes the seconds

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
tomst_data_22 <- map_dfr(tomst_22, read_tomst_file_21)
head(tomst_data_22)

# get plot codes 22 ------------------------------------------------------
plot_codes_22 <- read.csv2("Data/Data_tomst_loggers/tomst_plot_codes_2022.csv", skip = 1)
plot_codes_22

# split dataset into low and high 
# high
plot_high <- plot_codes_22 |> 
  select(block:date_out)
# low
plot_low <- plot_codes_22 |> 
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
  filter(tomst != "na")

# combine tomst data with plot labels -------------------------------------
tomst_22_raw <- left_join(plot_codes_clean, tomst_data_22, by = "tomst")
head(tomst_22_raw)

# Add treat_warming and treat_competition columns based on treat ----------
tomst_22_raw <- tomst_22_raw |> 
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
tomst_22_raw <- tomst_22_raw |> 
  mutate(VWC = calc_soil_moist(rawsoilmoist = Soilmoisture_raw, 
                                     soil_temp = TMS_T1, 
                                     soilclass ="silt_loam"))
# using silt loam even though this might not be correct but comes closest


# rename raw soil moisture into TMS_moist ---------------------------------
# these are the raw moisture count values
tomst_22_raw <- tomst_22_raw |> 
  rename(TMS_moist = Soilmoisture_raw)


# plot soil moisture ------------------------------------------------------
# one line per logger
ggplot(tomst_22_raw, aes(x = date_time, y = VWC, color = tomst)) +
  geom_line() +
  theme(legend.position = "none")

# combined treatment column -----------------------------------------------
tomst_22_raw <- tomst_22_raw |> 
  mutate(treat_combined = paste(site, treat_warming, treat_competition, sep = "_"))


# filter field season already here ----------------------------------------
# in the field "14.06.2022"
# out maybe: 06.10.21

start_date <- as.Date("2022-06-17") # actually out 14.06.22 but very low values for some in the beginning
end_date <- as.Date("2022-10-11")

# Filter the data for the specified date range
tomst_22_raw_filtered <- tomst_22_raw |> 
  filter(between(date_time, left = start_date, right = end_date))

# plot soil moisture ------------------------------------------------------
# one line per logger
ggplot(tomst_22_raw_filtered, aes(x = date_time, y = VWC, color = tomst)) +
  geom_line() +
  theme(legend.position = "none")

# 94217337 has quite low values in September but keep?

# plot TMS_1  ------------------------------------------------------
# one line per logger
ggplot(tomst_22_raw_filtered, aes(x = date_time, y = TMS_T1, color = tomst)) +
  geom_line() +
  theme(legend.position = "none")
# looks fine

# plot TMS_2  ------------------------------------------------------
# one line per logger
ggplot(tomst_22_raw_filtered, aes(x = date_time, y = TMS_T2, color = tomst)) +
  geom_line() +
  theme(legend.position = "none")
# definitely something weird going on here

# fix TMS_2 ---------------------------------------------------------------
# plot all individually?

# identify problem loggers
# get all logger numbers
loggers <- unique(tomst_22_raw_filtered$tomst)

# create pdf with one plot for TMS_2 per logger
pdf("Data/Data_tomst_loggers/Graphs/RangeX_plots_tomst_loggers_TMS_2_22.pdf")

# Create and print plots for each logger using map
unique(tomst_22_raw_filtered$tomst) |> 
  map(~ {
    logger_data <- subset(tomst_22_raw_filtered, tomst == .x)
    p <- ggplot(logger_data, aes(x = date_time, y = TMS_T2, color = tomst)) +
      geom_line() +
      theme(legend.position = "none") +
      ggtitle(paste("Logger:", .x))
    print(p)
  })

# Close the PDF
dev.off()

# go manually though them and check which ones look wrong

# these are faulty loggers:
# Define the vector of logger numbers to set TMS_T2 to NA
loggers_to_na <- c("94217346", "94217307", "94217333", "94217344",
                   "94217328", "94217326", "94217332", "94217310",
                   "94217320", "94217348", "94217318", "94217327",
                   "94217321", "94217322", "94217330", "94217345", 
                   "94217325", "94217301", "94217323", "94217308",
                   "94217306", "94217349", "94217317", "94217303",
                   "94217341", "94217305", "94217324", "94217335",
                   "94217316", "94217302", "94217334", "94217347",
                   "94217343", "94217331", "94217329", "94217314",
                   "94217336", "94217338", "94217311", "94217340",
                   "94217339", "94217337", "94217315") 

# make the values in TMS_2 for these faulty loggers NA
tomst_22_raw_filtered$TMS_T2[tomst_22_raw_filtered$tomst %in% loggers_to_na] <- NA
  
# plot TMS_2 again
ggplot(tomst_22_raw_filtered, aes(x = date_time, y = TMS_T2, color = tomst)) +
  geom_line() +
  theme(legend.position = "none")
# good now

# plot TMS_3  ------------------------------------------------------
# one line per logger
ggplot(tomst_22_raw_filtered, aes(x = date_time, y = TMS_T3, color = tomst)) +
  geom_line() +
  theme(legend.position = "none")
# many show -82
# e.g. 94217346, 94217307, 94217333, 94217328 have -82 --> delete logger?


# filter out tomst loggers with faulty values for TMS_3
tomst_loggers_faulty <- tomst_22_raw_filtered |> 
  filter(TMS_T3 == -82) |> 
  select(tomst) |> 
  distinct()
tomst_loggers_faulty
# 38 loggers

# make NA instead of -82 values to keep the loggers for TMS_1 which seems fine
tomst_22_raw_filtered <- tomst_22_raw_filtered |> 
  mutate(TMS_T3 = if_else(TMS_T3 == -82, NA_real_, TMS_T3))

ggplot(tomst_22_raw_filtered, aes(x = date_time, y = TMS_T3, color = tomst)) +
  geom_line() +
  theme(legend.position = "none")


# import metadata -------------------------------------------------------
metadata <- read.csv("Data/Metadata/RangeX_clean_MetadataPlot_NOR.csv")

# fix col names --------------------------------------------------------
names(metadata)
names(tomst_22_raw_filtered)

tomst_22_raw_filtered <- tomst_22_raw_filtered |> 
  rename("block_ID_original" = "block",
         "plot_ID_original" = "treat")

# to match plot_ID_original
tomst_22_raw_filtered <- tomst_22_raw_filtered |> 
  mutate(plot_ID_original = case_when(
    plot_ID_original == "A" ~ "a",
    plot_ID_original == "B" ~ "b",
    plot_ID_original == "C" ~ "c",
    plot_ID_original == "D" ~ "d",
    plot_ID_original == "E" ~ "e",
    plot_ID_original == "F" ~ "f"
  ))


# merge metadata with tomst_22_raw_filtered_clean ----------------------
metadata <- metadata |> 
  mutate(across(c(block_ID_original, plot_ID_original), as.character))

tomst_22_raw_filtered <- tomst_22_raw_filtered |> 
  mutate(across(c(block_ID_original, plot_ID_original), as.character))

tomst_22_clean<- left_join(metadata, tomst_22_raw_filtered, 
                           by = c( "site", "block_ID_original",
                                   "plot_ID_original",
                                   "treat_warming", "treat_competition"))

# some rows are NA everywhere except unique_plot_ID
# delete these rows
tomst_22_clean <- tomst_22_clean |> 
  filter(!is.na(date_time))

# select only columns needed for clean data on OSF ------------------------
rx_tomst_22_clean <- tomst_22_clean |> 
  select(unique_plot_ID, date_time, 
         TMS_T1, TMS_T2, TMS_T3, 
         TMS_moist, VWC)



# save clean data ---------------------------------------------------------
write.csv(rx_tomst_22_clean, "Data/Data_tomst_loggers/CleanEnvTMS4/RangeX_clean_EnvTMS4_2022_NOR.csv", row.names = FALSE)

tms22 <- read_csv("Data/Data_tomst_loggers/CleanEnvTMS4/RangeX_clean_EnvTMS4_2022_NOR.csv")



