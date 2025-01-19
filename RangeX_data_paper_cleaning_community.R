

# RangeX community cover data cleaning  -------------------------------

## Data used: RangeX_raw_comcov_high_2021.xlsx, 
##            RangeX_raw_comcov_low_2021.xlsx,
##            RangeX_raw_comcov_high_2023.xlsx, 
##            RangeX_raw_comcov_low_2023.xlsx,
##            RangeX_Metadata.csv
## Date:      15.01.2025
## Author:    Nadine Arzt
## Purpose:   Cleaning of the complete raw data files of community 2021-2023


# load library ------------------------------------------------------------
library(conflicted)
conflict_prefer_all("dplyr", quiet = TRUE)
library(tidyverse)
library(purrr)
library(readxl)
library(janitor)

# remotes::install_github("Between-the-Fjords/turfmapper")
library(turfmapper)

# things to fix -----------------------------------------------------------
# it's Chinese style 2023-24
# change the subplots to match with 2021-22
# who is cris?
# who is JT?

# comments on sheets ----------------------------------------------------------------
## 21 high 2B: *Started using Agr cap (didn't in Blocks 3-6)...likely confused with Ant odo and Ple alp
## 21 high 2D: *Viola sp: mix of palustre and riviniana?


# import general metadata file -------------------------------------------------
metadata <- read.csv2("Data/RangeX_Metadata.csv")
head(metadata)
colnames(metadata)

## clean column names
metadata <- clean_names(metadata)

## filter only NOR
metadata_NOR <- metadata |> 
  dplyr::filter(grepl('NOR', region))
head(metadata_NOR)
str(metadata_NOR)

metadata_NOR <- metadata_NOR |> 
  mutate(across(block_id_original, as.character))

metadata_NOR_com <- metadata_NOR |> 
  select(site, block_id_original, plot_id_original, treat_warming, 
         treat_competition, added_focals, block_id,
         unique_plot_id)

# filter out duplicates
duplicates <- metadata_NOR_com[duplicated(metadata_NOR_com$unique_plot_id), ]
duplicates

# keep only unique rows
metadata_NOR_com <- metadata_NOR_com |> 
  distinct(unique_plot_id, .keep_all = TRUE)
# 60 plots


# add metadata for c and d control plots ----------------------------------
missing_plots <- expand.grid(
  site = "hi",
  block_id_original = as.character(1:10), # Convert to character
  plot_id_original = c("c", "d"),
  treat_competition = "vege",
  added_focals = "nf"
) |> 
  # Assign treat_warming based on plot_id_original
  mutate(
    treat_warming = if_else(plot_id_original == "c", "warm", "ambi"), 
    block_id = as.integer(block_id_original), # Ensure block_id matches block_id_original
    unique_plot_id = paste0(
      "NOR.", site, ".", treat_warming, ".", treat_competition, ".", added_focals, ".",
      sprintf("%02d", block_id) # Format block_id as two digits
    )
  )

names(missing_plots)

# correct order
missing_plots <- missing_plots |> 
  select(site, block_id_original, plot_id_original, treat_warming, 
         treat_competition, added_focals, block_id, unique_plot_id)


# join with metadata
metadata_NOR_com <- bind_rows(metadata_NOR_com, missing_plots)


# we don't need bare plots here
metadata_NOR_com <- metadata_NOR_com |> 
  filter(treat_competition != "bare")



# import community metadata -----------------------------------------------
files <- dir(path = "Data/Data_community/", pattern = "\\.xlsx$", full.names = TRUE, recursive = TRUE)

#Function to read in meta data
meta_com_raw <- map_df(set_names(files), function (file) {
  print(file)
  sheets <- excel_sheets(file)[-1] # don't read first sheet, it is readme
  map_df(sheets,  ~ {
    read_xlsx(
      path = file, 
      sheet = .x, 
      range = cell_limits(c(1, 20), c(15, 21)), # start at row 1, column 20 and end at row 15, column 21
      col_names = FALSE,       # column names are not present
      # col_types = "text" # Flexible to handle varying column numbers
    ) |> 
      setNames(c("metadata_name", "metadata_value")) |> 
      mutate(sheet_name = .x)
  })
}, .id = "file") |> 
  # date needs to be date format and sometimes it's a range 
  mutate(
    metadata_value = case_when(
      metadata_name == "date" & !is.na(as.numeric(metadata_value)) ~ 
        as.character(as.Date(as.numeric(metadata_value), origin = "1899-12-30")), # Numeric to Date
      metadata_name == "date" ~ metadata_value, # Leave text-based ranges as is
      TRUE ~ metadata_value
    )
  ) |> 
  filter(!is.na(metadata_name)) # Remove rows where metadata_name is NA from 21


# pivot wider meta data ---------------------------------------------------
meta_com_raw_wide <- meta_com_raw |> 
  pivot_wider(names_from = metadata_name, values_from = metadata_value)


# import vegetation data --------------------------------------------------
data_com_raw <- map_df(set_names(files), function(file) {
  print(file)
  sheets <- excel_sheets(file)[-1]
  
  map_df(sheets, ~ {
    data <- read_xlsx(
      path = file, 
      sheet = .x, 
      range = cell_cols(1:18), 
      col_names = TRUE, 
      col_types = "text"
    )
    
    # rename columns based on the dataset's structure
    if ("species" %in% names(data)) {
      data <- data |> rename(species = species) # species in 21 and Species in 23
    } else if ("Species" %in% names(data)) {
      data <- data |> rename(species = Species)
    }
    
    if ("total cover" %in% names(data)) {
      data <- data |> rename(total_cover = `total cover`)
    } else if ("total cover %" %in% names(data)) {
      data <- data |> rename(total_cover = `total cover %`)
    }
    
    data |> 
      mutate(sheet_name = .x, file = file)
  })
})


# combine meta data and veg data -------------------------------------------------------------
community_data <- left_join(meta_com_raw_wide, data_com_raw, by = c("file", "sheet_name"))


# add column with year and site -------------------------------------------
# Extract year and site from the file column and add them as new columns
community_data <- community_data %>%
  mutate(
    year = str_extract(file, "\\d{4}"), # Extracts the year (4 digits)
    site = str_extract(file, "(?<=comcov_)[a-z]+") # Extracts the site (text after 'comcov_')
  )


# fix community_data columns -----------------------------------------------------
community_data_raw <- community_data |> 
  rename("block_id_original" = "block",
         "plot_id_original" = "plot") |> 
  mutate(site = case_when(site == "low" ~ "lo",
                          site == "high" ~ "hi",
                          TRUE ~ site)) |> 
  mutate(across(c(block_id_original, plot_id_original, recorder, 
                  scribe, species), as.character)) |> 
  mutate(plot_id_original = case_when(plot_id_original == "A" ~ "a",
                                      plot_id_original == "B" ~ "b",
                                      plot_id_original == "C" ~ "c",
                                      plot_id_original == "D" ~ "d",
                                      TRUE ~ plot_id_original))
  
# replace NAs with 0 ------------------------------------------------------
community_data_raw <- community_data_raw |> 
  mutate_at(vars(19:36),
            ~replace(., is.na(.), 0))


# mutate(across(c(`total cover (%)`, `bare ground (%)`, `litter (%)`,
#                   `cryptogams (%)`, height_1, height_2, height_3,
#                   height_4, height_5)), as.numeric())



# names to initials -------------------------------------------------------
unique(community_data_raw$recorder)
unique(community_data_raw$scribe)


community_data_raw <- community_data_raw |>
  mutate(
    recorder = case_when(
      recorder %in% c("Dagmar", "dagmar", "DE", "de") ~ "DE",
      recorder %in% c("Nathan", "nathan", "np") ~ "NP",
      recorder %in% c("Josh", "josh") ~ "JL",
      recorder %in% c("Susanne", "susanne") ~ "SB",
      recorder %in% c("np/de") ~ "NP/DE",
      recorder %in% c("Nadine") ~ "NA",
      recorder %in% c("Nadine/Susanne") ~ "NA/SB",
      recorder %in% c("Dagmar, Nadine, Susanne") ~ "DE/NA/SB",
      TRUE ~ recorder
    ),
    scribe = case_when(
      scribe %in% c("Dagmar", "dagmar", "DE", "de") ~ "DE",
      scribe %in% c("Nathan", "nathan", "np") ~ "NP",
      scribe %in% c("josh") ~ "JL",
      scribe %in% c("Susanne") ~ "SB",
      scribe %in% c("Nadine") ~ "NA",
      scribe %in% c("np/de") ~ "NP/DE",
      scribe %in% c("Susanne/Nadine") ~ "SB/NA",
      scribe %in% c("Dagmar, Nadine, Susanne") ~ "DE/NA/SB",
      scribe %in% c("Cris", "cris", "C", "c") ~ "C",
      scribe %in% c("jana") ~ "JR",
      scribe %in% c("cris/jana") ~ "C/JR",
      scribe %in% c("Nadine/Julia") ~ "NA/JS",
      TRUE ~ scribe
    )
  )

# Replace "NULL" values with NA
community_data_raw <- community_data_raw |> 
  mutate(
    across(
      c(recorder, scribe), 
      ~ if_else(str_detect(.x, "NULL"), NA_character_, .x)
    )
  )


# make column collector out of recorder and scribe
community_data_raw$collector <- paste(community_data_raw$recorder, 
                                      community_data_raw$scribe,
                                      sep = "/")
  
# if the same keep only one time
community_data_raw <- community_data_raw |> 
  mutate(collector = case_when(collector == "NP/NP" ~ "NP",
                               collector == "VV/VV" ~ "VV",
                               collector == "DE/DE" ~ "DE",
                               collector == "SB/SB" ~ "SB",
                               collector == "NA/NA" ~ "NA",
                               collector == "NP/DE/NP/DE" ~ "NP/DE",
                               collector == "NA/NA/JS" ~ "NA/JS",
                               collector == "NA/DE/NA" ~ "NA/DE",
                               collector == "VV/NA/NA/VV" ~ "VV/NA",
                               collector == "NA/SB/SB/NA" ~ "NA/SB",
                               collector == "DE/NA/SB/DE/NA/SB" ~ "DE/NA/SB",
                          TRUE ~ collector))

unique(community_data_raw$collector)


# merge metadata with all years veg data ----------------------------------

community_data_raw_NOR <- left_join(community_data_raw, metadata_NOR_com,
                                    by = c("site", "block_id_original", 
                                           "plot_id_original"))
  


# delete obviously wrong species names ----------------------------------------------
species <- sort(unique(community_data_raw_NOR$species))
species

community_data_raw_NOR <- community_data_raw_NOR |> 
  filter(species != "Cover per subplot") |> 
  filter(species != "Cover per plot") |> 
  filter(species != "Tomst") |> 
  filter(species != "tomst")


# dealing with f = fertile in 2023 data -------------------------------------------
# make new column
# community_data_raw_NOR <- community_data_raw_NOR |> 
#   mutate(reproductive_capacity = if_else(str_detect(cover, "f"), 1, 0),
#     cover = str_remove(cover, "f") # Optional: Remove the "f" from `cover`
#   )


# check NAs in unique_plot_id ---------------------------------------------

community_data_NA <- community_data_raw_NOR |> 
  filter(is.na(unique_plot_id))

sum(is.na(community_data_raw_NOR$unique_plot_id)) # 0


# fix Chinese reading style of 2023 ---------------------------------------






# turfmapper --------------------------------------------------------------
# lange tabelle mit jahr species cover subturf presence 

community_data_raw_NOR_long <- community_data_raw_NOR |> 
  pivot_longer(
    cols = "1":"16",
    names_to = "subturf",
    values_to = "cover") |>
  filter(cover != 0) |> 
  mutate(reproductive_capacity = if_else(str_detect(cover, "f"), 1, 0),
         cover = str_remove(cover, "f")) |> 
  mutate(subturf = as.numeric(subturf)) |> 
  mutate(cover = as.numeric(cover)) 
  

community_data_raw_NOR_long <- community_data_raw_NOR |>
  pivot_longer(
    cols = "1":"16",
    names_to = "subturf",
    values_to = "cover") |>
  mutate(subturf = as.numeric(subturf)) |> 
  mutate(cover = as.numeric(cover)) |> 
  filter(cover != 0)  # only want presences  
  # mutate(reproductive_capacity = if_else(str_detect(cover, "f"), 1, 0),
  #         cover = str_remove(cover, "f")) 
  # mutate(subturf = as.numeric(subturf)) |> 
  # mutate(cover = as.numeric(cover)) # make cover numeric

# set up subturf grid
grid <- make_grid(ncol = 4)

# plot NOR.hi.warm.vege.wf.01
community_data_raw_NOR_long |>
  mutate(subturf = as.numeric(subturf)) |> 
  filter(unique_plot_id %in% c("NOR.hi.warm.vege.wf.01")) |> 
  # mutate(subturf = as.numeric(subturf)) |> 
  # mutate(cover = as.numeric(cover)) |> 
  make_turf_plot(
    data = _,
    year = year, species = species, cover = cover, subturf = subturf,
    site_id = site,
    turf_id = unique_plot_id,
    grid_long = grid
  )


# plot NOR.lo.ambi.vege.wf.01
community_data_raw_NOR_long |>
  mutate(subturf = as.numeric(subturf)) |> 
  filter(unique_plot_id %in% c("NOR.hi.warm.vege.wf.02")) |> 
  # mutate(subturf = as.numeric(subturf)) |> 
  # mutate(cover = as.numeric(cover)) |> 
  make_turf_plot(
    data = _,
    year = year, species = species, cover = cover, subturf = subturf,
    site_id = site,
    turf_id = unique_plot_id,
    grid_long = grid
  )



# loops through all plots -------------------------------------------------

x <- CommunitySubplot %>%
  mutate(subplot = as.numeric(subplot),
         year_recorder = paste(year, recorder, sep = "_")) %>%
  select(-year) %>%
  arrange(destSiteID, destPlotID, turfID) %>%
  group_by(destSiteID, destPlotID, turfID) %>%
  nest() %>%
  {map2(
    .x = .$data,
    .y = glue::glue("Site {.$destSiteID}: plot {.$destPlotID}: turf {.$turfID}"),
    .f = ~make_turf_plot(
      data = .x,
      year = year_recorder,
      species = species,
      cover = cover,
      subturf = subplot,
      title = glue::glue(.y),
      grid_long = grid)
  )} %>%
  walk(print)











# filter by 2021 and change <1 with 1 --------------------------------------------------------
com_cov_21 <- community_data_raw |> 
  filter(year == "2021") |> 
  mutate(across(c(19:34), as.numeric)) |>  # not total cover
  # mutate(total_cover = case_when(total_cover == "<1" ~ "1",
  #                                TRUE ~ total_cover)) |> 
  # mutate(across(c(35:36), as.numeric))
  
  # actually leave it as <1, CHE has that as well
  
  
  # filter by 2023 --------------------------------------------------------
com_cov_23 <- community_data_raw |> 
  filter(year == "2023")



# merge metadata with veg data 21 ---------------------------------------------------
rx_com_cov_21 <- left_join(com_cov_21, metadata_NOR_com,
                           by = c("site", "block_id_original", 
                                  "plot_id_original"))

# merge metadata with veg data 23 ---------------------------------------------------
rx_com_cov_23 <- left_join(com_cov_23, metadata_NOR_com,
                           by = c("site", "block_id_original", 
                                  "plot_id_original"))

# keep only relevant columns for OSF --------------------------------------

rx_com_cov_raw_21 <- rx_com_cov_21 |> 
  select(unique_plot_id, date, species, total_cover,
         collector, added_focals) |> 
  rename("date_measurement" = "date",
         "cover" = "total_cover")








