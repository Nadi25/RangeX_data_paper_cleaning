

# RangeX community cover data cleaning  -------------------------------

## Data used: RangeX_raw_comcov_high_2021.xlsx, 
##            RangeX_raw_comcov_low_2021.xlsx,
##            RangeX_raw_comcov_high_2023.xlsx, 
##            RangeX_raw_comcov_low_2023.xlsx,
##            RangeX_metadata_plot_NOR.csv
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
library(glue)


# things to fix -----------------------------------------------------------
# it's Chinese style 2023-24
# change the subplots to match with 2021-22
# who is cris?
# who is JT?
# wrong spelled species: Hypericum maculata

# comments on sheets ----------------------------------------------------------------
## 21 high 2B: *Started using Agr cap (didn't in Blocks 3-6)...likely confused with Ant odo and Ple alp
## 21 high 2D: *Viola sp: mix of palustre and riviniana?


# import general metadata file -------------------------------------------------

metadata_plot <- read.csv("Data/RangeX_metadata_plot_NOR.csv", header = TRUE)

metadata_plot <- metadata_plot |> 
  select(region, site, block_id_original, plot_id_original, treat_warming, 
         treat_competition, added_focals, block_id, unique_plot_id)

# we don't need bare plots here
metadata_NOR_com <- metadata_plot |> 
  filter(treat_competition != "bare")
# 50 plots now

metadata_NOR_com <- metadata_NOR_com |> 
  mutate(block_id_original = as.character(block_id_original))


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


# turfmapper --------------------------------------------------------------
# lange tabelle mit jahr species cover subturf presence 
# make extra column "reproductive_capacity" with f in cover
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
  

# fix Chinese reading style of 2023 ---------------------------------------
# extra table with translation
match_21_23_subplots <- data.frame(
  subturf_2021 = 1:16,
  subturf_2023 = c(1, 5, 9, 13,   # Column 1
                   2, 6, 10, 14,  # Column 2
                   3, 7, 11, 15,  # Column 3
                   4, 8, 12, 16)  # Column 4
)

# Filter the 2023 data
community_data_raw_23 <- community_data_raw_NOR_long |> 
  filter(year == "2023")

# Join the mapping table to align the 2023 subturf with the 2021 layout
community_data_raw_23 <- community_data_raw_23 |> 
  left_join(match_21_23_subplots, by = c("subturf" = "subturf_2023")) |> 
  mutate(
    subturf = subturf_2021 # Replace 2023 subturf with 2021 equivalent
  ) |> 
  select(-subturf_2021) # Drop unnecessary columns

# Combine updated 2023 data with the rest of the dataset
community_data_raw_NOR_long <- community_data_raw_NOR_long |> 
  filter(year != "2023") |> # Exclude the old 2023 data
  bind_rows(community_data_raw_23) # Add the updated 2023 data


# community_data_raw_NOR_long <- community_data_raw_NOR |>
#   pivot_longer(
#     cols = "1":"16",
#     names_to = "subturf",
#     values_to = "cover") |>
#   mutate(subturf = as.numeric(subturf)) |> 
#   mutate(cover = as.numeric(cover)) |> 
#   filter(cover != 0)  # only want presences  
#   # mutate(reproductive_capacity = if_else(str_detect(cover, "f"), 1, 0),
#   #         cover = str_remove(cover, "f")) 
#   # mutate(subturf = as.numeric(subturf)) |> 
#   # mutate(cover = as.numeric(cover)) # make cover numeric

# set up subturf grid
grid <- make_grid(ncol = 4)


# test with plot NOR.hi.warm.vege.wf.01 -----------------------------------
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
# Open a single PDF document
pdf("Data/Data_community/Turfmapper_21_23_all_plots_comparison.pdf", width = 8, height = 12)

# Group, nest, and prepare data for plotting
nested_data <- community_data_raw_NOR_long |> 
  # mutate(
  #   year_collector = paste(year, collector, sep = "_") # Combine year and collector
  # ) |> 
  group_by(site, plot_id_original, unique_plot_id) |> # Group by plot-level identifiers only
  nest() |> 
  mutate(
    plot_title = glue("Site {site} : Turf {unique_plot_id} - Comparison 2021 vs 2023")
  )


# Loop through the plots and add them to the PDF
walk2(
  .x = nested_data$data,
  .y = nested_data$plot_title,
  .f = ~{
    # Ensure .x contains data for both years for the plot comparison
    if (length(unique(.x$year)) > 1) {
      plot <- make_turf_plot(
        data = .x,
        year = .x$year,  # Pass year_collector instead of just year
        species = .x$species,     # Pass the species column
        cover = .x$cover,         # Pass the cover column
        subturf = .x$subturf,     # Pass the subturf column
        title = .y,               # Use the title from glue
        grid_long = grid          # Assuming grid is predefined
      )
      print(plot) # Add the plot to the PDF
    }
  }
)

# Close the PDF device
dev.off()





# x <- CommunitySubplot %>%
#   mutate(subplot = as.numeric(subplot),
#          year_recorder = paste(year, recorder, sep = "_")) %>%
#   select(-year) %>%
#   arrange(destSiteID, destPlotID, turfID) %>%
#   group_by(destSiteID, destPlotID, turfID) %>%
#   nest() %>%
#   {map2(
#     .x = .$data,
#     .y = glue::glue("Site {.$destSiteID}: plot {.$destPlotID}: turf {.$turfID}"),
#     .f = ~make_turf_plot(
#       data = .x,
#       year = year_recorder,
#       species = species,
#       cover = cover,
#       subturf = subplot,
#       title = glue::glue(.y),
#       grid_long = grid)
#   )} %>%
#   walk(print)



# keep only relevant columns for OSF --------------------------------------
community_data_clean_NOR <- community_data_raw_NOR |> 
  select(unique_plot_id, date, species, total_cover,
         collector, added_focals)|> 
  rename("date_measurement" = "date",
         "cover" = "total_cover")


# there is a lot of plots where it's more then one day
# 

# fix date ----------------------------------------------------------------
# these need fixing
# 16.08.2023 - 17.08.2023
# 17.08.23 - 18.08.23
# 26.07.23/14.08.23
# 14.08./16.08.23
# 22.08./23.08.23
# 18.08./19.08.23
# 22.08/23.08.23
# 21.08/22.08.23
# 23.08./24.08.23
# 24.08./28.08.23
# 23.07./25.07./26.07.23
# 28.08./29.08.23
# 29.08./30.08.23
# 4.8./9.8. 23
# 3.8./4.8.23


community_data_clean_NOR <- community_data_clean_NOR |> 
  mutate(date_measurement = as.character(date_measurement)) |> 
  mutate(date_measurement = case_when(
    date_measurement ==  "16.08.2023 - 17.08.2023" ~ "17.08.2023",
    date_measurement ==  "17.08.23 - 18.08.23" ~ "18.08.2023",
    date_measurement ==  "16.08.2023 - 17.08.2023" ~ "17.08.2023",
    date_measurement ==  "26.07.23/14.08.2" ~ "14.08.2023",
    date_measurement ==  "14.08./16.08.23" ~ "16.08.2023",
    date_measurement ==  "22.08./23.08.23" ~ "23.08.2023",
    date_measurement ==  "21.08/22.08.23" ~ "22.08.2023",
    date_measurement ==  "23.08./24.08.23" ~ "24.08.2023",
    date_measurement ==  "24.08./28.08.23" ~ "28.08.2023",
    date_measurement ==  "23.07./25.07./26.07.23" ~ "26.07.2023",
    date_measurement ==  "28.08./29.08.23" ~ "29.08.2023",
    date_measurement ==  "29.08./30.08.23" ~ "30.08.2023",
    date_measurement ==  "4.8./9.8. 23" ~ "09.08.2023",
    date_measurement ==  "3.8./4.8.23" ~ "04.08.2023",
    TRUE ~ date_measurement
  )) |> 
  mutate(date_measurement = as.Date(date_measurement))

unique(community_data_clean_NOR$date_measurement)

# cover should stay character because of <1 
# or should we change <1 to 1? or 0.1?



# only focals column ------------------------------------------------------
species <- sort(unique(community_data_clean_NOR$species))
species

# added focals is in general in the plot
# only focals says if the cover is only based on planted individuals 
# of a focal or if there are naturally occurring individuals taken into 
# account as well

# we distinguished between focal and wild in the field
# focals are indicated with *
# therefore all focal should be yes and all others NA

# change column name to only_focals

community_data_clean_NOR <- community_data_clean_NOR |> 
  rename("only_focals" = "added_focals")

community_data_clean_NOR <- community_data_clean_NOR |> 
  mutate(only_focals = case_when(species == "Cynosurus cristatus*" ~ "yes",
                                 species == "Centaurea nigra*" ~ "yes",
                                 species == "Hypericum maculatum*" ~ "yes",
                                 species == "Leucanthemum vulgaris*" ~ "yes",
                                 species == "Luzula multiflora*" ~ "yes",
                                 species == "Pimpinella saxifraga*" ~ "yes",
                                 species == "Plantago lanceolata*" ~ "yes",
                                 species == "Silene dioica*" ~ "yes",
                                 species == "Succisa pratensis*" ~ "yes",
                                 species == "Trifolium pratense*" ~ "yes",
                                 TRUE ~ NA_character_))





# prepare species data set to check spelling of species names -----------------------------------------

species_to_check <- community_data_clean_NOR |> 
  select(species) |> 
  distinct(species) |> 
  arrange(species)

# add column with unique identifier = row number
species_to_check <- species_to_check |> 
  mutate(ID = row_number())

species_to_check <- species_to_check |> 
  select(ID, species) |> 
  rename("species_submitted" = "species")


# save species to check  --------------------------------------------------

# write.csv(species_to_check, "Data/Data_community/Species_names_to_check.csv")




# https://bien.nceas.ucsb.edu/bien/tools/tnrs/tnrs-api/
# https://github.com/ojalaquellueva/TNRSapi/blob/master/example_scripts/tnrs_api_example.R


