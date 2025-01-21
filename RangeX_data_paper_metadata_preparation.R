
# Prepare and fix metadata NOR --------------------------------------------

## Data used: RangeX_Metadata.csv
## Date:      21.01.2025
## Author:    Nadine Arzt
## Purpose:   Preparing meta data for NOR

# load library ------------------------------------------------------------
library(conflicted)
conflict_prefer_all("dplyr", quiet = TRUE)
library(tidyverse)
library(janitor)


# load metadata old -----------------------------------------------------------
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



# add columns -------------------------------------------------------------
new_columns <- c("date_planting", "functional_group", "ind_number")

metadata_NOR[ , new_columns] <- NA


# fill in planting date ---------------------------------------------------

metadata_NOR <- metadata_NOR |> 
  mutate(date_planting = case_when(site == "lo" ~ "05.07.2021",
                                   site == "hi" ~ "28.06.2021", 
                                   TRUE ~ NA_character_ ),
         date_planting = as.Date(date_planting, format = "%d.%m.%Y"))




# add functional group ----------------------------------------------------
unique(metadata_NOR$species)

metadata_NOR <- metadata_NOR |> 
  mutate(functional_group = case_when(species == "sucpra" ~ "forb",
                                      species == "cennig" ~ "forb",
                                      species == "cyncri" ~ "graminoid", 
                                      species == "pimsax" ~ "forb", 
                                      species == "luzmul" ~ "graminoid", 
                                      species == "plalan" ~ "forb", 
                                      species == "sildio" ~ "forb", 
                                      species == "leuvul" ~ "forb", 
                                      species == "tripra" ~ "legume", 
                                      species == "hypmac" ~ "forb",
                                      TRUE ~ NA_character_ ))


# add ind_number ----------------------------------------------------------
# nothing was replanted so all get a 1
metadata_NOR <- metadata_NOR |> 
  mutate(ind_number = 1)


# make correct order ------------------------------------------------------
metadata_NOR <- metadata_NOR |> 
  select(date_planting, region, site, block_id_original, plot_id_original,
         position_id_original, species, functional_group, treat_warming, 
         treat_competition, added_focals, block_id, position_id, ind_number,
         unique_plot_id, unique_plant_id)


# save clean meta data file for focals ------------------------------------
# write.csv(metadata_NOR, file =  "Data/RangeX_metadata_focal_NOR.csv")



# metadata file plot level ------------------------------------------------

# have all 60 plots once
unique(metadata_NOR$unique_plot_id)

metadata_NOR_plot <- metadata_NOR |> 
  distinct(unique_plot_id, .keep_all = TRUE)

unique(metadata_NOR_plot$unique_plot_id)


# add metadata for c and d control plots ----------------------------------
control_plots <- expand.grid(
  region = "NOR",
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
      region, site, ".", treat_warming, ".", treat_competition, ".", added_focals, ".",
      sprintf("%02d", block_id) # Format block_id as two digits
    ),
    block_id_original = as.integer(block_id_original)
  )

names(control_plots)

# correct order
control_plots <- control_plots |> 
  select(region, site, block_id_original, plot_id_original, treat_warming, 
         treat_competition, added_focals, block_id, unique_plot_id)


# join with metadata
metadata_NOR_plot_clean <- bind_rows(metadata_NOR_plot, control_plots)



# filter only needed columns ----------------------------------------------

metadata_NOR_plot_clean <- metadata_NOR_plot_clean |> 
  select(region, site, block_id_original, plot_id_original, treat_warming, 
         treat_competition, added_focals, block_id, unique_plot_id)



# order plot_id_original correct ------------------------------------------

metadata_NOR_plot_clean <- metadata_NOR_plot_clean |> 
  arrange(site,
          block_id_original,
          match(plot_id_original, c("a", "b", "c", "d", "e", "f")) 
    )


# save clean metadata NOR plot --------------------------------------------
# write.csv(metadata_NOR_plot_clean, file =  "Data/RangeX_metadata_plot_NOR.csv")












