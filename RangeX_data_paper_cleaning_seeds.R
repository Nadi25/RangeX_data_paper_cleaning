
# Seeds and seed germination NOR --------------------------------------------

## Data used: RangeX_raw_seeds_germination_even_NOR_2023.xlsx,
##            RangeX_raw_seeds_germination_uneven_NOR_2023.xlsx
## Date:      05.02.2025
## Author:    Nadine Arzt
## Purpose:   Clean seed data


# comments ----------------------------------------------------------------
# weight_even: number_infructescence in even? Don't understand. What is difference to flowers
# weight_even: flowers: how many infructescences = flowers collected separately, e.g. 3
# weight_even: number_infructescence: how many infructescences per filter, e.g. 2
# problem: weight_odd: and odd doesn't have number_infructescence

# weight_odd: date in odd: not transferred correct -> fixed
# weight_odd: not sure what why we have 2 seeds_number_4
# weight_odd: FYG5543: e6 (and c4?)	3 of e6, 2 of other?
# position_ID_old: means that it is a different plant than the one we have functional traits on



# load library ------------------------------------------------------------
library(conflicted)
conflict_prefer_all("dplyr", quiet = TRUE)
library(tidyverse)
library(openxlsx)


# import data -------------------------------------------------------------
# seed_weight_odd <- read.xlsx("Data/Data_seeds/RangeX_raw_seed_weight_odd_NOR_2023.xlsx", sheet = 1)
seed_weight_odd <- read.csv2("Data/Data_seeds/RangeX_raw_seed_weight_odd_NOR_2023.csv")

seed_weight_even <- read.xlsx("Data/Data_seeds/RangeX_raw_seed_weight_even_NOR_2023.xlsx", sheet = 1)

# seed_germi_odd <- read.xlsx("Data/Data_seeds/RangeX_raw_seeds_germination_odd_NOR_2023.xlsx")
# 
# seed_germi_even <- read.xlsx("Data/Data_seeds/RangeX_raw_seeds_germination_even_NOR_2023.xlsx", sheet = 1)

metadata <- read.csv("Data/RangeX_metadata_focal_NOR.csv")
metadata <- metadata |> 
  select(-"X")


# merge odd and even data sets ------------------------------------------
names(seed_weight_odd)
names(seed_weight_even)


# rename column names in weight even to match odd --------------------------------------------

# Rename columns automatically to make them unique
colnames(seed_weight_odd) <- make.names(colnames(seed_weight_odd), unique = TRUE)

seed_weight_odd <- seed_weight_odd |> 
  rename("flowers" = "X.flowers",
         "seeds_number_1" = "X.seeds",
         "weight_1" = "weight.in.gram",
         "date_agar_1" = "date.in.agar.fridge",
         "collected_1" = "collected.from.field",
         "comment_1" = "comment",
         "seeds_number_2" = "X.seeds.1",
         "weight_2" = "weight",
         "date_agar_2" = "date.in.agar.fridge.1",
         "collected_2" = "collected",
         "comment_2" = "comment.1",
         "seeds_number_3" = "X.seeds.2",
         "weight_3" = "weight.1",
         "date_agar_3" = "date.in.agar.fridge.2",
         "collected_3" = "collected.1",
         "comment_3" = "comment.2",
         "seeds_number_4" = "seeds",
         "weight_4" = "weight.2",
         "collected_4" = "collected.2")

colnames(seed_weight_odd)
# not sure what why we have 2 seeds_number_4

# delete "seeds_number_4", "weight_4", "collected_4"? not actually empty!    
seed_weight_odd <- seed_weight_odd |> 
  select(where(~ any(!is.na(.))))

# delete empty rows
seed_weight_odd <- seed_weight_odd |> 
  filter(if_any(everything(), ~ !is.na(.) & . != ""))
# 303 rows


# clean position ID in odd ------------------------------------------------

seed_weight_odd$positionID

seed_weight_odd <- seed_weight_odd %>%
  mutate(
    positionID_old = str_extract(positionID, "\\(.*\\)"),  # Extract text in brackets
    positionID = str_trim(str_remove(positionID, "\\s*\\(.*\\)"))  # Remove brackets and clean spaces
  )


# fix date columns ----------------------------------------------------------------
# not the same format
seed_weight_odd$date_agar_1

# Define a function to clean and standardize dates
clean_dates <- function(date_column) {
  case_when(
    str_detect(date_column, "^\\d{1,2}\\.\\d{1,2}\\.\\d{2,4}$") ~ dmy(date_column),  # Full format (DD.MM.YYYY)
    str_detect(date_column, "^\\d{1,2}\\.\\d{1,2}$") ~ {  
      # Extract day and month
      parts <- str_split_fixed(date_column, "\\.", 2)
      day <- as.numeric(parts[, 1])
      month <- as.numeric(parts[, 2])
      
      # Assign correct year based on the month
      date_final <- make_date(
        year = ifelse(month >= 12, 2023, 2024),  # December and earlier → 2023, otherwise → 2024
        month = month,
        day = day
      )
      
      date_final
    },
    date_column == "" ~ NA_Date_,  # Convert empty strings to NA
    TRUE ~ as.Date(NA)  # Handle unexpected cases
  )
}

# apply function to all date columns
seed_weight_odd <- seed_weight_odd %>%
  mutate(across(c(date_agar_1, date_agar_2, date_agar_3, 
                  collected_1, collected_2, collected_3, collected_4), clean_dates))

colnames(seed_weight_odd)


# clean names weight_even -------------------------------------------------
colnames(seed_weight_even)

seed_weight_even <- seed_weight_even |> 
  rename("positionID_old" = "positionID",
         "positionID" = "positionID_clean")

# delete calculation columns
seed_weight_even <- seed_weight_even |> 
  select(-c("weight.seed", "seeds_number.inf", "weight.inf"))





# merge seed weight with metadata -----------------------------------------

# find out which column names are not matching
names_odd <- colnames(seed_weight_odd)
names_even <- colnames(seed_weight_even)

intersect(names_odd, names_even)

# odd doesn't have "number_infructescence"
# delete for now


# keep only necessary columns for now -------------------------------------
rx_seed_weight_odd <- seed_weight_odd |> 
  select("ID", "date", "site", "species", "treat1",
         "treat2", "blockID", "plotID", "positionID", "flowers",
         "seeds_number_1", "weight_1", "date_agar_1", "collected_1", "comment_1",
         "seeds_number_2", "weight_2", "date_agar_2", "collected_2", "comment_2",
         "weight_3", "date_agar_3", "collected_3", "comment_3", "seeds_number_4",
         "weight_4", "collected_4")

rx_seed_weight_even <- seed_weight_even |> 
  select("ID", "date", "site", "species", "treat1",
         "treat2", "blockID", "plotID", "positionID", "flowers",
         "seeds_number_1", "weight_1", "date_agar_1", "collected_1", "comment_1",
         "seeds_number_2", "weight_2", "date_agar_2", "collected_2", "comment_2",
         "weight_3", "date_agar_3", "collected_3", "comment_3", "seeds_number_4",
         "weight_4", "collected_4")


# combine even and odd into one data set --------------------------------------------
seed_data <- rbind(rx_seed_weight_odd, rx_seed_weight_even)


# rename columns and match with metadata ----------------------------------------------------------
seed_data <- seed_data |> 
  rename("block_ID_original" = "blockID",
         "plot_ID_original" = "plotID",
         "position_ID_original" = "positionID",
         "treat_warming" = "treat1",
         "treat_competition" = "treat2")


# to match species
seed_data <- seed_data |> 
  mutate(species = case_when(
    species == "CC" ~ "cyncri",
    species == "SP" ~ "sucpra",
    species == "HM" ~ "hypmac",
    species == "CN" ~ "cennig",
    species == "LV" ~ "leuvul",
    species == "SD" ~ "sildio",
    species == "PL" ~ "plalan",
    species == "LM" ~ "luzmul",
    species == "TP" ~ "tripra",
    species == "PS" ~ "pimsax",
    TRUE ~ species  # Keep other species unchanged
  ))

# to match site
seed_data <- seed_data |> 
  mutate(site = case_when(
    site == "LS" ~ "lo",
    site == "HS" ~ "hi"
  ))

# to match plot_ID_original
seed_data <- seed_data |> 
  mutate(plot_ID_original = case_when(
    plot_ID_original == "A" ~ "a",
    plot_ID_original == "B" ~ "b",
    plot_ID_original == "C" ~ "c",
    plot_ID_original == "D" ~ "d",
    plot_ID_original == "E" ~ "e",
    plot_ID_original == "F" ~ "f"
  ))

# to match treat_warming
seed_data <- seed_data |> 
  mutate(treat_warming = case_when(
    site == "lo" & treat_warming == "" ~ "ambi",  # For site "lo" and empty cells
    treat_warming == "warm" ~ "warm",  
    treat_warming == "ambient" ~ "ambi" 
  ))


# merge with meta data ----------------------------------------------------
# need to be same, e.g. chr, to merge

metadata <- metadata |> 
  mutate(across(c(block_ID_original, plot_ID_original, position_ID_original), as.character))

seed_data <- seed_data %>%
  mutate(across(c(block_ID_original, plot_ID_original, position_ID_original), as.character))


rx_seed_raw <- left_join(metadata, seed_data, 
                         by = c( "site", "block_ID_original",
                                 "plot_ID_original", "position_ID_original", 
                                 "species", "treat_warming", "treat_competition"))


# add counter -------------------------------------------------------------
rx_seed_raw <- rx_seed_raw |> 
  mutate(counter = ifelse(block_ID_original %in% c(1, 3, 5, 7, 9),
                               "IE", "TD"))



# calculate weight for one seed -------------------------------------------
rx_seed_raw <- rx_seed_raw |> 
  mutate(across(c(weight_1, weight_2, weight_3, flowers), as.numeric))

rx_seed_raw <- rx_seed_raw |> 
  mutate(seedweight = (weight_1 + weight_2 + weight_3)/flowers)





# filter only necessary columns for OSF -----------------------------------

# unique_plant_ID,"species","date_collection","counter","inflorescence_size","no_seeds","seedweight"
























