
# Seeds and seed germination NOR --------------------------------------------

## Data used: RangeX_raw_seeds_germination_even_NOR_2023.xlsx,
##            RangeX_raw_seeds_germination_uneven_NOR_2023.xlsx
## Date:      05.02.2025
## Author:    Nadine Arzt
## Purpose:   Clean seed data


# comments ----------------------------------------------------------------
# weight_even: number_infructescence in even? Don't understand. What is difference to flowers
# weight_odd: and odd doesn't have number_infructescence
# weight_odd: date in odd: not transfered correct
# weight_odd: not sure what why we have 2 seeds_number_4



# load library ------------------------------------------------------------
library(conflicted)
conflict_prefer_all("dplyr", quiet = TRUE)
library(tidyverse)
library(openxlsx)


# import data -------------------------------------------------------------
seed_weight_odd <- read.xlsx("Data/Data_seeds/RangeX_raw_seed_weight_odd_NOR_2023.xlsx", sheet = 1)

seed_weight_even <- read.xlsx("Data/Data_seeds/RangeX_raw_seed_weight_even_NOR_2023.xlsx", sheet = 1)

seed_germi_odd <- read.xlsx("Data/Data_seeds/RangeX_raw_seeds_germination_odd_NOR_2023.xlsx")

seed_germi_even <- read.xlsx("Data/Data_seeds/RangeX_raw_seeds_germination_even_NOR_2023.xlsx", sheet = 1)

metadata <- read.csv("Data/RangeX_metadata_focal_NOR.csv")



# merge odd and even data sets ------------------------------------------
names(seed_weight_odd)
names(seed_weight_even)


# rename column names to match --------------------------------------------

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

# delete "seeds", "weight.2", "collected.2" because empty      
seed_weight_odd <- seed_weight_odd %>%
  select(where(~ any(!is.na(.))))


seed_weight_even <- seed_weight_even |> 
  rename("positionID_old" = "positionID",
         "positionID" = "positionID_clean")



# clean position ID in odd ------------------------------------------------

seed_weight_odd$positionID

seed_weight_odd <- seed_weight_odd %>%
  mutate(
    positionID_old = str_extract(positionID, "\\(.*\\)"),  # Extract text in brackets
    positionID = str_trim(str_remove(positionID, "\\s*\\(.*\\)"))  # Remove brackets and clean spaces
  )





# merge seed weight with metadata -----------------------------------------

seed_data <- left_join(metadata, seed_weight_even, by = c("site"))


























