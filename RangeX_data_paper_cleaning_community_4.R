
# RangeX community cover data cleaning 3  -------------------------------

## Data used: RangeX_clean_VegSurveyGeneral_21_22_23_NOR.csv
## Date:      10.12.2025
## Author:    Nadine Arzt
## Purpose:   Fixing some more things, also based on decicions from 2025

# load library ------------------------------------------------------------
library(conflicted)
conflict_prefer_all("dplyr", quiet = TRUE)
library(tidyverse)


# import vege data nor general --------------------------------------------
vege <- read.csv("Data/Data_community/CleanVegSurvey/RangeX_clean_VegSurveyGeneral_21_22_23_NOR.csv")


# fix <1 and X ------------------------------------------------------------
vege_nor <- vege |>
  mutate(
    cover = case_when(
      cover == "<1" ~ "0.5",
      cover == "X"  ~ "1",
      cover == "12+R2:R25" ~ "12",
      TRUE ~ cover
    ),
    cover = as.numeric(cover)
  )

# remove rows with 0 in cover ---------------------------------------------
vege_nor <- vege_nor |>
  filter(cover != 0)


# save clean data cover plot ----------------------------------------------
#write.csv(vege_nor, "Data/Data_community/CleanVegSurvey/RangeX_clean_VegSurveyGeneral_21_22_23_NOR.csv", row.names = FALSE)
veg_survey_general <- read_csv("Data/Data_community/CleanVegSurvey/RangeX_clean_VegSurveyGeneral_21_22_23_NOR.csv")

































