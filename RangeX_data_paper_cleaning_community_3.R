
# RangeX community cover data cleaning 3  -------------------------------

## Data used: RangeX_clean_VegSurveyGeneral_21_22_23_NOR.csv
## Date:      10.12.2025
## Author:    Nadine Arzt
## Purpose:   Fixing some more things

# load library ------------------------------------------------------------
library(conflicted)
conflict_prefer_all("dplyr", quiet = TRUE)
library(tidyverse)


# import vege data nor general --------------------------------------------
vege_nor <- read.csv("Data/Data_community/CleanVegSurvey/RangeX_clean_VegSurveyGeneral_21_22_23_NOR.csv")


# fix <1 and X ------------------------------------------------------------
vege_nor <- vege_nor |>
  mutate(
    cover = case_when(
      cover == "<1" ~ "0.5",
      cover == "X"  ~ NA_character_,
      TRUE ~ cover
    ),
    cover = as.numeric(cover)
  )





































