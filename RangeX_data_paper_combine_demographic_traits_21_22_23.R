

## RangeX combine demographic trait data from 2021, 2022 and 2023

## Data used: RangeX_clean_traits_2021.csv,
##            RangeX_clean_traits_2022.csv,
##            RangeX_clean_traits_2023.csv 
## Date:      26.10.23
## Author:    Nadine Arzt
## Purpose:   combine traits data 2021 + 2022 + 2023

# load packages -----------------------------------------------------------
library(dplyr)
library(tidyr) # data manipulation
library(ggplot2)
library("openxlsx")

# load data ---------------------------------------------------------------
rangex_traits_2021 <- read.csv("Data/Data_demographic_traits/RangeX_clean_traits_2021.csv")
rangex_traits_2022 <- read.csv("Data/Data_demographic_traits/RangeX_clean_traits_2022.csv")
rangex_traits_2023 <- read.csv("Data/Data_demographic_traits/RangeX_clean_traits_2023.csv")

head(rangex_traits_2021)
head(rangex_traits_2022)
head(rangex_traits_2023)

## delete columns petiole_length2/3 and sam, 
## because they are not in the yearly demographics table
## might come back in later in further cleaning
rangex_traits_2021 <- rangex_traits_2021 %>% 
  dplyr::select(-petiole_length2, -petiole_length3, -sam) %>% 
  rename("petiole_length" = "petiole_length1")
#
rangex_traits_2022 <- rangex_traits_2022 %>% 
  dplyr::select(-petiole_length2, -petiole_length3, -sam) %>% 
  rename("petiole_length" = "petiole_length1")
#

# combine traits 21 and 22 ------------------------------------------------

# Check if rangex_traits_2021 is equal to rangex_traits_2022
isEqual2021and2022 <- all.equal(rangex_traits_2021, rangex_traits_2022)

# Check if rangex_traits_2021 is equal to rangex_traits_2023
isEqual2021and2023 <- all.equal(rangex_traits_2021, rangex_traits_2023)

# Check if rangex_traits_2022 is equal to rangex_traits_2023
isEqual2022and2023 <- all.equal(rangex_traits_2022, rangex_traits_2023)

# Check the results
if (is.logical(isEqual2021and2022) && isEqual2021and2022 &&
    is.logical(isEqual2021and2023) && isEqual2021and2023 &&
    is.logical(isEqual2022and2023) && isEqual2022and2023) {
  print("All three data frames are equal.")
} else {
  print("At least two of the data frames are not equal.")
}


## get column names
dput(colnames(rangex_traits_2021))
dput(colnames(rangex_traits_2022))
dput(colnames(rangex_traits_2023))


# combine 2021 + 22 + 23 --------------------------------------------------
rangex_traits_NOR <- rbind(rangex_traits_2021, rangex_traits_2022, rangex_traits_2023)
head(rangex_traits)



# names to initials -------------------------------------------------------
rangex_traits_NOR <- rangex_traits_NOR |>
  mutate(collector = case_when(
    collector %in% c("Dagmar") ~ "DE",
    collector %in% c("Nadine") ~ "NA",
    collector %in% c("Malo") ~ "MF", # Malo Le Fur
    collector %in% c("Dagmar_Nadine_Lizzy") ~ "DE/NA/ED",
    collector %in% c("DE, NA, MF") ~ "DE/NA/MF",
    TRUE ~ collector
  ))


# save as csv for google drive yearly demographics -------------------------------------------------------------

# write.csv(rangex_traits_NOR, "C:/Users/naart3294/OneDrive - University of Bergen/PhD_RangeX/R codes/RangeX_data_paper_cleaning/Data/Data_demographic_traits/RangeX_clean_yearly_size_2021_2022_2023_NOR.csv", row.names = FALSE)












