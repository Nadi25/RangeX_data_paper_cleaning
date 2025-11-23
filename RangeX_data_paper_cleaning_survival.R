

# Peak survival 2021-23 ---------------------------------------------------

## Data used: RangeX_clean_YearlyDemographics_2021_2022_2023_NOR.csv 
## Date:      02.11.25
## Author:    Nadine Arzt
## Purpose:   create peak survival dataset


# comments ----------------------------------------------------------------
# NOR.hi.warm.vege.wf.07.09.1, hi 7a e4 2021
# NOR.hi.warm.vege.wf.07.06.1 --> 2021 f9-f3
# NOR.hi.warm.vege.wf.07.29.1 survived for 4 years?? 7a f9 cyncri




# load packages -----------------------------------------------------------
library(dplyr)
library(tidyr)


# import metadata ---------------------------------------------------------
metadata <- read.csv("Data/Metadata/RangeX_clean_MetadataFocal_NOR.csv")


# import YearlyDemographics data ------------------------------------------
traits_demo_nor <- read.csv("Data/Data_demographic_traits/Clean_YearlyDemographics/RangeX_clean_YearlyDemographics_2021_2022_2023_NOR.csv")


# create survival dataset -------------------------------------------------
survival <- traits_demo_nor |> 
  select(unique_plant_ID, species, date_measurement, date_planting, 
         collector, survival) |> 
  mutate(variable = "peak_survival") |> 
  rename(value = survival)



# correct order -----------------------------------------------------------
survival <- survival |> 
  select(unique_plant_ID, species, date_measurement, date_planting,
         collector, variable, value)





# check survival over all three years -------------------------------------
survival_summary <- survival |> 
  mutate(year = year(date_measurement)) |> 
  group_by(unique_plant_ID) |> 
  summarise(
    n_years_recorded = n_distinct(year),
    total_survival = sum(value, na.rm = TRUE),
    years = paste(sort(unique(year)), collapse = ", ")) |> 
  arrange(n_years_recorded)




# check survival per year -------------------------------------------------
survival_summary2 <- survival |>
  mutate(year = year(date_measurement)) |>
  group_by(unique_plant_ID) |>
  summarise(
    n_years_recorded = n_distinct(year),
    total_survival = sum(value, na.rm = TRUE),
    years_recorded = paste(sort(unique(year)), collapse = ", "),
    survived_years = paste(sort(unique(year[value == 1])), collapse = ", "),
    missing_years = paste(sort(setdiff(2021:2023, unique(year))), collapse = ", ")) |>
  arrange(n_years_recorded)



# save as csv for OSF  -------------------------------------------------------------
write.csv(survival, "Data/Data_survival/Clean_survival/RangeX_clean_Survival_2021_2022_2023_NOR.csv", row.names = FALSE)

survival_nor <- read.csv("Data/Data_survival/Clean_survival/RangeX_clean_Survival_2021_2022_2023_NOR.csv")





