
# Climate data TOMST loggers NOR 2021-2024 data combination ------------------------------

## Data used: RangeX_clean_EnvTMS4_2021_NOR.csv
##            RangeX_clean_EnvTMS4_2022_NOR.csv
##            RangeX_clean_EnvTMS4_2023_NOR.csv
##            RangeX_clean_EnvTMS4_2024_NOR.csv
## Date:      21.11.2025
## Author:    Nadine Arzt
## Purpose:   Combine TOMST logger data 2021 - 2024

# load library ------------------------------------------------------------
library(conflicted)
conflict_prefer_all("dplyr", quiet = TRUE)
library(tidyverse)


# Import TMS data 2021-24 -----------------------------------------------------
tms21 <- read.csv("Data/Data_tomst_loggers/CleanEnvTMS4/RangeX_clean_EnvTMS4_2021_NOR.csv")
tms22 <- read.csv("Data/Data_tomst_loggers/CleanEnvTMS4/RangeX_clean_EnvTMS4_2022_NOR.csv")
tms23 <- read.csv("Data/Data_tomst_loggers/CleanEnvTMS4/RangeX_clean_EnvTMS4_2023_NOR.csv")
tms24 <- read.csv("Data/Data_tomst_loggers/CleanEnvTMS4/RangeX_clean_EnvTMS4_2024_NOR.csv")



# combine all years into one file -----------------------------------------
EnvTMS4 <- bind_rows(tms21, tms22, tms23, tms24)


# save clean data ---------------------------------------------------------
write.csv(EnvTMS4, "Data/Data_tomst_loggers/CleanEnvTMS4/RangeX_clean_EnvTMS4_2021-24_NOR.csv", row.names = FALSE)














