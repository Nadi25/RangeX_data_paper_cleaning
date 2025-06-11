
# RangeX community cover data cleaning 2  -------------------------------

## Data used: RangeX_raw_comcov_high_2021.xlsx, 
##            RangeX_raw_comcov_low_2021.xlsx,
##            RangeX_raw_comcov_high_2023.xlsx, 
##            RangeX_raw_comcov_low_2023.xlsx,
##            RangeX_metadata_plot_NOR.csv
## Date:      11.06.2025
## Author:    Nadine Arzt
## Purpose:   Cleaning of the complete raw data files of community 2021-2023


# check! ------------------------------------------------------------------
# 2021: is hi 3A and 3B switched? changed to b = NOR.hi.warm.vege.wf.03 and a = NOR.hi.ambi.vege.wf.03

# Circle grass = Festuca pratensis

# ?Galium: NOR.lo.ambi.vege.wf.06: lo 6A, 2021-08-30
# ?Melica nutans (grass 2 ): NOR.hi.warm.vege.wf.06, hi 6A: 2021-08-13

# Geranium sylvaticum in NOR.hi.ambi.vege.wf.03

# Cirsium helenoides and Cirsium arvense

# fix in community_data_raw_NOR_long to keep the subturf data and save clean version for only total cover later?

# library -----------------------------------------------------------------
library(openxlsx)

# source community cleaning script 1 --------------------------------------
source("RangeX_data_paper_cleaning_community.R")

community_data_clean_NOR


# import file with species names that need to be corrected ----------------
# this was created in script "Check_species_names_with_TNRS.R"
species_names_to_correct <- read.xlsx("Data/Data_community/Species_names_to_correct.xlsx")



# 1: ???
# NOR.hi.ambi.vege.wf.01 (hi, 1B) in 23


# 2: ?Antennaria dioica
# NOR.hi.ambi.vege.wf.04 only in 21
# maybe Omalotheca sylvatic?

community_data_clean_NOR_fixed <- community_data_clean_NOR |> 
  mutate(species = case_when(species == "?Antennaria dioica" & unique_plot_ID == "NOR.hi.ambi.vege.wf.04" ~ "Omalotheca sylvatica",
    TRUE ~ species))

# 3: ?Festuca rubra
# NOR.hi.warm.vege.wf.04: 2021-08-09
# NORhi.ambi.vege.nf.04: 2021-08-11 
# NOR.hi.ambi.vege.wf.05: 2021-08-11
# NOR.hi.warm.vege.nf.05: 2021-08-11
# NOR.hi.warm.vege.wf.04: 2022-07-19
# NOR.hi.warm.vege.nf.05: NA --> was not recorded in 2022
# wasn't recognized much in 2021
# will accept 

community_data_clean_NOR_fixed <- community_data_clean_NOR_fixed |> 
  mutate(species = case_when(
    species == "?Festuca rubra" ~ "Festuca rubra",
    TRUE ~ species
  ))

# 4: ?Galium
# NOR.lo.ambi.vege.wf.06: 2021-08-30
# could be Galium boreale or uliginosum
# more likely boreale because it has an occurrence in 
# both subplots (12, 16) in 23


# 5: 5 ?Hypochaeris radicata
# NOR.hi.ambi.vege.wf.09, hi 9B: 2021-08-21
# same subplots in 23
# so we believe it?
community_data_clean_NOR_fixed <- community_data_clean_NOR_fixed |> 
  mutate(species = case_when(
    species == "?Hypochaeris radicata" ~ "Hypochaeris radicata",
    TRUE ~ species
  ))

# 6: 	?Melica nutans (grass 2 )
# NOR.hi.warm.vege.wf.06, hi 6A: 2021-08-13
# could it be the same as Hairy poa/festuca? in 23?


# 7: ?Omalotheca
# NOR.hi.ambi.vege.wf.03, hi 3B: 2021-08-08
# there is also Omalotheca in 22 in same subplot after 3a and 3b have been switched as indicated on 22 datasheet
community_data_clean_NOR_fixed <- community_data_clean_NOR_fixed |> 
  mutate(species = case_when(
    species == "?Omalotheca" ~ "Omalotheca sylvatica",
    TRUE ~ species
  ))
# and Omalotheca to Omalotheca sylvatica in NOR.hi.ambi.vege.wf.03 in 22
community_data_clean_NOR_fixed <- community_data_clean_NOR_fixed |> 
  mutate(species = case_when(
    species == "Omalotheca" & unique_plot_ID == "NOR.hi.ambi.vege.wf.03" ~ "Omalotheca sylvatica",
    TRUE ~ species
  ))

# 8: ?Poa
# NOR.lo.ambi.vege.wf.01: 2021-09-01
# NOR.lo.ambi.vege.wf.01: 2022-07-21
# Poa pratensis is in other subplots

# 9: ?Pyrola
# NOR.hi.ambi.vege.wf.06: 2021-08-11 in subturf 9
# NOR.hi.ambi.vege.wf.06: 2022-07-19 in subturf 9
# 2023 has it in subplot 5 
# could be possible --> we accept
community_data_clean_NOR_fixed <- community_data_clean_NOR_fixed |> 
  mutate(species = case_when(
    species == "?Pyrola" & unique_plot_ID == "NOR.hi.ambi.vege.wf.06" ~ "Pyrola minor",
    TRUE ~ species
  ))










# 84: Circle grass = Festuca pratensis
community_data_clean_NOR_fixed <- community_data_clean_NOR_fixed |> 
  mutate(species = case_when(species == "Circle grass" ~ "Festuca pratensis",
                             TRUE ~ species))
















