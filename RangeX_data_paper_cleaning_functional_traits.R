

# RangeX data combine functional traits with leaf area and clean raw data file----------------------------------

## Data used: RangeX_raw_functional_traits_2023.csv, 
##            RangeX_raw_functional_traits_leaf_area_NOR_2023.csv,
##            RangeX_Metadata.csv 
## Date:      11.11.24
## Author:    Nadine Arzt
## Purpose:   Combine functional traits with leaf area 
##            and clean the complete raw data file of functional traits


# Questions ---------------------------------------------------------------

# 1. delete rows with NAs, so that data set is only 600 plants not 1800?
# 2. delete outliers? How to define them
# 3. why does functional_traits_NOR_23 has 598 rows and when I delete 
# rows with na in date of functional_leaf_traits_NOR_23 it has 568?
# 4. GCW2731 hi luzmul warm bare 5e g2 is not from this plot --> check 
# 5. GHZ1788 (warm bare) and GAV1827 (ambi bare): hypmac hi 2e h5 twice --> check 
# 6. GID4537 hi 2f i6 leuvul warm bare --> was corrected to ambi --> check 
#   GAM4208 hi 2f i6 leuvul ambi bare
# 7. FYP9768 hi sildio warm bare 2f c8 --> was corrected to ambi --> check 
#   GAT6555 hi sildio ambi bare 2f c8
# 8. hypmac: some have small extra leaves --> reweigh + recolor?
# 9. hypmac: what if part of stem is not cut?
# 10. make folder without overlapping scans for OSF? --> do leaf area again?

# load packages -----------------------------------------------------------

library(tidyverse)
library(conflicted)
conflict_prefer_all("dplyr", quiet = TRUE)

# load data 2023 functional traits  ---------------------------------------------------------------
functional_traits <- read.csv2("Data/RangeX_raw_functional_traits_2023.csv")
head(functional_traits)

# check structure of data set ---------------------------------------------
str(functional_traits)
length(functional_traits) ## 18 columns

## get column names
dput(colnames(functional_traits))

# delete empty rows
## remove rows at the end, which have only NAs or nothing in it
functional_traits <- functional_traits |> 
  drop_na(blockID)
length(functional_traits$blockID) # 611 rows

## but why, it should be only 600



# delete duplicates that have no dry mass -----------------------------------
# comment: accidentally sampled 2, in fridge
# no dry mass for 5 samples

functional_traits <- functional_traits |> 
  filter(!is.na(dry_mass))
# 606 rows


# import leaf area data ---------------------------------------------------

leaf_area_NOR <- read.csv("Data/RangeX_raw_functional_traits_leaf_area_all.csv")
leaf_area_NOR


# Join leaf area to rest of functional traits -----------------------------

functional_traits_NOR <- left_join(functional_traits, leaf_area_NOR,
                                  by = "ID")

functional_traits_NOR

# check structure of data set ---------------------------------------------
str(functional_traits_NOR)
length(functional_traits_NOR) ## 22 columns

## get column names
dput(colnames(functional_traits_NOR))

# need to find out why 606 leaves but 600 scans
# 6 leaves have no leaf area
# FPI1781, FMQ6120, FSK5791, GDR8049 are in "bad_scans" folder because these scans stopped the code

# GAS2139 and GCX6229 have no scan


# rename columns and match with metadata ----------------------------------------------------------

# to match metadata
functional_traits_NOR <- functional_traits_NOR |> 
  rename("block_ID_original" = "blockID",
         "plot_ID_original" = "plotID",
         "position_ID_original" = "positionID",
         "treat_warming" = "treat1",
         "treat_competition" = "treat2")


# to match species
functional_traits_NOR <- functional_traits_NOR |> 
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
functional_traits_NOR <- functional_traits_NOR |> 
  mutate(site = case_when(
    site == "LS" ~ "lo",
    site == "HS" ~ "hi"
  ))

# to match plot_ID_original
functional_traits_NOR <- functional_traits_NOR |> 
  mutate(plot_ID_original = case_when(
    plot_ID_original == "A" ~ "a",
    plot_ID_original == "B" ~ "b",
    plot_ID_original == "C" ~ "c",
    plot_ID_original == "D" ~ "d",
    plot_ID_original == "E" ~ "e",
    plot_ID_original == "F" ~ "f"
  ))

# to match treat_warming
functional_traits_NOR <- functional_traits_NOR |> 
  mutate(treat_warming = case_when(
    site == "lo" & treat_warming == "" ~ "ambi",  # For site "lo" and empty cells
    treat_warming == "warm" ~ "warm",  
    treat_warming == "ambient" ~ "ambi" 
  ))



# why 606 samples? --------------------------------------------------------

# Create a unique identifier by combining the relevant columns
functional_traits_NOR$unique_id <- paste(functional_traits_NOR$site,
                                         functional_traits_NOR$species,
                                         functional_traits_NOR$treat_warming,
                                         functional_traits_NOR$treat_competition,
                                         functional_traits_NOR$block_ID_original,
                                         functional_traits_NOR$plot_ID_original,
                                         functional_traits_NOR$position_ID_original,
                                         sep = "_")

# Check for duplicates
duplicates_1 <- functional_traits_NOR[duplicated(functional_traits_NOR$unique_id) |
                                      duplicated(functional_traits_NOR$unique_id, fromLast = TRUE), ]

# View the duplicated rows
print(duplicates_1) # 16

# FUZ6303 and FVG1548 from same plant but FUZ6303 has no rep height
# delete FUZ6303 as it was probably a second leaf
functional_traits_NOR <- functional_traits_NOR |> 
  filter(ID != "FUZ6303")

# FQJ0469 and FOA7524: same plant
# delete FOA7524 since it was eaten more
functional_traits_NOR <- functional_traits_NOR |> 
  filter(ID != "FOA7524")


# FPN3919 and FQX8412:
# delete FPN3919 since it looks like the sheath of the leaf was included
functional_traits_NOR <- functional_traits_NOR |> 
  filter(ID != "FPN3919")

# FPR5674 and FPF9043:
# can't decide which to delete, so random: FPF9043
functional_traits_NOR <- functional_traits_NOR |> 
  filter(ID != "FPF9043")

# FPJ6661 (little bit folded) and FPB1557 (little whole):
# delete FPJ6661 because weight and la match for the other one
functional_traits_NOR <- functional_traits_NOR |> 
  filter(ID != "FPJ6661")

# FRB0226 and FRF6726
# delete FRF6726 because it looks less healthy
functional_traits_NOR <- functional_traits_NOR |> 
  filter(ID != "FRF6726")

# FTN5712 and FRR6902
# completely different rep height
# FTN5712 is hypmac not leuvul
# delete FTN5712 for now because it is wrong
# how can I find out what plant it actually is?
functional_traits_NOR <- functional_traits_NOR |> 
  filter(ID != "FTN5712")


# FRJ0854 and FRN2659
# delete FRN2659 because it looks less healthy
functional_traits_NOR <- functional_traits_NOR |> 
  filter(ID != "FRN2659")

length(functional_traits_NOR$ID)
# 598 samples now

sum(is.na(functional_traits_NOR$leaf_area))
# 6 samples have no leaf area but mass and thickness --> keep

sum(is.na(functional_traits_NOR$date))


# fix what can be fixed: typos in position or treat ---------------------------------------

# 1: hi 1b cyncri f5 (not g5)
functional_traits_NOR <- functional_traits_NOR |> 
  mutate(position_ID_original = case_when(
    site == "hi" & treat_warming == "ambi" & treat_competition == "vege" & species == "cyncri" & block_ID_original == "1" & plot_ID_original == "b" ~ "f5",
    TRUE ~ position_ID_original  # Keep the rest unchanged
  ))

# 2-18: hi 2e warm
functional_traits_NOR <- functional_traits_NOR |> 
  mutate(treat_warming = case_when(
    site == "hi" & treat_competition == "bare" & block_ID_original == "2" & plot_ID_original == "e" ~ "warm",
    TRUE ~ treat_warming  # Keep the rest unchanged
  ))

# 2-18: hi 2f ambi
functional_traits_NOR <- functional_traits_NOR |> 
  mutate(treat_warming = case_when(
    site == "hi" & treat_competition == "bare" & block_ID_original == "2" & plot_ID_original == "f" ~ "ambi",
    TRUE ~ treat_warming  # Keep the rest unchanged
  ))

# 19: 3e is bare
functional_traits_NOR <- functional_traits_NOR |> 
  mutate(treat_competition = case_when(
    site == "hi" & treat_warming == "warm" & block_ID_original == "3" & plot_ID_original == "e" & species == "hypmac" ~ "bare",
    TRUE ~ treat_competition  # Keep the rest unchanged
  ))

# 20: hi 4a tripra i2 (not i1)
functional_traits_NOR <- functional_traits_NOR |> 
  mutate(position_ID_original = case_when(
    site == "hi" & treat_warming == "warm" & treat_competition == "vege" & species == "tripra" & block_ID_original == "4" & plot_ID_original == "a" ~ "i2",
    TRUE ~ position_ID_original  # Keep the rest unchanged
  ))

# 21: hi 4b luzmul f5 (not f9)
functional_traits_NOR <- functional_traits_NOR |> 
  mutate(position_ID_original = case_when(
    site == "hi" & treat_warming == "ambi" & treat_competition == "vege" & species == "luzmul" & block_ID_original == "4" & plot_ID_original == "b" ~ "f5",
    TRUE ~ position_ID_original  # Keep the rest unchanged
  ))

# 22: hi 4e e8 warm bare (not vege)
functional_traits_NOR <- functional_traits_NOR |> 
  mutate(treat_competition = case_when(
    site == "hi" & treat_warming == "warm" & block_ID_original == "4" & plot_ID_original == "e" & species == "leuvul"  ~ "bare",
    TRUE ~ treat_competition  # Keep the rest unchanged
  ))

# 23: GCW2731 hi luzmul warm bare 5e g2 is not from this plot
# see comment as well
# check if there is a luzmul missing somewhere
# delete for now 
functional_traits_NOR <- functional_traits_NOR |> 
  filter(ID != "GCW2731")

# 24: hi ambi vege 10b b3 sucpra (not hypmac)
functional_traits_NOR <- functional_traits_NOR |> 
  mutate(species = case_when(
    site == "hi" & treat_warming == "ambi" & treat_competition == "vege" & block_ID_original == "10" & plot_ID_original == "b" & ID == "GMA3984" ~ "sucpra",
    TRUE ~ species  # Keep the rest unchanged
  ))

# 25: hi cennig ambi vege 10b c4 (not e4)
functional_traits_NOR <- functional_traits_NOR |> 
  mutate(position_ID_original = case_when(
    site == "hi" & treat_warming == "ambi" & treat_competition == "vege" & species == "cennig" & block_ID_original == "10" & plot_ID_original == "b" & ID == "GMI6448" ~ "c4",
    TRUE ~ position_ID_original  # Keep the rest unchanged
  ))


# 26: lo pimsax ambi bare 3b h9 (not vege)
functional_traits_NOR <- functional_traits_NOR |> 
  mutate(treat_competition = case_when(
    site == "lo" & treat_warming == "ambi" & block_ID_original == "3" & plot_ID_original == "b" & species == "pimsax" ~ "bare",
    TRUE ~ treat_competition  # Keep the rest unchanged
  ))

# 27: lo sildio ambi bare 4b b5 (not vege)
functional_traits_NOR <- functional_traits_NOR |> 
  mutate(treat_competition = case_when(
    site == "lo" & treat_warming == "ambi" & block_ID_original == "4" & plot_ID_original == "b" & species == "sildio" ~ "bare",
    TRUE ~ treat_competition  # Keep the rest unchanged
  ))

# 28: lo tripra ambi vege 8a h7 (not b)
functional_traits_NOR <- functional_traits_NOR |> 
  mutate(plot_ID_original = case_when(
    site == "lo" & treat_warming == "ambi" & treat_competition == "vege" & species == "tripra" & block_ID_original == "8" & ID == "FUP6969" ~ "a",
    TRUE ~ plot_ID_original  # Keep the rest unchanged
  ))

# 29 and 30: positions switched
# 29: lo 9b f7 = sucpra
functional_traits_NOR <- functional_traits_NOR |> 
  mutate(position_ID_original = case_when(
    site == "lo" & treat_warming == "ambi" & treat_competition == "bare" & species == "leuvul" & block_ID_original == "9" & plot_ID_original == "b" ~ "f9",
    TRUE ~ position_ID_original  # Keep the rest unchanged
  ))

# 30: lo 9b f9 = leuvul
functional_traits_NOR <- functional_traits_NOR |> 
  mutate(position_ID_original = case_when(
    site == "lo" & treat_warming == "ambi" & treat_competition == "bare" & species == "sucpra" & block_ID_original == "9" & plot_ID_original == "b" ~ "f7",
    TRUE ~ position_ID_original  # Keep the rest unchanged
  ))

# 597 samples

# Import metadata ---------------------------------------------------------
metadata <- read.csv2("Data/RangeX_Metadata.csv")
head(metadata)
dput(colnames(metadata))

## filter only NOR
metadata_NOR <- metadata %>%
  filter(grepl('NOR', region))
head(metadata_NOR)


# merge functional traits data with metadata ------------------------------

dput(colnames(metadata_NOR))
dput(colnames(functional_traits_NOR))

functional_traits_NOR_23 <- left_join(metadata_NOR, functional_traits_NOR,
                                      by = c( "site", "block_ID_original",
                                              "plot_ID_original", 
                                              "position_ID_original", "species", 
                                              "treat_warming", "treat_competition"))

head(functional_traits_NOR_23)

sum(is.na(functional_traits_NOR_23$date)) # 1206
length(functional_traits_NOR_23$date) # 1803

# this means already here 30 samples are missing?
# why?

# what goes wrong with matching? ------------------------------------------

# join: TRUE when matching worked
joined_data <- functional_traits_NOR |> 
  left_join(metadata_NOR, 
            by = c("site", "block_ID_original", "plot_ID_original",
                   "position_ID_original", "species", "treat_warming", "treat_competition")) |> 
  mutate(matched = ifelse(is.na(unique_plant_ID), FALSE, TRUE))

sum(!joined_data$matched) # 30 didn't match # now 0

# something must be wrong with the position of the plants
# hopefully typos
unmatched_rows_2 <- joined_data %>%
  filter(!matched)

# 30 samples FALSE
# need to fix these before matching with metadata
# in "fix what can be fixed: typos in position or treat"
# everything fixed and 1 row deleted
# but why is it 1803 now


# check for duplicates in unique_plant_ID ---------------------------------
duplicates_2 <- functional_traits_NOR_23 |> 
  group_by(unique_plant_ID) |> 
  filter(n() > 1)

# Display duplicates
duplicates_2 # 6

# GHZ1788 (warm bare) and GAV1827 (ambi bare): hypmac hi 2e h5 twice
# but GAV1827 was corrected above to warm

# GID4537 hi 2f i6 leuvul warm bare --> was corrected to ambi
# GAM4208 hi 2f i6 leuvul ambi bare

# FYP9768 hi sildio warm bare 2f c8 --> was corrected to ambi
# GAT6555 hi sildio ambi bare 2f c8
# 

# add missing columns ----------------------------------------------------------
colnames(functional_traits_NOR_23)

functional_traits_NOR_23 <- functional_traits_NOR_23 |> 
  dplyr::mutate(
    sto_density_top = NA,
    sto_density_bot = NA
  )


# mean of leaf thickness --------------------------------------------------
functional_traits_NOR_23 <- functional_traits_NOR_23 |> 
  mutate(leaf_thickness = rowMeans(cbind(thickness_1, thickness_2, thickness_3), 
                                   na.rm = TRUE))


# SLA and LDMC ---------------------------------------------------------------------

# calculate SLA (leaf area (mm2)/dry mass(mg)) and LDMC (dry mass (mg)/wet mass (g))

# get right units
# leaf area in cm2 so far
functional_traits_NOR_23 <- functional_traits_NOR_23 %>%
  mutate(leaf_area = leaf_area * 100, # mm2
         wet_mass_g = wet_mass, # needed for LDMC
         wet_mass = wet_mass * 1000, # mg
         dry_mass = dry_mass * 1000) # mg

functional_traits_NOR_23 <- functional_traits_NOR_23 |> 
  mutate(SLA = leaf_area/dry_mass,
         LDMC = dry_mass/wet_mass_g) 


# select columns to match meta data ---------------------------------------------
functional_leaf_traits_NOR_23 <- functional_traits_NOR_23 |> 
  select(unique_plant_ID, species, date, wet_mass, dry_mass, leaf_thickness, leaf_area, SLA, LDMC, sto_density_top, sto_density_bot)

functional_leaf_traits_NOR_23 <- functional_leaf_traits_NOR_23 |> 
  rename("date_collected" = "date")

# date format -------------------------------------------------------------
functional_leaf_traits_NOR_23 <- functional_leaf_traits_NOR_23 |> 
  mutate(date_collected  = as.Date(date_collected ))

str(functional_leaf_traits_NOR_23)


# check whether dry mass is heavier than the wet mass ---------------------
dry_wet <- functional_leaf_traits_NOR_23 %>%
  filter(dry_mass > wet_mass)
dry_wet # all good


# delete samples without measurements -------------------------------------
functional_leaf_traits_NOR_23_clean <- functional_leaf_traits_NOR_23 |> 
  filter(!is.na(date_collected))
length(functional_leaf_traits_NOR_23_clean$date_collected)# 597

sum(is.na(functional_leaf_traits_NOR_23$date_collected)) # 1206

str(functional_leaf_traits_NOR_23_clean)


# save clean file ---------------------------------------------------------
# write.csv(functional_leaf_traits_NOR_23_clean, file = "Data/RangeX_clean_functional_traits_NOR_2023.csv")







