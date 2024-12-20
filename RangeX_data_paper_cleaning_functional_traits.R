

# RangeX data cleaning functional traits ----------------------------------

## Data used: RangeX_raw_functional_traits_2023.csv and RangeX_Metadata.csv 
## Date: 11.11.24
## Author: Nadine Arzt
## Purpose: Clean the COMPLETE raw data file 


# load packages -----------------------------------------------------------

library(tidyverse)


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
functional_traits <- functional_traits %>% drop_na(blockID)
length(functional_traits$blockID) # 611 rows

## but why, it should be only 600



# delete duplicates that have no dry mass -----------------------------------
# comment: accidentally sampled 2, in fridge
# no dry mass for 5 samples

functional_traits <- functional_traits %>%
  filter(!is.na(dry_mass))



# import leaf area data ---------------------------------------------------

leaf_area_NOR <- read.csv("Data/RangeX_raw_functional_traits_leaf_area_NOR_2023.csv")
leaf_area_NOR


# Join leaf area to rest of functional traits -----------------------------

functional_traits_NOR <- left_join(functional_traits, leaf_area_NOR,
                                  by = "ID")

functional_traits_NOR

# check structure of data set ---------------------------------------------
str(functional_traits_NOR)
length(functional_traits_NOR) ## 18 columns

## get column names
dput(colnames(functional_traits_NOR))

# need to find out why 606 leaves but 601 scans
# 6 leaves have no leaf area
# FPI1781, FMQ6120, FSK5791, GDR8049 are in "bad_scans" folder because these scans stopped the code

# GAS2139 and GCX6229 have no scan


# rename columns and match with metadata ----------------------------------------------------------

# to match metadata
functional_traits_NOR <- functional_traits_NOR %>%
  rename("block_ID_original" = "blockID",
         "plot_ID_original" = "plotID",
         "position_ID_original" = "positionID",
         "treat_warming" = "treat1",
         "treat_competition" = "treat2")


# to match species
functional_traits_NOR <- functional_traits_NOR %>%
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
functional_traits_NOR <- functional_traits_NOR %>%
  mutate(site = case_when(
    site == "LS" ~ "lo",
    site == "HS" ~ "hi"
  ))

# to match plot_ID_original
functional_traits_NOR <- functional_traits_NOR %>%
  mutate(plot_ID_original = case_when(
    plot_ID_original == "A" ~ "a",
    plot_ID_original == "B" ~ "b",
    plot_ID_original == "C" ~ "c",
    plot_ID_original == "D" ~ "d",
    plot_ID_original == "E" ~ "e",
    plot_ID_original == "F" ~ "f"
  ))

# to match treat_warming
functional_traits_NOR <- functional_traits_NOR %>%
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
print(duplicates_1)

# FUZ6303 and FVG1548 from same plant but FUZ6303 has no rep height
# delete FUZ6303 as it was probably a second leaf
functional_traits_NOR <- functional_traits_NOR %>%
  filter(ID != "FUZ6303")

# FQJ0469 and FOA7524: same plant
# delete FOA7524 since it was eaten more
functional_traits_NOR <- functional_traits_NOR %>%
  filter(ID != "FOA7524")


# FPN3919 and FQX8412:
# delete FPN3919 since it looks like the sheath of the leaf was included
functional_traits_NOR <- functional_traits_NOR %>%
  filter(ID != "FPN3919")

# FPR5674 and FPF9043:
# can't decide which to delete, so random: FPF9043
functional_traits_NOR <- functional_traits_NOR %>%
  filter(ID != "FPF9043")

# FPJ6661 (little bit folded) and FPB1557 (little whole):
# delete FPJ6661 because weight and la match for the other one
functional_traits_NOR <- functional_traits_NOR %>%
  filter(ID != "FPJ6661")

# FRB0226 and FRF6726
# delete FRF6726 because it looks less healthy
functional_traits_NOR <- functional_traits_NOR %>%
  filter(ID != "FRF6726")

# FTN5712 and FRR6902
# completely different rep height
# FTN5712 is hypmac not leuvul
# delete FTN5712 for now because it is wrong
# how can I find out what plant it actually is?
functional_traits_NOR <- functional_traits_NOR %>%
  filter(ID != "FTN5712")


# FRJ0854 and FRN2659
# delete FRN2659 because it looks less healthy
functional_traits_NOR <- functional_traits_NOR %>%
  filter(ID != "FRN2659")


# Import metadata ---------------------------------------------------------
metadata <- read.csv2("RangeX_Metadata_NOR.csv")
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
                         by = c("site", "block_ID_original", "plot_ID_original",
                                "position_ID_original", "species","treat_warming", "treat_competition"))

head(functional_traits_NOR_23)




# add missing columns ----------------------------------------------------------
colnames(functional_traits_NOR_23)

functional_traits_NOR_23 <- functional_traits_NOR_23 %>%
  dplyr::mutate(
    sto_density_top = NA,
    sto_density_bot = NA
  )


# mean of leaf thickness --------------------------------------------------
functional_traits_NOR_23 <- functional_traits_NOR_23 %>%
  mutate(leaf_thickness = rowMeans(select(., thickness_1, thickness_2, thickness_3), 
                                        na.rm = TRUE))


# SLA and LDMC ---------------------------------------------------------------------

# calculate SLA (leaf area (mm2)/dry mass(mg)) and LDMC (dry mass (mg)/wet mass (g))

# get right units
# la in cm2 so far
functional_traits_NOR_23 <- functional_traits_NOR_23 %>%
  mutate(leaf_area = leaf_area * 100, # mm2
         wet_mass_g = wet_mass, # needed for LDMC
         wet_mass = wet_mass * 1000, # mg
         dry_mass = dry_mass * 1000) # mg

functional_traits_NOR_23 <- functional_traits_NOR_23 %>%
  mutate(SLA = leaf_area/dry_mass,
         LDMC = dry_mass/wet_mass_g) 





















