

# RangeX data cleaning functional traits ----------------------------------

## Data used: RangeX_raw_functional_traits_2023.csv,
## RangeX_raw_functional_traits_leaf_area_NOR_2023.csv and RangeX_Metadata.csv 
## Date: 11.11.24
## Author: Nadine Arzt
## Purpose: Clean the COMPLETE raw data file of functional traits
## and combine with leaf area


# Questions ---------------------------------------------------------------

# 1. delete rows with NAs, so that data set is only 600 plants not 1800?
# 2. delete outliers? How to define them
# 3. why does functional_traits_NOR_23 has 598 rows and when I delete 
# rows with na in date of functional_leaf_traits_NOR_23 it has 568?

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

leaf_area_NOR <- read.csv("Data/RangeX_raw_functional_traits_leaf_area_NOR_2023.csv")
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

# need to find out why 606 leaves but 601 scans
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
    site == "hi" & treat_warming == "warm" & block_ID_original == "3" & plot_ID_original == "e" ~ "bare",
    TRUE ~ treat_competition  # Keep the rest unchanged
  ))

# 20: hi 4a tripra i2 (not i1)
functional_traits_NOR <- functional_traits_NOR |> 
  mutate(position_ID_original = case_when(
    site == "hi" & treat_warming == "warm" & treat_competition == "vege" & species == "tripra" & block_ID_original == "4" & plot_ID_original == "a" ~ "i2",
    TRUE ~ position_ID_original  # Keep the rest unchanged
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

sum(is.na(functional_traits_NOR_23$date)) # 1232
length(functional_traits_NOR_23$date) # 1800

# this means already here 30 samples are missing?
# why?

# what goes wrong with matching? ------------------------------------------

# join: TRUE when matching worked
joined_data <- functional_traits_NOR |> 
  left_join(metadata_NOR, 
            by = c("site", "block_ID_original", "plot_ID_original",
                   "position_ID_original", "species", "treat_warming", "treat_competition")) |> 
  mutate(matched = ifelse(is.na(unique_plant_ID), FALSE, TRUE))

sum(!joined_data$matched) # 30 didn't match

# something must be wrong with the position of the plants
# hopefully typos
unmatched_rows <- joined_data %>%
  filter(!matched)

# need to fix these before matching with metadata




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

# save clean file ---------------------------------------------------------
# write.csv(functional_leaf_traits_NOR_23, file = "Data/RangeX_clean_functional_traits_NOR_2023.csv")


str(functional_leaf_traits_NOR_23)


# check whether dry mass is heavier than the wet mass ---------------------
dry_wet <- functional_leaf_traits_NOR_23 %>%
  filter(dry_mass > wet_mass)
dry_wet # all good

functional_leaf_traits_NOR_23_clean <- functional_leaf_traits_NOR_23 |> 
  filter(!is.na(date_collected))
length(functional_leaf_traits_NOR_23_clean$date_collected)# 568
# but why was it before 598 in functional_traits_NOR

sum(is.na(functional_leaf_traits_NOR_23$date_collected))

str(functional_leaf_traits_NOR_23_clean)

# functional_leaf_traits_NOR_23_clean <- functional_leaf_traits_NOR_23 |> 
#   filter(!is.na(wet_mass) | !is.na(dry_mass) | !is.na(SLA) | !is.na(leaf_thickness)| !is.na(LDMC)| !is.na(leaf_area))

# control plotting --------------------------------------------------------

# create treat column -----------------------------------------------------

functional_traits_NOR_23 <- functional_traits_NOR_23 |> 
  mutate(treat = paste(site, treat_warming, treat_competition, sep = "_"))


# prepare plotting data set -----------------------------------------------

traits_plotting <- functional_traits_NOR_23 |> 
  select(unique_plant_ID, ID, site, treat, species, date, wet_mass, wet_mass_g, dry_mass, leaf_thickness, leaf_area, SLA, LDMC, sto_density_top, sto_density_bot)

traits_plotting_long <- traits_plotting |> 
  pivot_longer(cols = c(wet_mass, dry_mass, leaf_thickness, leaf_area,
                        SLA, LDMC, sto_density_top, sto_density_bot),
               names_to = "trait_variable", values_to = "values")



# plotting overview ----------------------------------------------------------------
#  
ggplot(data = traits_plotting_long[traits_plotting_long$site == "lo",], aes(x = trait_variable, y = values)) +
  geom_boxplot() +
  facet_grid(species~treat, scales = "free")

ggplot(data = traits_plotting_long[traits_plotting_long$site == "hi",], aes(x = trait_variable, y = values, fill = treat)) +
  geom_boxplot() +
  geom_jitter()+
  facet_wrap(~species, scales = "free") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(data = traits_plotting_long[traits_plotting_long$site == "hi",], aes(x = trait_variable, y = values, colour = treat)) +
  geom_point() +
  facet_wrap(~species, scales = "free") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))



# plotting details --------------------------------------------------------

# wet mass (mg) vs dry mass (mg)
ggplot(traits_plotting, aes(x = dry_mass, y = wet_mass, colour = treat))+
  geom_point()

traits_plotting$dry_mass

# leaf thickness vs leaf area
ggplot(traits_plotting, aes(x = leaf_thickness, y = leaf_area, colour = treat))+
  geom_point()

# SLA vs LDMC
ggplot(traits_plotting, aes(x = SLA, y = LDMC))+
  geom_point()

# SLA
ggplot(traits_plotting, aes(x = species, y = SLA, fill = species)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 1) +
  theme_minimal()+
  geom_jitter()

# dry mass
ggplot(traits_plotting, aes(x = species, y = dry_mass, fill = species)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 1) +
  theme_minimal()+
  geom_jitter()

# wet mass
ggplot(traits_plotting, aes(x = species, y = wet_mass, fill = species)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 1) +
  theme_minimal()+
  geom_jitter()

# leaf thickness
ggplot(traits_plotting, aes(x = species, y = leaf_thickness, fill = site)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 1) +
  theme_minimal()+
  geom_jitter()

# leaf area
ggplot(traits_plotting[traits_plotting$site == "lo",], aes(x = species, y = leaf_area, fill = species)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 1) +
  theme_minimal()+
  geom_jitter()


# LDMC
ggplot(traits_plotting, aes(x = species, y = LDMC, fill = site)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 1) +
  theme_minimal()+
  geom_jitter()


# leaf area vs SLA
ggplot(traits_plotting, aes(x = leaf_area, y = SLA))+
  geom_point()



# leuvul ------------------------------
leuvul <- traits_plotting |> 
  filter(species == "leuvul")

# wet mass (mg) vs dry mass (mg)
ggplot(leuvul, aes(x = dry_mass, y = wet_mass, colour = treat))+
  geom_point()

# dry_mass
ggplot(leuvul, aes(x = species, y = dry_mass, fill = treat)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 1) +
  theme_minimal()+
  geom_jitter()

# wet_mass
ggplot(leuvul, aes(x = species, y = wet_mass, fill = treat)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 1) +
  theme_minimal()+
  geom_jitter()

# LDMC
ggplot(leuvul, aes(x = species, y = LDMC, fill = treat)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 1) +
  theme_minimal()+
  geom_jitter()

# SLA
ggplot(leuvul, aes(x = species, y = SLA, fill = treat)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 1) +
  theme_minimal()+
  geom_jitter()

# leaf area
ggplot(leuvul, aes(x = species, y = leaf_area, fill = treat)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 1) +
  theme_minimal()+
  geom_jitter()

ggplot(leuvul, aes(x = species, y = leaf_area, fill = treat)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 1) +
  theme_minimal()+
  geom_jitter()

# plalan ------------------------------
plalan <- traits_plotting |> 
  filter(species == "plalan")

# wet mass (mg) vs dry mass (mg)
ggplot(plalan, aes(x = dry_mass, y = wet_mass, colour = site))+
  geom_point()

# leaf area
ggplot(plalan, aes(x = species, y = leaf_area, fill = treat)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 1) +
  theme_minimal()+
  geom_jitter()

# dry_mass
ggplot(plalan, aes(x = species, y = dry_mass, fill = treat)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 1) +
  theme_minimal()+
  geom_jitter()

# wet_mass
ggplot(plalan, aes(x = species, y = wet_mass, fill = treat)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 1) +
  theme_minimal()+
  geom_jitter()

# LDMC
ggplot(plalan, aes(x = species, y = LDMC, fill = treat)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 1) +
  theme_minimal()+
  geom_jitter()

# SLA
ggplot(plalan, aes(x = species, y = SLA, fill = treat)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 1) +
  theme_minimal()+
  geom_jitter()

# leaf_thickness
ggplot(plalan, aes(x = species, y = leaf_thickness, fill = treat)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 1) +
  theme_minimal()+
  geom_jitter()


# write function to go through all species --------------------------------

radius <- function(r){
  2*pi*r
}

radius(3)
2*pi*3






