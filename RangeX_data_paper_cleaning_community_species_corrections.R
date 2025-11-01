
# RangeX community cover data cleaning 2  -------------------------------

## Data used: RangeX_raw_comcov_high_2021.xlsx, 
##            RangeX_raw_comcov_low_2021.xlsx,
##            RangeX_raw_comcov_high_2023.xlsx, 
##            RangeX_raw_comcov_low_2023.xlsx,
##            RangeX_metadata_plot_NOR.csv
##            Species_names_to_correct.xlsx
## source general cleaning script: "RangeX_data_paper_cleaning_community.R"
## Date:      11.06.2025
## Author:    Nadine Arzt
## Purpose:   Cleaning of the species names 2021-2023


# comments general -----------------------------------------------------------
# what to do with <1 in 21 and 22? 
# change to 1? leave?

# who is cris?
# who is JT?

# check and discuss! ----------------------------------------------------------
# 2021: is hi 3A and 3B switched? changed to b = NOR.hi.warm.vege.wf.03 and a = NOR.hi.ambi.vege.wf.03

# make sp to sp. 

# Circle grass = Festuca pratensis
# Carex flava = Carex demissa
# Anthoxanthum nipponicum
# Cirsium arvense and C. helenioides = C. heterophyllum

# Phleum at high site = alpinum? 

# ?Galium: NOR.lo.ambi.vege.wf.06: lo 6A, 2021-08-30

# ?Melica nutans (grass 2 ): NOR.hi.warm.vege.wf.06, hi 6A: 2021-08-13

# Geranium sylvaticum ? was this confused with e.g. Anemone? 
# e.g. in hi 5c?


# ?Stellaria graminea at high site?: NOR.hi.warm.vege.nf.06, hi 6C and NOR.hi.ambi.vege.wf.07, hi 7B

# Arve sp: NOR.hi.warm.vege.wf.06, hi 6A
# maybe Stellaria graminea?

# Avenella flexuosa (Deschampsia): NOR.hi.ambi.vege.wf.01, hi 1B
# is now called Deschampsia flexuosa
# so we can change it
# but doesn't say that it's was determined correct in 21 since it's not in 22 and 23

# Big grass: NOR.lo.ambi.vege.wf.07 - check also 24 and field

# Blue sedge: NOR.hi.ambi.vege.wf.06, hi 6B

# Carex bigetowii?: NOR.hi.ambi.vege.nf.03, hi 3D - check 24 and field

# Carex lapponica?? Must be something else: NOR.lo.ambi.vege.wf.05, lo 5A

# Carex sp...: hi 2abc, 3cd, 4a - 2021 - don't know how to figure them out


# Euphrasia frigida or stricta? hard to differentiate

# ??Grass 1: NOR.hi.warm.vege.wf.06, hi6A, is it the same as Hairy poa/festuca?
# 126: Hairy poa/festuca? --> Danthonia decumbens?

# Hairy Leontodon and Hairy Taraxacum/Leontodon: NOR.hi.ambi.vege.nf.08 hi8D 
# and hi10b NOR.hi.ambi.vege.wf.10
# hi7a in 23 is hairy leontodon
# just Leontodon autumnalis?
# many Leontodon sp in lo site 2,3,4,5

# Omalotheca supinum - Omalotheca sylvatica -  Omalotheca norwegica

# Sedge 3 is Carex panicea in NOR.hi.ambi.vege.wf.04
# don't know about NOR.hi.warm.vege.nf.04

# Stellaria sp

# Viola mess: Viola palustris, V. riviniana, V. canina - check 23 and 24!!

# sedge 7? 

# hi 1b: Antennaria dioica? only in 21
# hi 1b: Fern vs Blechnum spicant

# hi 1d: Carex ?pilulifera

# hi 2a: Carex ?capillaris

# Phleum alpinum? e.g. hi 2a, hi4d

# hi 3b: Hairy seedling (hitchhiker?) in 21

# Veronica officinalis in hi3b and hi2c: somehow wrong in 21
# Veronica alpina too in hi2d

# ?Veronica officinalis: NOR.hi.warm.vege.nf.03 - 2021: rosette

# Antennaria dioica in 2021 is something else?

# Was Carex vaginate from 21 C. pillulifera in 23?

# hi4d and hi5a: Anthoxanthum nipponicum? Ask Vigdis
# is all A. odoratum actually nipponicum?

# hi6a: Huperzia which? NOR.hi.warm.vege.wf.06

# hi6a: sedge 7 21 no idea

# hi6b: fern? Ask Susanne for picture

# hi6d: Carex bigelowii? only 23 - maybe rather nigra

# hi7a: 2021 Carex filiformis? is synonym of C. montana but that is not natice to Norway- ask Dagmar - might be something else
# maybe pilulifera?

# hi8d: Hairy Leontodon is autumnalis: subturf 12: 2 and 5 --> make it 7?

# is Taraxacum 2 Hypochaeris radicata? 

# hi9b: Luzmul* in 21 and 22 not found? -> forgot * in 22

# hi9b: digitalizing mistake with viola, ... fixed in original -> check

# hi10b: Equisetum pratense or arvense? 

# lo3a: Grass sp in 21?


# check in the field ------------------------------------------------------
# hi 8d: ?Rubus chamaemorus, NOR.hi.ambi.vege.nf.08 = Filipendula ulmaria

# ?Stellaria graminea at high site?: NOR.hi.warm.vege.nf.06, hi 6C and NOR.hi.ambi.vege.wf.07, hi 7B

# ?Veronica officinalis: NOR.hi.warm.vege.nf.03, hi 3C

# Ajuga?Bald?: NOR.hi.warm.vege.nf.02, hi 2C
# Bold Ajuga?: NOR.hi.ambi.vege.nf.01, hi 1D
# probably Ajua pyramidalis without hairs

# Carex palustris: NOR.hi.warm.vege.wf.06, hi 6A
# potentially Carex pilulifera??
# not sure!! Ask Dagmar

# Salix sp in hi8b, NOR.hi.ambi.vege.wf.08 --> Salix glauca x phylicifolia ?

# check Taraxacum sp. and Taraxacum 2 - one is hairy - one not
# e.g. hi8c
# is Taraxacum 2 Hypochaeris radicata? 
# e.g. hi2c, hi6d, hi9c, hi10a
# and compare with Hypochaeris in hi9b

# NOR.hi.warm.vege.wf.06, hi6A is Ajuga pyramidalis? check in which subplots

# NOR.hi.ambi.vege.wf.01 (hi, 1B) in 23 - Molinia?

# Violas: # e.g. hi8c

# hi 1b: Fern vs Blechnum spicant

# hi 2d: Carex pillulifera was C. echinata and C. pallescens in 21?

# hi 3b: Carex nigra? not found in 23

# hi4a: viola sp = palustris and sp2 = riviniana

# hi4a: Veronica officinalis?

# Pleum alpinum: e.g. hi4a

# hi4d: Anthoxanthum nipponicum? Ask Vigdis

# hi4d: Omalotheca norwegica?

# hi5a: Euphrasia stricta  23 vigdis

# hi5d: is Antennaria from 21 actually Omalotheca?

# hi 8a ang hi9d: Viola palustris?

# hi 8d: Viola 2 canina or riviniana?

# hi 9d: Equisetum pratense is arvense in 24

# hi10d: Hairy Taraxacum/Leontodon

# hi10d: Carex leporina?

# lo5a: Check Euphrasia is it stricta?

# lo7a: Big grass? big ligula

# lo9a: Viola canina? was recorded in 24 but not in 23 

# lo8A, 5: is there two types of Angelica?


# library -----------------------------------------------------------------
library(openxlsx)
library(stringr)
library(stringi)

# source community cleaning script 1 --------------------------------------
source("RangeX_data_paper_cleaning_community.R")

# work with this to have subturf information as well
community_data_raw_NOR

# import file with species names that need to be corrected ----------------
# this was created in script "Check_species_names_with_TNRS.R"
species_names_to_correct <- read.xlsx("Data/Data_community/Species_names_to_correct.xlsx")


# fix species names -------------------------------------------------------
# go through list and check original data sheets from all years to fix species names and 
# figure out problems
# save in community_data_raw_NOR_fixed

# problem that e.g. Veronica officinalis was spelled exactly the same in different years but displayed as two different species
# standardize species names
# stri_trans_general(..., "Latin-ASCII") removes hidden Unicode variants.
# str_squish() trims and removes extra internal spaces.
community_data_raw_NOR_fixed <- community_data_raw_NOR

# community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |>
#  mutate(species = str_squish(stri_trans_general(species, "Latin-ASCII")))

# community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |>
#   mutate(species = str_trim(species))  # Only trims leading/trailing whitespace

# but this also changes "?Veronica oficinalis" to "Veronica oficinalis"
# which is not what I want
# community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |>
#   mutate(species = str_replace_all(species, "\u00A0", " "))


community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = str_replace_all(species, "[\\u00a0\\s]+", " "))


veronica <- community_data_raw_NOR_fixed |> 
  filter(str_detect(species, "officinalis")) |> 
  distinct(year, species)



# 1: ???
# NOR.hi.ambi.vege.wf.01 (hi, 1B) in 23
# Molinia?
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(species == "???" & unique_plot_ID == "NOR.hi.ambi.vege.wf.01" 
                             ~ "cf. Molinia caerulea", TRUE ~ species))

# 2: ?Antennaria dioica
# NOR.hi.ambi.vege.wf.04 only in 21
# maybe Omalotheca sylvatic?
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
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
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "?Festuca rubra" ~ "Festuca rubra",
    TRUE ~ species
  ))

# 4: ?Galium
# NOR.lo.ambi.vege.wf.06: 2021-08-30
# could be Galium boreale or uliginosum
# more likely boreale because it has an occurrence in 
# both subplots (15, 16) in 23
# it says_ 4 leaves on 21 datasheet, so it must be boreale
# but it is already with a 1 and 5 in those subturfs, so do we just delete ?Galium ?
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |>
  filter(!(species == "?Galium" & unique_plot_ID %in% c("NOR.lo.ambi.vege.wf.06")))

# 5: 5 ?Hypochaeris radicata
# NOR.hi.ambi.vege.wf.09, hi 9B: 2021-08-21
# same subplots in 23
# so we believe it?
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "?Hypochaeris radicata" ~ "Hypochaeris radicata",
    TRUE ~ species
  ))

# 6: 	?Melica nutans (grass 2 )
# NOR.hi.warm.vege.wf.06, hi 6A: 2021-08-13
# could it be the same as Hairy poa/festuca? in 23?
# 154: Melica nutans (grass 2): no value
# maybe just delete?


# 7: ?Omalotheca
# NOR.hi.ambi.vege.wf.03, hi 3B: 2021-08-08
# there is also Omalotheca in 22 in same subplot after 3a and 3b have been switched as indicated on 22 datasheet
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "?Omalotheca" ~ "Omalotheca sylvatica",
    TRUE ~ species
  ))
# and Omalotheca to Omalotheca sylvatica in NOR.hi.ambi.vege.wf.03 in 22
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
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
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "?Pyrola" & unique_plot_ID == "NOR.hi.ambi.vege.wf.06" ~ "Pyrola minor",
    TRUE ~ species
  ))

#10: ?Rubus chamaemorus
# NOR.hi.ambi.vege.nf.08: 2021-08-20
# could it be Filipendula ulmaria? 
# it's the same subplot in 23
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "?Rubus chamaemorus" & unique_plot_ID == "NOR.hi.ambi.vege.nf.08" ~ "Filipendula ulmaria",
    TRUE ~ species
  ))

# 11: ?Salix = Salix herbacea in 22 and 23?
# NOR.hi.ambi.vege.wf.06: 2021
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "?Salix" & unique_plot_ID == "NOR.hi.ambi.vege.wf.06" ~ "Salix herbacea",
    species == "Salix herbacea?" & unique_plot_ID == "NOR.hi.ambi.vege.wf.06" ~ "Salix herbacea",
    TRUE ~ species
  ))


# 12: ?Stellaria graminea: NOR.hi.warm.vege.nf.06 
# and NOR.hi.ambi.vege.wf.07 but here not in 22 or 23
# 21 and 22
# accept for now but need to discuss
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "?Stellaria graminea" & unique_plot_ID == "NOR.hi.warm.vege.nf.06" ~ "Stellaria graminea",
    species == "?Stellaria graminea" & unique_plot_ID == "NOR.hi.ambi.vege.wf.07" ~ "Stellaria graminea",
    TRUE ~ species
  ))

# carex capillaris in NOR.hi.ambi.vege.wf.07
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "carex capillaris" ~ "Carex capillaris",
    TRUE ~ species
  ))

# 13: ?Stellaria graminea ?longifolia: NOR.hi.ambi.vege.nf.06
# same subplot in 23, so we accept
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "?Stellaria graminea ?longifolia" & unique_plot_ID == "NOR.hi.ambi.vege.nf.06" ~ "Stellaria graminea",
    TRUE ~ species
  ))

# # 14: ?Veronica officinalis: NOR.hi.warm.vege.nf.03
# # not sure! Accept fo rnow but check
# community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
#   mutate(species = case_when(
#     species == "?Veronica officinalis" & unique_plot_ID == "NOR.hi.warm.vege.nf.03" ~ "Veronica officinalis",
#     TRUE ~ species
#   ))

# 19: Ajuga?Bald?: NOR.hi.warm.vege.nf.02
# maybe it was Ajuga
# accept for now
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "Ajuga?Bald?" & unique_plot_ID == "NOR.hi.warm.vege.nf.02" ~ "Ajuga pyramidalis",
    TRUE ~ species
  ))

# 21: Alchemilla sp
# we don't know better

# 22: Anemone nemoralis: NOR.hi.ambi.vege.nf.05
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "Anemone nemoralis" & unique_plot_ID == "NOR.hi.ambi.vege.nf.05" ~ "Anemone nemorosa",
    TRUE ~ species
  ))

# 25: 	Anglica sylvestris: NOR.lo.ambi.vege.wf.04
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "Anglica sylvestris" & unique_plot_ID == "NOR.lo.ambi.vege.wf.04" ~ "Angelica sylvestris",
    TRUE ~ species
  ))

# 26: Ant nest: NOR.hi.warm.vege.nf.02 and NOR.hi.warm.vege.nf.04
# good to know but delete here
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |>
  filter(!(species == "Ant nest" & unique_plot_ID %in% c("NOR.hi.warm.vege.nf.02", "NOR.hi.warm.vege.nf.04")))

# 30: Arve sp: NOR.hi.warm.vege.wf.06
# don't know what that is
# maybe Stellaria graminea (23 datasheet for hi 6A)
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "Arve sp" & unique_plot_ID == "NOR.hi.warm.vege.wf.06" ~ "Stellaria graminea",
    TRUE ~ species
  ))

# 32: Avenella flexuosa (Deschampsia): 
# NOR.hi.ambi.vege.wf.01
# is now called Deschampsia flexuosa
# so we can change it
# but doesn't say that it's was determined correct in 21 since it's not in 22 and 23
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "Avenella flexuosa" & unique_plot_ID == "NOR.hi.ambi.vege.wf.01" ~ "Deschampsia flexuosa",
    species == "Avenella flexuosa (Deschampsia)" & unique_plot_ID == "NOR.hi.ambi.vege.wf.01" ~ "Deschampsia flexuosa",
    TRUE ~ species
  ))

# 33: Betuala pubescens: NOR.hi.warm.vege.wf.06
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "Betuala pubescens" & unique_plot_ID == "NOR.hi.warm.vege.wf.06" ~ "Betula pubescens",
    TRUE ~ species
  ))

# 34: Betula bubescens: NOR.hi.warm.vege.wf.06 and 
# 35: Betula bunescens: NOR.hi.ambi.vege.nf.06
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "Betula bubescens" & unique_plot_ID == "NOR.hi.warm.vege.wf.06" ~ "Betula pubescens",
    species == "Betula bunescens" & unique_plot_ID == "NOR.hi.ambi.vege.nf.06" ~ "Betula pubescens",
    TRUE ~ species
  ))

# 37 Big grass: NOR.lo.ambi.vege.wf.07

# 38: Big hole: NOR.hi.warm.vege.nf.03
# good to know but delete for now
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |>
  filter(!(species == "Big hole" & unique_plot_ID %in% c("NOR.hi.warm.vege.nf.03")))

# 41: Blue sedge: NOR.hi.ambi.vege.wf.06
# probably Carex panicea
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "Blue sedge" & unique_plot_ID == "NOR.hi.ambi.vege.wf.06" ~ "Carex panicea",
    TRUE ~ species
  ))

# 42: Blus sedge: NOR.hi.ambi.vege.wf.06
# delete because no values
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |>
  filter(!(species == "Blus sedge" & unique_plot_ID %in% c("NOR.hi.ambi.vege.wf.06")))

# 43: Bold Ajuga?: NOR.hi.ambi.vege.nf.01
# probably Ajia pyramidalis without hairs but check in the field
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "Bold Ajuga?" & unique_plot_ID == "NOR.hi.ambi.vege.nf.01" ~ "Ajuga pyramidalis",
    TRUE ~ species
  ))

# 44: Calluna sp: NOR.hi.warm.vege.nf.04
# 45: Calluna sp.: NOR.hi.ambi.vege.wf.05
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "Calluna sp" & unique_plot_ID == "NOR.hi.warm.vege.nf.04" ~ "Calluna vulgaris",
    species == "Calluna sp." & unique_plot_ID == "NOR.hi.ambi.vege.wf.05" ~ "Calluna vulgaris",
    TRUE ~ species
  ))

# 52: Carex bigetowii?: NOR.hi.ambi.vege.nf.03
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "Carex bigetowii?" & unique_plot_ID == "NOR.hi.ambi.vege.nf.03" ~ "Carex cf. bigelowii",
    TRUE ~ species
  ))

# 55: Carex demisa: NOR.hi.warm.vege.nf.05
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "Carex demisa" & unique_plot_ID == "NOR.hi.warm.vege.nf.05" ~ "Carex demissa",
    TRUE ~ species
  ))
# NOR.hi.warm.vege.wf.06
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "Carex demisa" & unique_plot_ID == "NOR.hi.warm.vege.wf.06" ~ "Carex demissa",
    TRUE ~ species
  ))
# NOR.hi.warm.vege.nf.06
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "Carex demisa" & unique_plot_ID == "NOR.hi.warm.vege.nf.06" ~ "Carex demissa",
    TRUE ~ species
  ))


# NOR.hi.warm.vege.wf.06: Anthoxanthum, subturf 11, 6ff, 2023
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |>
  mutate(`11` = if_else(
    species == "Anthoxanthum odoratum" & 
      unique_plot_ID == "NOR.hi.warm.vege.wf.06" & 
      year == 2023, 
    "6f", 
    `11`
  ))

# all Carex flava is Carex demissa
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "Carex flava" ~ "Carex demissa",
    TRUE ~ species
  ))

# 61: Carex lapponica?? Must be something else: NOR.lo.ambi.vege.wf.05
# 2021 
# change to Carex sp.?
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "Carex lapponica?? Must be something else" & unique_plot_ID == "NOR.lo.ambi.vege.wf.05" ~ "Carex sp.",
    TRUE ~ species
  ))

# 62: Carex leparina: NOR.hi.warm.vege.nf.03
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "Carex leparina Must be something else" & unique_plot_ID == "NOR.lo.ambi.vege.wf.05" ~ "Carex leporina",
    TRUE ~ species
  ))

# 67: Carex palllescens: NOR.hi.ambi.vege.wf.09
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "Carex palllescens" & unique_plot_ID == "NOR.hi.ambi.vege.wf.09" ~ "Carex pallescens",
    TRUE ~ species
  ))

# 68: Carex palustre: NOR.hi.ambi.vege.wf.04, hi 4B should be Cirsium palustre
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "Carex palustre" & unique_plot_ID == "NOR.hi.ambi.vege.wf.04" ~ "Cirsium palustre",
    TRUE ~ species
  ))

# Carex palustre: NOR.hi.warm.vege.wf.04 = Cirsium
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "Carex palustre" & unique_plot_ID == "NOR.hi.warm.vege.wf.04" ~ "Cirsium palustre",
    TRUE ~ species
  ))

# Carex palustris: NOR.hi.warm.vege.wf.06 2022
# potentially Carex pilulifera??
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "Carex palustris" & unique_plot_ID == "NOR.hi.warm.vege.wf.06" ~ "Carex cf. pilulifera",
    TRUE ~ species
  ))
# Carex palustris?: NOR.hi.warm.vege.wf.06 2023
# delete
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |>
  filter(!(species == "Carex palustris?" & unique_plot_ID %in% c("NOR.hi.warm.vege.wf.06")))

# 73: Carex piluluifera: NOR.hi.warm.vege.wf.02
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "Carex piluluifera" & unique_plot_ID == "NOR.hi.warm.vege.wf.02" ~ "Carex pilulifera",
    TRUE ~ species
  ))

# 75: Carex sp
# 76: Carex sp 2
# 77: Carex sp 4
# 78: Carex sp 5
# no value - delete
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |>
  filter(!( (species == "Carex sp" | species == "Carex sp 2"
             | species == "Carex sp 4" | species == "Carex sp 5")
             & total_cover == "0"))

# 80: Cearex demissa: NOR.hi.warm.vege.nf.07
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "Cearex demissa" & unique_plot_ID == "NOR.hi.warm.vege.nf.07" ~ "Carex demissa",
    TRUE ~ species
  ))


# 83: Circium palustre = Cirsium palustre
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "Circium palustre" ~ "Cirsium palustre",
    TRUE ~ species
  ))

# 84: Circle grass = Festuca pratensis
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(species == "Circle grass" ~ "Festuca pratensis",
                             TRUE ~ species))

# 86: Cirsium helenoides = Cirsium helenioides
# C.helenoides is probably a synonym of Cirsium heterophyllum
# also it was recorded as C. arvense in 21 and 22
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "Cirsium helenoides" ~ "Cirsium heterophyllum",
    species == "Cirsium arvense" ~ "Cirsium heterophyllum",
    TRUE ~ species
  ))

# 94: Empitrum nigrum
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "Empitrum nigrum" & unique_plot_ID == "NOR.hi.warm.vege.nf.02" ~ "Empetrum nigrum",
    TRUE ~ species
  ))

# 96: Equilobium anagallidifolium? cf anagallidifolium, hi5B
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "Equilobium anagallidifolium?" ~ "Equilobium cf. anagallidifolium",
    TRUE ~ species
  ))

# 100: Equisetum sylvestris = E. sylvaticum
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "Equisetum sylvestris" ~ "Equisetum sylvaticum",
    TRUE ~ species
  ))

# 101: 	Equisteum pratense; NOR.hi.ambi.vege.nf.09
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |>
  filter(!(species == "Equisteum pratense"
            & total_cover == "0"))

# 103: Euphrasia fragida: NOR.hi.warm.vege.wf.06
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "Euphrasia fragida" & unique_plot_ID == "NOR.hi.warm.vege.wf.06" ~ "Euphrasia frigida",
    TRUE ~ species
  ))

# 104: Euphrasia str
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "Euphrasia str" ~ "Euphrasia stricta",
    TRUE ~ species
  ))

# 107: Fern
# delete
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |>
  filter(!(species == "Fern" & total_cover == "0"))

# 108: Festuca fuzzy
# 109: Festuca fuzzy?
# Danthonia decumbens found out in 23
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "Festuca fuzzy" ~ "Danthonia decumbens",
    species == "Festuca fuzzy?" ~ "Danthonia decumbens",
    TRUE ~ species
  ))

community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |>
  filter(!(species == "Festuca fuzzy" & total_cover == "0"))


# 110: Festuca pratense: NOR.lo.ambi.vege.wf.02 and NOR.lo.ambi.vege.wf.01
# 113: Fesuca pratensis
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "Festuca pratense" ~ "Festuca pratensis",
    species == "Fesuca pratense" ~ "Festuca pratensis",
    TRUE ~ species
  ))

# 115: 	Fluffy grass (hitchhiker?) = Holcus lanatus
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "Fluffy grass (hitchhiker?)" ~ "Holcus lanatus",
    TRUE ~ species
  ))

# 119: Galium uligonosum = Galium uliginosum in NOR.hi.warm.vege.nf.06
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "Galium uligonosum" ~ "Galium uliginosum",
    TRUE ~ species
  ))

# 122: Grass 1: NOR.hi.warm.vege.wf.06, hi6A, is it the same as Hairy poa/festuca?
# 126: Hairy poa/festuca?
# Danthonia decumbens?
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "Grass 1" & unique_plot_ID == "NOR.hi.warm.vege.wf.06" ~ "Danthonia decumbens",
    species == "Hairy poa/festuca?" & unique_plot_ID == "NOR.hi.warm.vege.wf.06" ~ "Danthonia decumbens",
    TRUE ~ species
  ))

# 123: Grass sp
# was found as leaf with skitracks and long hairs
# Danthonia decumbens in 24, so it was that in 23 but not sure about 21
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "Grass sp" & unique_plot_ID == "NOR.hi.warm.vege.wf.04" 
    & year == "2023" ~ "Danthonia decumbens",
    TRUE ~ species
  ))

# 124: Hairy Leontodon: NOR.hi.ambi.vege.nf.08 hi8D and hi10b NOR.hi.ambi.vege.wf.10
# not recorded in 24, so maybe just Leontodon autumnalis
# there is some hairy leaves but we didn't distinguish hairy from normal 

# 125: Hairy Taraxacum/Leontodon

# 127: Hairy seedling
# 128: Hairy seedling (hitchhiker?)
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |>
  filter(!(species == "Hairy seedling" & total_cover == "0"))

community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |>
  filter(!(species == "Hairy seedling (hitchhiker?)" & total_cover == "0"))

# 131: Hieracium pilosella(=Pilosella officinalis)
# 132: Hieracium pilusella
# is synonym of Pilosella officinarum 
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "Hieracium pilosella(=Pilosella officinalis)" ~ "Pilosella officinarum",
    species == "Hieracium pilusella" ~ "Pilosella officinarum",
    species == "Hieracium pilosella" ~ "Pilosella officinarum",
    species == "Hieracium" ~ "Pilosella officinarum",
    TRUE ~ species
  ))


# 133: Hieracium sp: no value
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |>
  filter(!(species == "Hieracium sp" & total_cover == "0"))

# 136: Hypericum maculata
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "Hypericum maculata" ~ "Hypericum maculatum",
    TRUE ~ species
  ))

# 140: Juncus articulata: NOR.hi.warm.vege.nf.06
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "Juncus articulata" ~ "Juncus articulatus",
    TRUE ~ species
  ))

# 146: 	Leontodon sp: hi7a in 23 is hairy leontodon
# lo 2a, 3a, 4a, 5a
# NOR.lo.ambi.vege.wf.04 is Leontodon autumnalis
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "Leontodon sp" & unique_plot_ID == "NOR.lo.ambi.vege.wf.04"
    ~ "Leontodon autumnalis",
    species == "Leontodon sp" & unique_plot_ID == "NOR.lo.ambi.vege.wf.05"
    ~ "Leontodon autumnalis",
    species == "Leontodon sp" & unique_plot_ID == "NOR.lo.ambi.vege.wf.02"
    ~ "Leontodon autumnalis",
    TRUE ~ species
  ))


# Knautia arvensis in NOR.lo.ambi.vege.wf.03 in subturf 6: 1?
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |>
  mutate(`6` = if_else(
    species == "Knautia arvensis" & 
      unique_plot_ID == "NOR.lo.ambi.vege.wf.03" & 
      year == 2023, 
    "1", 
    `6`
  ))

# 158: Omalotheca supinum is probably sylvatica 
# but Omalotheca is complicated
# could be also O. norwegica
# depends on number of nerves on leaves but hard to see


# 164: Phleum pratensis is Phleum pratense
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "Phleum pratensis" ~ "Phleum pratense",
    TRUE ~ species
  ))

# 165: Phleum sp
# probably also Phleum pratense
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "Phleum sp" ~ "Phleum pratense",
    TRUE ~ species
  ))

# 173: 	Poa sp
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "Poa sp" ~ "Poa pratensis",
    TRUE ~ species
  ))

# 181: Rock
# good to know but delete here
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |>
  filter(!(species == "Rock" & total_cover == "0"))

# 183: Rubus sp
# delete in 23 because no value but leave in 21 because we don't know
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |>
  filter(!(species == "Rubus sp" & total_cover == "0"))

# 185: Rush 1: NOR.hi.warm.vege.wf.05
# 2022: Juncus filiformis
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "Rush 1" & unique_plot_ID == "NOR.hi.warm.vege.wf.05" 
    ~ "Juncus filiformis",
    TRUE ~ species
  ))

# 187: Sagina sagnoides in NOR.hi.warm.vege.nf.02
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "Sagina sagnoides" & unique_plot_ID == "NOR.hi.warm.vege.nf.02" 
    ~ "Sagina saginoides",
    TRUE ~ species
  ))

# 188: Sagina sp
# don't know - leave

# 189: 	Salix e in NOR.hi.warm.vege.wf.10
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "Salix e" & unique_plot_ID == "NOR.hi.warm.vege.wf.10" 
    ~ "Salix herbacea",
    TRUE ~ species
  ))

# 192: Salix sp in hi8b, NOR.hi.ambi.vege.wf.08
# Salix cf. glauca x phylicifolia
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "Salix sp" & unique_plot_ID == "NOR.hi.ambi.vege.wf.08" 
    ~ "Salix cf. glauca x phylicifolia",
    TRUE ~ species
  ))

# 193: Sedge 1 is Carex panicea 
# according to 22 data sheet for hi5b NOR.hi.ambi.vege.wf.05
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "Sedge 1" & unique_plot_ID == "NOR.hi.ambi.vege.wf.05" 
    ~ "Carex panicea",
    TRUE ~ species
  ))

# 194: Sedge 3 is Carex panicea in NOR.hi.ambi.vege.wf.04
# don't know about NOR.hi.warm.vege.nf.04
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "Sedge 3" & unique_plot_ID == "NOR.hi.ambi.vege.wf.04" 
    ~ "Carex panicea",
    TRUE ~ species
  ))

# 195: Selaginalla selaginoides
# 197: Selaginella selagineloides
# 198: Selaginella selaginetoides
# 200: Setaginella selagi
# probably everything Selaginella selaginoides
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "Selaginalla selaginoides" ~ "Selaginella selaginoides",
    species == "Selaginella selagineloides" ~ "Selaginella selaginoides",
    species == "Selaginella selaginetoides" ~ "Selaginella selaginoides",
    species == "Setaginella selagi" ~ "Selaginella selaginoides",
    species == "Setaginella" ~ "Selaginella selaginoides",
    TRUE ~ species
  ))

# 202: Sibbaldia procumbens
# should be correct

# 203: Silena dioica*
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "Silena dioica*" ~ "Silene dioica*",
    TRUE ~ species
  ))

# 206: 	Stellaria sp
# probably Stellaria graminea?
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "Stellaria sp" ~ "Stellaria graminea",
    TRUE ~ species
  ))

# 209: Taraxacum  sp.
# 210: Taraxacum 2
# 211: Taraxacum 2?
# 212: Taraxacum sp
# 213: Taraxacum sp.
# there is two Taraxacum - one with hairs - one without

# in NOR.hi.warm.vege.wf.08 it might be Taraxacum sp. 
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "Taraxacum  sp." & unique_plot_ID == "NOR.hi.warm.vege.wf.08" 
    ~ "Taraxacum sp.",
    species == "Taraxacum 2?" & unique_plot_ID == "NOR.hi.warm.vege.wf.08" 
    ~ "Taraxacum sp.",
    species == "Taraxacum sp" & unique_plot_ID == "NOR.hi.warm.vege.wf.08" 
    ~ "Taraxacum sp.",
    TRUE ~ species
  ))

# in NOR.hi.ambi.vege.nf.09 it's probably Taraxacum 2 in 23 
# because that was in 21
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "Taraxacum sp" & unique_plot_ID == "NOR.hi.ambi.vege.nf.09" 
    ~ "Taraxacum 2",
    TRUE ~ species
  ))



# NOR.hi.warm.vege.wf.10 is Taraxacum 2
# NOR.hi.warm.vege.nf.09 is Taraxacum 2
# NOR.hi.warm.vege.nf.08 is Taraxacum 2
# NOR.hi.warm.vege.nf.02 is Taraxacum 2
# NOR.hi.ambi.vege.wf.02 is Taraxacum 2
# NOR.hi.ambi.vege.nf.09 is Taraxacum 2
# NOR.hi.ambi.vege.nf.08 is Taraxacum 2

# NOR.hi.ambi.vege.wf.10 is Taraxacum sp.
# because it's sp. in 23
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "Taraxacum 2" & unique_plot_ID == "NOR.hi.ambi.vege.wf.10" 
    ~ "Taraxacum sp.",
    TRUE ~ species
  ))

# NOR.hi.ambi.vege.nf.10 is probably Taraxacum sp.
# because it's sp. in 23
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "Taraxacum" & unique_plot_ID == "NOR.hi.ambi.vege.nf.10" 
    ~ "Taraxacum sp.",
    species == "Taraxacum 2 " & unique_plot_ID == "NOR.hi.ambi.vege.nf.10" 
    ~ "Taraxacum sp.",
    TRUE ~ species
  ))

# NOR.hi.ambi.vege.nf.06 has Taraxacum sp. and 2
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "Taraxacum sp" & unique_plot_ID == "NOR.hi.ambi.vege.nf.06" 
    ~ "Taraxacum sp.",
    TRUE ~ species
  ))

# change all Taraxacum sp to sp.
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "Taraxacum sp" ~ "Taraxacum sp.",
    TRUE ~ species
  ))

# 214: Trichophorum cae in NOR.hi.warm.vege.wf.06
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "Trichophorum cae" & unique_plot_ID == "NOR.hi.warm.vege.wf.06" 
    ~ "Trichophorum cespitosum",
    TRUE ~ species
  ))

# 220: Vaccinium myrtilles in NOR.hi.warm.vege.nf.01
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "Vaccinium myrtilles" & unique_plot_ID == "NOR.hi.warm.vege.nf.01" 
    ~ "Vaccinium myrtillus",
    TRUE ~ species
  ))

# 224: Vaccinium vitis-idea
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "Vaccinium vitis-idea" ~ "Vaccinium vitis-idaea",
    TRUE ~ species
  ))

# 229: Veronica ?alpina: could not find this one
# 230: Veronica alpina: this should be fine
# 231: Veronica officinalis: this should be fine
# Veronica officinalis? is without value in 23
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |>
  filter(!(species == "Veronica officinalis?" & total_cover == "0"))

# 232: Viccia cracca 
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "Viccia cracca" ~ "Vicia cracca",
    TRUE ~ species
  ))

# 234: Viola 2
# 235: Viola 2 (canina?)
# 238: Viola riviana should be Viola riviniana
# 241: Viola sp
# 241: Viola sp 2
# 243: Voila palustris should be Viola palustris
# Violas are a mess
# there is at least two, maybe hybridizing
# Viola palustris most common
# some V. riviniana and some V. canina

# just fix spelling for now
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "Viola riviana" ~ "Viola riviniana",
    species == "Voila palustris" ~ "Viola palustris",
    TRUE ~ species
  ))

# 245: moss
# we don't record moss
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |>
  filter(!(species == "moss"))

# 246: rosette in NOR.hi.warm.vege.wf.06
# is Ajuga pyramidalis: datasheet from 22
# BUT: it's in differnt subplots?
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "rosette" & unique_plot_ID == "NOR.hi.warm.vege.wf.06" 
    ~ "Ajuga pyramidalis",
    TRUE ~ species
  ))

# 247: 	sedge 7
# not recorded in 22 and later
# delete? 

# Carex ?pilulifera: NOR.hi.ambi.vege.nf.01
# accept
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "Carex ?pilulifera" & unique_plot_ID == "NOR.hi.ambi.vege.nf.01" 
    ~ "Carex pilulifera",
    TRUE ~ species
  ))

# Carex ?capillaris: NOR.hi.ambi.vege.nf.01
# accept
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "Carex ?capillaris" & unique_plot_ID == "NOR.hi.ambi.vege.nf.01" 
    ~ "Carex cf. capillaris",
    TRUE ~ species
  ))

# Picea abies: NOR.hi.ambi.vege.nf.01, hi 1d
# accept ? in subturf 10 because it was also recorded in 23
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |>
  mutate(`10` = if_else(
    species == "Picea abies" & 
      unique_plot_ID == "NOR.hi.ambi.vege.nf.01" & 
      year == 2021, 
    "1", 
    `10`
  ))

# Hypericum maculatum in NOR.hi.ambi.vege.wf.02
# is the focal * because it was the focal in 23 
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "Hypericum maculatum" & unique_plot_ID == "NOR.hi.ambi.vege.wf.02" ~ "Hypericum maculatum*",
    TRUE ~ species
  ))

# Trifolium pratense in NOR.hi.ambi.vege.wf.02
# is the focal * because it was the focal in 23 
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "Trifolium pratense" & unique_plot_ID == "NOR.hi.ambi.vege.wf.02" ~ "Trifolium pratense*",
    TRUE ~ species
  ))

# Sagina sp is Sagina saginoides in NOR.hi.ambi.vege.wf.03
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "Sagina sp" & unique_plot_ID == "NOR.hi.ambi.vege.wf.03" ~ "Sagina saginoides",
    TRUE ~ species
  ))

# ?Veronica officinalis: NOR.hi.warm.vege.nf.03
# 2021: rosette
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "?Veronica officinalis" 
    & unique_plot_ID == "NOR.hi.warm.vege.nf.03" ~ "cf. Veronica officinalis",
    TRUE ~ species
  ))

# Carex bigelowii?: NOR.hi.warm.vege.nf.03
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "Carex bigelowii?" & unique_plot_ID == "NOR.hi.warm.vege.nf.03" ~ "Carex cf. bigelowii",
    TRUE ~ species
  ))

# Carex leparina: NOR.hi.warm.vege.nf.03: should be C. leporina
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "Carex leparina" & unique_plot_ID == "NOR.hi.warm.vege.nf.03" ~ "Carex leporina",
    TRUE ~ species
  ))

# Succisa pratensis*: NOR.hi.warm.vege.wf.01 hi1a
# 4(dead)
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |>
  mutate(`3` = if_else(
    species == "Succisa pratensis*" & 
      unique_plot_ID == "NOR.hi.warm.vege.wf.01" & 
      year == 2021, 
    "4", 
    `3`
  ))

# violas: NOR.hi.ambi.vege.nf.03
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "Viola sp" & unique_plot_ID == "NOR.hi.ambi.vege.nf.03" ~ "Viola palustris",
    species == "Viola riviniana?" & unique_plot_ID == "NOR.hi.ambi.vege.nf.03" ~ "Viola cf. riviniana",
    TRUE ~ species
  ))


# Viola sp and sp2 hi 4a:NOR.hi.warm.vege.wf.04
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "Viola sp" & unique_plot_ID == "NOR.hi.warm.vege.wf.04" ~ "Viola palustris",
    species == "Viola sp 2" & unique_plot_ID == "NOR.hi.ambi.vege.nf.03" ~ "Viola cf. riviniana",
    TRUE ~ species
  ))

# Sedge 3: hi4c is Carex panicea as said in 22 NOR.hi.warm.vege.nf.04
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "Sedge 3" & unique_plot_ID == "NOR.hi.warm.vege.nf.04" ~ "Carex panicea",
    TRUE ~ species
  ))

# Viola sp and sp2 hi 4a:NOR.hi.warm.vege.nf.04
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "Viola sp" & unique_plot_ID == "NOR.hi.warm.vege.nf.04" ~ "Viola palustris",
    species == "Viola sp 2" & unique_plot_ID == "NOR.hi.ambi.vege.nf.03" ~ "Viola cf. riviniana",
    TRUE ~ species
  ))

# Omalotheca sylvatica is norwegica in hi4d: NOR.hi.ambi.vege.nf.04
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "Omalotheca sylvatica" & unique_plot_ID == "NOR.hi.ambi.vege.nf.04" ~ "Omalotheca norvegica",
    species == "Omalotheca norvegica?" & unique_plot_ID == "NOR.hi.ambi.vege.nf.04" ~ "Omalotheca norvegica",
    TRUE ~ species
  ))

# Viola sp is palustris in hi4d: NOR.hi.ambi.vege.nf.04
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "Viola sp" & unique_plot_ID == "NOR.hi.ambi.vege.nf.04" ~ "Viola palustris",
    TRUE ~ species
  ))

# Euphrasia frigida might be stricta in hi5a: NOR.hi.warm.vege.wf.05
# because vigdis said so in 23
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "Euphrasia frigida" & unique_plot_ID == "NOR.hi.warm.vege.wf.05" ~ "Euphrasia stricta",
    TRUE ~ species
  ))

# Selaginella is selaginoides? NOR.hi.warm.vege.wf.05
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "Selaginella" & unique_plot_ID == "NOR.hi.warm.vege.wf.05" ~ "Selaginella selaginoides",
    TRUE ~ species
  ))

# Viola sp is palustris in hi5a: NOR.hi.warm.vege.wf.05
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "Viola sp" & unique_plot_ID == "NOR.hi.warm.vege.wf.05" ~ "Viola palustris",
    TRUE ~ species
  ))

# Poa pratensis? is ok, hi5b: NOR.hi.ambi.vege.wf.05
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "Poa pratensis?" & unique_plot_ID == "NOR.hi.ambi.vege.wf.05" ~ "Poa pratensis",
    TRUE ~ species
  ))

# hi5b:
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "Viola sp" & unique_plot_ID == "NOR.hi.ambi.vege.wf.05" ~ "Viola palustris",
    TRUE ~ species
  ))

# hi5c: NOR.hi.warm.vege.nf.05
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "Viola sp" & unique_plot_ID == "NOR.hi.warm.vege.nf.05" ~ "Viola palustris",
    TRUE ~ species
  ))

# hi5d: NOR.hi.ambi.vege.nf.05
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "Viola sp" & unique_plot_ID == "NOR.hi.ambi.vege.nf.05" ~ "Viola palustris",
    TRUE ~ species
  ))

# hi6a: Viola NOR.hi.warm.vege.wf.06
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "Viola sp" & unique_plot_ID == "NOR.hi.warm.vege.wf.06" ~ "Viola palustris",
    TRUE ~ species
  ))

# hi6c: Viola NOR.hi.warm.vege.nf.06
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "Viola sp" & unique_plot_ID == "NOR.hi.warm.vege.nf.06" ~ "Viola palustris",
    TRUE ~ species
  ))

# hi 6d: Veronica ?alpina NOR.hi.ambi.vege.nf.06
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "Veronica ?alpina" & unique_plot_ID == "NOR.hi.ambi.vege.nf.06" ~ "Veronica alpina",
    TRUE ~ species
  ))

# hi 6d: Viola NOR.hi.ambi.vege.nf.06
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "Viola sp" & unique_plot_ID == "NOR.hi.ambi.vege.nf.06" ~ "Viola palustris",
    TRUE ~ species
  ))

# hi 7a: Leontodon sp is with hairs is also autumnalis NOR.hi.warm.vege.wf.07
# delete then
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |>
  filter(!(species == "Leontodon sp" & unique_plot_ID %in% c("NOR.hi.warm.vege.wf.07")))

# hi 7a: Viola NOR.hi.warm.vege.wf.07
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "Viola sp" & unique_plot_ID == "NOR.hi.warm.vege.wf.07" ~ "Viola palustris",
    TRUE ~ species
  ))

# hi 7b: Viola NOR.hi.ambi.vege.wf.07
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "Viola sp" & unique_plot_ID == "NOR.hi.ambi.vege.wf.07" ~ "Viola palustris",
    TRUE ~ species
  ))

# hi 7c: Viola NOR.hi.warm.vege.nf.07
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "Viola sp" & unique_plot_ID == "NOR.hi.warm.vege.nf.07" ~ "Viola palustris",
    TRUE ~ species
  ))

# hi 7d: Viola NOR.hi.ambi.vege.nf.07
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "Viola sp" & unique_plot_ID == "NOR.hi.ambi.vege.nf.07" ~ "Viola palustris",
    TRUE ~ species
  ))

# hi 8a: Viola NOR.hi.warm.vege.wf.08
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "Viola sp" & unique_plot_ID == "NOR.hi.warm.vege.wf.08" ~ "Viola palustris",
    TRUE ~ species
  ))

# hi 8b: Viola NOR.hi.ambi.vege.wf.08
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "Viola sp" & unique_plot_ID == "NOR.hi.ambi.vege.wf.08" ~ "Viola palustris",
    TRUE ~ species
  ))

# hi 8c: Viola NOR.hi.warm.vege.nf.08
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "Viola sp" & unique_plot_ID == "NOR.hi.warm.vege.nf.08" ~ "Viola palustris",
    TRUE ~ species
  ))

# hi 8c: Viola canina is rviniana in 24: NOR.hi.warm.vege.nf.08
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "Viola canina" & unique_plot_ID == "NOR.hi.warm.vege.nf.08" ~ "Viola cf. riviniana",
    TRUE ~ species
  ))

# hi 8d: Viola 2 canina is rviniana in 24: NOR.hi.ambi.vege.nf.08
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "Viola 2" & unique_plot_ID == "NOR.hi.ambi.vege.nf.08" ~ "Viola cf. riviniana",
    species == "Viola 2 (canina?)" & unique_plot_ID == "NOR.hi.ambi.vege.nf.08" ~ "Viola cf. riviniana",
    TRUE ~ species
  ))

# hi8d: Hairy Leontodon is autumnalis: 2 + 5 = 7 in subturf 12? 
# NOR.hi.ambi.vege.nf.08
# and delete hairy leontodon
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |>
  mutate(`12` = if_else(
    species == "Leontodon autumnalis" & 
      unique_plot_ID == "NOR.hi.ambi.vege.nf.08" & 
      year == 2023, 
    "7", 
    `12`
  ))

community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |>
  filter(!(species == "Hairy Leontodon" & unique_plot_ID %in% c("NOR.hi.ambi.vege.nf.08") & year == "2023"))

# hi9b: Luzmul* in 21 and 22 NOR.hi.ambi.vege.wf.09
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "Luzula multiflora" & unique_plot_ID == "NOR.hi.ambi.vege.wf.09" & `6` == "1" ~ "Luzula multiflora*",
    TRUE ~ species
  ))

community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "Luzula multiflora" & unique_plot_ID == "NOR.hi.ambi.vege.wf.09" & `8` == "4" & year == "2021" ~ "Luzula multiflora*",
    TRUE ~ species
  ))

# hi 9b: Viola NOR.hi.ambi.vege.wf.09
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "Viola sp" & unique_plot_ID == "NOR.hi.ambi.vege.wf.09" ~ "Viola palustris",
    TRUE ~ species
  ))

# hi 9c: Viola NOR.hi.warm.vege.nf.09
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "Viola sp" & unique_plot_ID == "NOR.hi.warm.vege.nf.09" ~ "Viola palustris",
    TRUE ~ species
  ))

# hi 9d: Equisetum pratense is arvense in 24: Viola NOR.hi.ambi.vege.nf.09
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "Equisetum pratense" & unique_plot_ID == "NOR.hi.ambi.vege.nf.09" ~ "Equisetum arvense",
    TRUE ~ species
  ))

# hi 10a: Viola NOR.hi.warm.vege.wf.10
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "Viola sp" & unique_plot_ID == "NOR.hi.warm.vege.wf.10" ~ "Viola palustris",
    TRUE ~ species
  ))

# hi 10b: Viola NOR.hi.ambi.vege.wf.10
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "Viola sp" & unique_plot_ID == "NOR.hi.ambi.vege.wf.10" ~ "Viola palustris",
    TRUE ~ species
  ))

# hi10b: Hairy Leontodon is autumnalis: 2 + 3 = 5 in subturf 15? 
# NOR.hi.ambi.vege.wf.10
# and delete hairy leontodon
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |>
  mutate(`15` = if_else(
    species == "Leontodon autumnalis" & 
      unique_plot_ID == "NOR.hi.ambi.vege.wf.10" & 
      year == 2023, 
    "7", 
    `15`
  ))

community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |>
  filter(!(species == "Hairy Leontodon" & unique_plot_ID %in% c("NOR.hi.ambi.vege.wf.10") & year == "2023"))

# hi 10b: Equisetum pratense is arvense in 23: Viola NOR.hi.ambi.vege.wf.10
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "Equisetum pratense" & unique_plot_ID == "NOR.hi.ambi.vege.wf.10" ~ "Equisetum arvense",
    TRUE ~ species
  ))

# hi 10c: Viola NOR.hi.warm.vege.nf.10
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "Viola sp" & unique_plot_ID == "NOR.hi.warm.vege.nf.10" ~ "Viola palustris",
    TRUE ~ species
  ))

# hi 10d: Viola NOR.hi.ambi.vege.nf.10
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "Viola sp" & unique_plot_ID == "NOR.hi.ambi.vege.nf.10" ~ "Viola palustris",
    TRUE ~ species
  ))

# hi 10d: Carex leporina? NOR.hi.ambi.vege.nf.10
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "Carex leporina?" & unique_plot_ID == "NOR.hi.ambi.vege.nf.10" ~ "Carex cf. leporina",
    TRUE ~ species
  ))

# lo 1 a: Poa? is maybe Poa pratensis: NOR.lo.ambi.vege.wf.01
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "Poa?" & unique_plot_ID == "NOR.lo.ambi.vege.wf.01" ~ "Poa pratensis",
    TRUE ~ species
  ))

community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "Viola sp" & unique_plot_ID == "NOR.lo.ambi.vege.wf.01" ~ "Viola palustris",
    TRUE ~ species
  ))

# lo2a: Viola NOR.lo.ambi.vege.wf.02
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "Viola sp" & unique_plot_ID == "NOR.lo.ambi.vege.wf.02" ~ "Viola palustris",
    TRUE ~ species
  ))

# lo3a: Viola NOR.lo.ambi.vege.wf.03
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "Viola sp" & unique_plot_ID == "NOR.lo.ambi.vege.wf.03" ~ "Viola palustris",
    TRUE ~ species
  ))

# lo3a: Carex cf. leporina NOR.lo.ambi.vege.wf.03
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "Carex leporina?" & unique_plot_ID == "NOR.lo.ambi.vege.wf.03" ~ "Carex cf. leporina",
    TRUE ~ species
  ))

community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |>
  mutate(`11` = if_else(
    species == "Carex cf. leporina" & 
      unique_plot_ID == "NOR.lo.ambi.vege.wf.03" & 
      year == 2023, 
    "1", 
    `11`
  ))

# lo3a: Leontodon
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "Leontodon sp" & unique_plot_ID == "NOR.lo.ambi.vege.wf.03" ~ "Leontodon autumnalis",
    TRUE ~ species
  ))

# lo4a: Viola NOR.lo.ambi.vege.wf.04
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "Viola sp" & unique_plot_ID == "NOR.lo.ambi.vege.wf.04" ~ "Viola palustris",
    TRUE ~ species
  ))

# lo5a: Viola NOR.lo.ambi.vege.wf.05
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "Viola sp" & unique_plot_ID == "NOR.lo.ambi.vege.wf.05" ~ "Viola palustris",
    TRUE ~ species
  ))

# lo6a: Viola NOR.lo.ambi.vege.wf.06
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "Viola sp" & unique_plot_ID == "NOR.lo.ambi.vege.wf.06" ~ "Viola palustris",
    TRUE ~ species
  ))

# lo7a: Viola NOR.lo.ambi.vege.wf.07
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "Viola sp" & unique_plot_ID == "NOR.lo.ambi.vege.wf.07" ~ "Viola palustris",
    TRUE ~ species
  ))

# lo7a: Stellaria NOR.lo.ambi.vege.wf.07
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |>
  mutate(`2` = if_else(
    species == "Stellaria graminea" & 
      unique_plot_ID == "NOR.lo.ambi.vege.wf.07" & 
      year == 2023, 
    "1", 
    `2`
  ))

# lo8a: Viola NOR.lo.ambi.vege.wf.08
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "Viola sp" & unique_plot_ID == "NOR.lo.ambi.vege.wf.08" ~ "Viola palustris",
    TRUE ~ species
  ))

# lo8a: Filipendula NOR.lo.ambi.vege.wf.08
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |>
  mutate(`10` = if_else(
    species == "Filipendula ulmaria" & 
      unique_plot_ID == "NOR.lo.ambi.vege.wf.08" & 
      year == 2023, 
    "2", 
    `10`
  ))

# lo9a: Viola NOR.lo.ambi.vege.wf.09
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "Viola sp" & unique_plot_ID == "NOR.lo.ambi.vege.wf.09" ~ "Viola palustris",
    TRUE ~ species
  ))

# lo9a: Fesuca NOR.lo.ambi.vege.wf.09
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "Fesuca pratensis" & unique_plot_ID == "NOR.lo.ambi.vege.wf.09" ~ "Festuca pratensis",
    TRUE ~ species
  ))

# lo10a: Viola NOR.lo.ambi.vege.wf.10
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "Viola sp" & unique_plot_ID == "NOR.lo.ambi.vege.wf.10" ~ "Viola palustris",
    TRUE ~ species
  ))




# turfmapper --------------------------------------------------------------
# hi 3a = hi 3b !! 

# lange tabelle mit jahr species cover subturf presence 
# make extra column "reproductive_capacity" with f in cover
community_data_raw_NOR_fixed_long <- community_data_raw_NOR_fixed |> 
  pivot_longer(
    cols = "1":"16",
    names_to = "subturf",
    values_to = "cover") |>
  filter(cover != 0) |> 
  mutate(reproductive_capacity = if_else(str_detect(cover, "f"), 1, 0),
         cover = str_remove(cover, "f")) |> 
  mutate(subturf = as.numeric(subturf)) |> 
  mutate(cover = as.numeric(cover)) 


# fix Chinese reading style of 2023 ---------------------------------------
# extra table with translation
match_21_23_subplots <- data.frame(
  subturf_2021 = 1:16,
  subturf_2023 = c(1, 5, 9, 13,   # Column 1
                   2, 6, 10, 14,  # Column 2
                   3, 7, 11, 15,  # Column 3
                   4, 8, 12, 16)  # Column 4
)

# Filter the 2023 data
community_data_raw_23 <- community_data_raw_NOR_fixed_long |> 
  filter(year == "2023")

# Join the mapping table to align the 2023 subturf with the 2021 layout
community_data_raw_23 <- community_data_raw_23 |> 
  left_join(match_21_23_subplots, by = c("subturf" = "subturf_2023")) |> 
  mutate(
    subturf = subturf_2021 # Replace 2023 subturf with 2021 equivalent
  ) |> 
  select(-subturf_2021) # Drop unnecessary columns

# Combine updated 2023 data with the rest of the dataset
community_data_raw_NOR_fixed_long <- community_data_raw_NOR_fixed_long |> 
  filter(year != "2023") |> # Exclude the old 2023 data
  bind_rows(community_data_raw_23) # Add the updated 2023 data

# set up subturf grid
grid <- make_grid(ncol = 4)

# test with plot  -----------------------------------
community_data_raw_NOR_fixed_long |>
  mutate(subturf = as.numeric(subturf)) |> 
  filter(unique_plot_ID %in% c("NOR.hi.ambi.vege.nf.03")) |> 
  # mutate(subturf = as.numeric(subturf)) |> 
  # mutate(cover = as.numeric(cover)) |> 
  make_turf_plot(
    data = _,
    year = year, species = species, cover = cover, subturf = subturf,
    site_id = site,
    turf_id = unique_plot_ID,
    grid_long = grid
  )


community_data_raw_NOR_fixed_long <- community_data_raw_NOR_fixed_long |> 
  mutate(year_collector = paste(year, collector, sep = "_"))

# loops through all plots -------------------------------------------------
# Open a single PDF document
pdf("Data/Data_community/Turfmapper_21_22_23_all_plots_fixed_species_names.pdf", width = 8, height = 12)

# Group, nest, and prepare data for plotting
nested_data <- community_data_raw_NOR_fixed_long |> 
  # mutate(year_collector = paste(year, collector, sep = "_")) |> 
  group_by(site, unique_plot_ID) |> # Group by plot-level identifiers only
  nest() |> 
  mutate(
    block_ID = map_chr(data, ~ first(.x$block_ID_original)),
    plot_ID = map_chr(data, ~ first(.x$plot_ID_original)),
    plot_title = glue("{site} {block_ID} {plot_ID} : {unique_plot_ID}")
  )


# Loop through the plots and add them to the PDF
walk2(
  .x = nested_data$data,
  .y = nested_data$plot_title,
  .f = ~{
    # Ensure .x contains data for both years for the plot comparison
    if (length(unique(.x$year)) > 1) {
      plot <- make_turf_plot(
        data = .x,
        year = .x$year_collector,  # Pass year_collector instead of just year
        species = .x$species,     # Pass the species column
        cover = .x$cover,         # Pass the cover column
        subturf = .x$subturf,     # Pass the subturf column
        title = .y,               # Use the title from glue
        grid_long = grid          # Assuming grid is predefined
      )
      print(plot) # Add the plot to the PDF
    }
  }
)

# Close the PDF device
dev.off()



# VegSurveyGeneral --------------------------------------------------------
# 1 m2 plot cover data for OSF --------------------------------------------
# keep only relevant columns for OSF
community_data_clean_NOR_plot <- community_data_raw_NOR_fixed |> 
  select(unique_plot_ID, date, species, total_cover,
         collector, added_focals)|> 
  rename("date_measurement" = "date",
         "cover" = "total_cover")

# there is a lot of plots where it's more then one day

# 2022 NAs ----------------------------------------------------------------
# not all plots were recorded in 22 so they have NAs and cover = 0
# delete all rows with date_measurement = NA and cover = 0
date_NA <- community_data_clean_NOR_plot |> 
  filter(is.na(date_measurement))

community_data_clean_NOR_plot <- community_data_clean_NOR_plot |>
  filter(!(is.na(date_measurement) & cover == "0"))


# fix date ----------------------------------------------------------------
# these need fixing
# 16.08.2023 - 17.08.2023
# 17.08.23 - 18.08.23
# 26.07.23/14.08.23
# 14.08./16.08.23
# 22.08./23.08.23
# 22.08/23.08.23
# 18.08./19.08.23
# 22.08/23.08.23
# 21.08/22.08.23
# 23.08./24.08.23
# 24.08./28.08.23
# 23.07./25.07./26.07.23
# 28.08./29.08.23
# 29.08./30.08.23
# 4.8./9.8. 23
# 3.8./4.8.23

community_data_clean_NOR_plot <- community_data_clean_NOR_plot |> 
  mutate(date_measurement = as.character(date_measurement)) |> 
  mutate(date_measurement = case_when(
    date_measurement ==  "16.08.2023 - 17.08.2023" ~ "2023-08-17",
    date_measurement ==  "17.08.23 - 18.08.23" ~ "2023-08-18",
    date_measurement ==  "26.07.23/14.08.23" ~ "2023-08-14",
    date_measurement ==  "14.08./16.08.23" ~ "2023-08-16",
    date_measurement ==  "22.08./23.08.23" ~ "2023-08-23",
    date_measurement ==  "22.08/23.08.23" ~ "2023-08-23",
    date_measurement ==  "18.08./19.08.23" ~ "2023-08-19",
    date_measurement ==  "21.08/22.08.23" ~ "2023-08-22",
    date_measurement ==  "23.08./24.08.23" ~ "2023-08-24",
    date_measurement ==  "24.08./28.08.23" ~ "2023-08-28",
    date_measurement ==  "23.07./25.07./26.07.23" ~ "2023-07-26",
    date_measurement ==  "28.08./29.08.23" ~ "2023-08-29",
    date_measurement ==  "29.08./30.08.23" ~ "2023-08-30",
    date_measurement ==  "4.8./9.8. 23" ~ "2023-08-09",
    date_measurement ==  "3.8./4.8.23" ~ "2023-08-04",
    TRUE ~ date_measurement
  )) |> 
  mutate(date_measurement = as.Date(date_measurement))

unique(community_data_clean_NOR_plot$date_measurement)

# cover should stay character because of <1 
# or should we change <1 to 1? or 0.1?

# only focals column ------------------------------------------------------
species <- sort(unique(community_data_clean_NOR_plot$species))
species

# added focals is in general in the plot
# only focals says if the cover is only based on planted individuals 
# of a focal or if there are naturally occurring individuals taken into 
# account as well

# we distinguished between focal and wild in the field
# focals are indicated with *
# therefore all focal should be yes and all others NA

# change column name to only_focals

community_data_clean_NOR_plot <- community_data_clean_NOR_plot |> 
  rename("only_focals" = "added_focals")

community_data_clean_NOR_plot <- community_data_clean_NOR_plot |> 
  mutate(only_focals = case_when(species == "Cynosurus cristatus*" ~ "yes",
                                 species == "Centaurea nigra*" ~ "yes",
                                 species == "Hypericum maculatum*" ~ "yes",
                                 species == "Leucanthemum vulgaris*" ~ "yes",
                                 species == "Luzula multiflora*" ~ "yes",
                                 species == "Pimpinella saxifraga*" ~ "yes",
                                 species == "Plantago lanceolata*" ~ "yes",
                                 species == "Silene dioica*" ~ "yes",
                                 species == "Succisa pratensis*" ~ "yes",
                                 species == "Trifolium pratense*" ~ "yes",
                                 TRUE ~ NA_character_))


# arrange better ----------------------------------------------------------
community_data_clean_NOR_plot <- community_data_clean_NOR_plot |> 
  arrange(unique_plot_ID, species, date_measurement)

date_NA <- community_data_clean_NOR_plot |> 
  filter(is.na(date_measurement))

# save clean data cover plot ----------------------------------------------
#write.csv(community_data_clean_NOR_plot, "Data/Data_community/CleanVegSurvey/RangeX_clean_VegSurveyGeneral_21_22_23_NOR.csv", row.names = FALSE)
veg_survey_general <- read_csv("Data/Data_community/CleanVegSurvey/RangeX_clean_VegSurveyGeneral_21_22_23_NOR.csv")


# VegSurveyNOR ------------------------------------------------------------
# 16 sub plots cover data for OSF --------------------------------------------
# keep only relevant columns for OSF
community_data_clean_NOR_subplots <- community_data_raw_NOR_fixed_long |> 
  select(unique_plot_ID, date, species, subturf, cover,
         collector, reproductive_capacity)|> 
  rename("date_measurement" = "date",
         "subplot" = "subturf")

date_NA <- community_data_clean_NOR_subplots |> 
  filter(is.na(date_measurement))

# fix date ----------------------------------------------------------------
# these need fixing
# 16.08.2023 - 17.08.2023
# 17.08.23 - 18.08.23
# 26.07.23/14.08.23
# 14.08./16.08.23
# 22.08./23.08.23
# 18.08./19.08.23
# 22.08/23.08.23
# 21.08/22.08.23
# 23.08./24.08.23
# 24.08./28.08.23
# 23.07./25.07./26.07.23
# 28.08./29.08.23
# 29.08./30.08.23
# 4.8./9.8. 23
# 3.8./4.8.23

community_data_clean_NOR_subplots <- community_data_clean_NOR_subplots |> 
  mutate(date_measurement = as.character(date_measurement)) |> 
  mutate(date_measurement = case_when(
    date_measurement ==  "16.08.2023 - 17.08.2023" ~ "2023-08-17",
    date_measurement ==  "17.08.23 - 18.08.23" ~ "2023-08-18",
    date_measurement ==  "26.07.23/14.08.23" ~ "2023-08-14",
    date_measurement ==  "14.08./16.08.23" ~ "2023-08-16",
    date_measurement ==  "22.08./23.08.23" ~ "2023-08-23",
    date_measurement ==  "22.08/23.08.23" ~ "2023-08-23",
    date_measurement ==  "18.08./19.08.23" ~ "2023-08-19",
    date_measurement ==  "21.08/22.08.23" ~ "2023-08-22",
    date_measurement ==  "23.08./24.08.23" ~ "2023-08-24",
    date_measurement ==  "24.08./28.08.23" ~ "2023-08-28",
    date_measurement ==  "23.07./25.07./26.07.23" ~ "2023-07-26",
    date_measurement ==  "28.08./29.08.23" ~ "2023-08-29",
    date_measurement ==  "29.08./30.08.23" ~ "2023-08-30",
    date_measurement ==  "4.8./9.8. 23" ~ "2023-08-09",
    date_measurement ==  "3.8./4.8.23" ~ "2023-08-04",
    TRUE ~ date_measurement
  )) |> 
  mutate(date_measurement = as.Date(date_measurement))

unique(community_data_clean_NOR_subplots$date_measurement)

# cover should stay character because of <1 
# or should we change <1 to 1? or 0.1?

# 2022 NAs ----------------------------------------------------------------
# not all plots were recorded in 22 so they have NAs and cover = 0
# delete all rows with date_measurement = NA and cover = 0
# community_data_clean_NOR_subplots <- community_data_clean_NOR_subplots |>
#   filter(!(is.na(date_measurement)))

# arrange better ----------------------------------------------------------
community_data_clean_NOR_subplots <- community_data_clean_NOR_subplots |> 
  arrange(unique_plot_ID, species, date_measurement)

date_NA <- community_data_clean_NOR_subplots |> 
  filter(is.na(date_measurement))


# save clean data cover subplots ----------------------------------------------
#write.csv(community_data_clean_NOR_subplots, "Data/Data_community/CleanVegSurvey/RangeX_clean_VegSurveyNOR_21_22_23_NOR.csv", row.names = FALSE)
veg_survey_nor <- read_csv("Data/Data_community/CleanVegSurvey/RangeX_clean_VegSurveyNOR_21_22_23_NOR.csv")



