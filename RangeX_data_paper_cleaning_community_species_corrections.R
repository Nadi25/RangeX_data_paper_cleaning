
# RangeX community cover data cleaning 2  -------------------------------

## Data used: RangeX_raw_comcov_high_2021.xlsx, 
##            RangeX_raw_comcov_low_2021.xlsx,
##            RangeX_raw_comcov_high_2023.xlsx, 
##            RangeX_raw_comcov_low_2023.xlsx,
##            RangeX_metadata_plot_NOR.csv
## Date:      11.06.2025
## Author:    Nadine Arzt
## Purpose:   Cleaning of the species names 2021-2023


# check and discuss! ----------------------------------------------------------
# 2021: is hi 3A and 3B switched? changed to b = NOR.hi.warm.vege.wf.03 and a = NOR.hi.ambi.vege.wf.03

# Circle grass = Festuca pratensis
# Carex flava = Carex demissa

# ?Galium: NOR.lo.ambi.vege.wf.06: lo 6A, 2021-08-30
# ?Melica nutans (grass 2 ): NOR.hi.warm.vege.wf.06, hi 6A: 2021-08-13

# Geranium sylvaticum in NOR.hi.ambi.vege.wf.03

# Cirsium helenoides and Cirsium arvense

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

# Cirsium arvense = C. helenioides = C. heterophyllum

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

# check in the field ------------------------------------------------------
# hi 8d: ?Rubus chamaemorus, NOR.hi.ambi.vege.nf.08 = Filipendula ulmaria

# ?Stellaria graminea at high site?: NOR.hi.warm.vege.nf.06, hi 6C and NOR.hi.ambi.vege.wf.07, hi 7B

# ?Veronica officinalis: NOR.hi.warm.vege.nf.03, hi 3C

# Ajuga?Bald?: NOR.hi.warm.vege.nf.02, hi 2C
# Bold Ajuga?: NOR.hi.ambi.vege.nf.01, hi 1D
# probably Ajia pyramidalis without hairs

# Carex palustris: NOR.hi.warm.vege.wf.06, hi 6A
# potentially Carex pilulifera??
# not sure!! Ask Dagmar

# Salix sp in hi8b, NOR.hi.ambi.vege.wf.08 --> Salix glauca x phylicifolia ?

# check Taraxacum sp. and Taraxacum 2 - one is hairy - one not



# library -----------------------------------------------------------------
library(openxlsx)

# source community cleaning script 1 --------------------------------------
source("RangeX_data_paper_cleaning_community.R")

community_data_clean_NOR
community_data_raw_NOR

# import file with species names that need to be corrected ----------------
# this was created in script "Check_species_names_with_TNRS.R"
species_names_to_correct <- read.xlsx("Data/Data_community/Species_names_to_correct.xlsx")



# 1: ???
# NOR.hi.ambi.vege.wf.01 (hi, 1B) in 23


# 2: ?Antennaria dioica
# NOR.hi.ambi.vege.wf.04 only in 21
# maybe Omalotheca sylvatic?
community_data_raw_NOR_fixed <- community_data_raw_NOR |> 
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
# both subplots (12, 16) in 23


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
    species == "carex capillaris" & unique_plot_ID == "NOR.hi.ambi.vege.wf.07" ~ "Carex capillaris",
    TRUE ~ species
  ))

# 13: ?Stellaria graminea ?longifolia: NOR.hi.ambi.vege.nf.06
# same subplot in 23, so we accept
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "?Stellaria graminea ?longifolia" & unique_plot_ID == "NOR.hi.ambi.vege.nf.06" ~ "Stellaria graminea",
    TRUE ~ species
  ))

# 14: ?Veronica officinalis: NOR.hi.warm.vege.nf.03
# not sure! Accept fo rnow but check
community_data_raw_NOR_fixed <- community_data_raw_NOR_fixed |> 
  mutate(species = case_when(
    species == "?Veronica officinalis" & unique_plot_ID == "NOR.hi.warm.vege.nf.03" ~ "Veronica officinalis",
    TRUE ~ species
  ))

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



