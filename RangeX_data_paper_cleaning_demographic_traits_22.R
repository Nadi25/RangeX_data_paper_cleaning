
# ## RangeX data cleaning for traits 2022 -------------------------------------------------

## Data used: RangeX_raw_traits_high_2022.csv, RangeX_raw_traits_low_2022.csv, 
##            RangeX_Metadata.csv, RangeX_YearlyDemographics.csv
## Date:      14.06.23
## Author:    Nadine Arzt
## Purpose:   Clean the complete raw data file 
##            Missing entries? Missing values? 
##            Implausible values? Wrong column names? 
##            Data classes defined? and add treatments & Plant_ID 

# load packages -----------------------------------------------------------

library(dplyr)
library(tidyr) # data manipulation
library(ggplot2) # test-plotting
library(stringr) # working with regex
library("tidyverse")
library(ggstatsplot)

# load data 2022 traits high ---------------------------------------------------------------
traits_high_22 <- read.csv2("Data/Data_demographic_traits/RangeX_raw_traits_high_2022.csv")
head(traits_high_22)

# check structure of data set ---------------------------------------------
str(traits_high_22)
length(traits_high_22) ## 27 columns

## get column names
dput(colnames(traits_high_22))

# delete superfluous columns ----------------------------------------------
## columns x.1- x.15 have only NAs --> delete
traits_high_22 <- traits_high_22 %>%
  select(
    where(
      ~!all(is.na(.x))
    )
  )

# traits_high_22 <- traits_high_22 %>%
#   dplyr::select(-X, -X.1, -X.2, -X.3, -X.4, -X.5, -X.6, -X.7, -X.8, -X.9, -X.10, -X.11,
#                 -X.12, -X.13, -X.14, -X.15)

length(traits_high_22) ## 12 columns

## X is a second column with notes, but there is only one note in it
## delete?
traits_high_22 <- traits_high_22 %>%
  dplyr::select(-X)
length(traits_high_22$block) ## 1200

# change column names -----------------------------------
## get column names
dput(colnames(traits_high_22))

## add column with region = NOR for Norway
traits_high_22 <- traits_high_22 %>%
  add_column(region = "NOR")
traits_high_22

## add column with site = hi for high
traits_high_22 <- traits_high_22 %>%
  add_column(site = "hi")
traits_high_22


## rename column names
traits_high_22 <- traits_high_22 %>%
  rename("block_ID_original" = "block",
         "plot_ID_original" = "treat",
         "position_ID_original" = "coord",
         "height_vegetative_str" = "height",
         "petiole_length" = "petiole_L",
         "leaf_length" = "leaf_L",
         "leaf_width" = "leaf_W",
         "number_flowers" = "flowers")


# load data 2022 traits low ---------------------------------------------------------------

traits_low_22 <- read.csv2("Data/Data_demographic_traits/RangeX_raw_traits_low_2022.csv")
head(traits_low_22)

# check structure of data set ---------------------------------------------
str(traits_low_22)
length(traits_low_22) ## 15 columns
length(traits_low_22$block) ## 960 rows --> only supposed to have 600

# delete superfluous columns with NAs -------------------------------------
traits_low_22 <- traits_low_22 %>%
  select(
    where(
      ~!all(is.na(.x))
    )
  )
head(traits_low_22)

# delete superfluous rows with NAs ----------------------------------------
## remove rows at the end, which have only NAs or nothing in it
traits_low_22 <- traits_low_22 %>% drop_na(block)
length(traits_low_22$block) ## 600 rows

# change column names -----------------------------------
## get column names
dput(colnames(traits_low_22))
## add column with region = NOR for Norway
traits_low_22 <- traits_low_22 %>%
  add_column(region = "NOR")
traits_low_22

## add column with site = lo for low
traits_low_22 <- traits_low_22 %>%
  add_column(site = "lo")
traits_low_22

## rename column names
traits_low_22 <- traits_low_22 %>%
  rename("block_ID_original" = "block",
         "plot_ID_original" = "treat",
         "position_ID_original" = "coord",
         "height_vegetative_str" = "height",
         "petiole_length" = "petiole_L",
         "leaf_length" = "leaf_L",
         "leaf_width" = "leaf_W",
         "number_flowers" = "flowers")
traits_low_22
#

# merge data traits high with traits low ---------------------------------------
## get column names
dput(colnames(traits_high_22))
dput(colnames(traits_low_22))

## combine high and low site
traits_22 <- rbind(traits_high_22, traits_low_22)
head(traits_22)

# sort after site, block, plot, position
traits_22 <- traits_22 %>%
  group_by(site, block_ID_original , plot_ID_original) %>%
  arrange(block_ID_original,plot_ID_original, position_ID_original, .by_group = TRUE)



# add column year ----------------------------------------------------------
traits_22 <- traits_22 %>%
  add_column(year = 2022)
traits_22

class(traits_22$year)



# load metadata file  ------------------------------------------------------
metadata <- read.csv("Data/Metadata/RangeX_clean_MetadataFocal_NOR.csv")
head(metadata)
dput(colnames(metadata))



# merge metadata with trait data ------------------------------------------

dput(colnames(metadata))
dput(colnames(traits_high_22))

traits_2022 <- left_join(traits_22, metadata,
                         by = c("region", "site", "block_ID_original", "plot_ID_original",
                                "position_ID_original", "species"))



na_plants <- traits_2022 %>%
  filter(
    is.na(height_vegetative_str) &
      is.na(petiole_length) &
      is.na(leaf_length) &
      is.na(leaf_width)
  ) %>%
  pull(unique_plant_ID) %>%
  unique()
na_plants
# NOR.lo.ambi.vege.wf.06.14.1: lo 6a f5 cyncri


# reorder column names ----------------------------------------------------
## to get it in the metadata format
dput(colnames(traits_2022))

col_order <- c("region", "site", "block_ID_original", "plot_ID_original", 
               "position_ID_original", "species", "functional_group", "date", "date_planting",
               "treat_warming", "treat_competition", 
               "added_focals", "block_ID", "position_ID", "unique_plot_ID", 
               "unique_plant_ID", "height_vegetative_str", "petiole_length", "leaf_length",
               "leaf_width", 
               "number_flowers", "notes")

traits_2022 <- traits_2022[, col_order]
traits_2022


# date --------------------------------------------------------------------
## change format of date
traits_2022 <- traits_2022 %>% 
  mutate(date = as.Date(date, "%d.%m.%Y"))


# survival  ---------------------------------------------------------------
traits_2022 <- traits_2022 |> 
  mutate(survival = if_else(!is.na(leaf_length) | !is.na(height_vegetative_str), 1, 0))
# actually matches with a lot of dead? comments or not found



# data exploration --------------------------------------------------------

# add column treatment ----------------------------------------------------
## you need a column with the site + treatments: hi_warm_vege
traits_2022_exploration <- traits_2022

traits_2022_exploration$treatment <- paste(traits_2022_exploration$site, 
                                           traits_2022_exploration$treat_warming,
                                           traits_2022_exploration$treat_competition,
                                           sep = "_")

unique(traits_2022_exploration$treatment)



# plot figures to explore data --------------------------------------------
## vegetative height streched for all species and all treatments
ggplot(data = traits_2022_exploration, aes(species, height_vegetative_str, fill = treatment))+
  geom_boxplot()



# Check every column for NAs. ... -----------------------------------------------------------

summary(traits_2022_exploration)

# filter all rows with NA's for height_vegetative_str
traits_2022_height_na <- traits_2022_exploration %>% 
  filter(is.na(height_vegetative_str)) 

length(traits_2022_height_na$height_vegetative_str) ## 45
#
## plants with NAs everywhere are dead/not found --> see notes

## why do some plants not have a height, but leaf length/width 
## maybe forgot to enter or measure data

## hi, 1 f, c2, e4, g6 (NOR.hi.ambi.bare.wf.01.01 / NOR.hi.ambi.bare.wf.01.10 / NOR.hi.ambi.bare.wf.01.18)
## hi, 10 e, d3
## hi, 10 f, d3, g8
## lo, 10 b, d7, h3

## CHECK on raw data file!
## maybe that's because there was no flower, so Nathan measured height as total-flower

## what do we do with these values

## AND what do we do with dead plants
## just leave them in the dataset

# filter all rows with NA's for leaf length
traits_2022_leaf_l_na <- traits_2022_exploration %>% 
  filter(is.na(leaf_length))
#
length(traits_2022_leaf_l_na$leaf_length) ## 50
#
## 13 plants have height and flower number, but no leaf length/width
## in total 33 plants of the high and 17 of the low site have no leaf length 

# filter all rows with NA's for leaf width
traits_2022_leaf_w_na <- traits_2022_exploration %>% 
  filter(is.na(leaf_width))
#
length(traits_2022_leaf_w_na$leaf_width) ## 51
#
## 	hi, 10 a, e8, sildio has leaf length, but no width (also not in raw data sheet)

# number of flowers -------------------------------------------------------
## some values for number of flowers have a star, why
## because there is a comment, like: not blooming yet, fallen off,...
traits_2022_exploration$number_flowers

traits_2022_exploration <- traits_2022_exploration %>% 
  separate(number_flowers, c("number_flowers", "B"))

## column B is empty, but the stars are gone now, so delete column B

traits_2022_exploration <- traits_2022_exploration %>% 
  dplyr::select(-B)

class(traits_2022_exploration$number_flowers)
## is character, but should be numeric
## change to numeric
traits_2022_exploration$number_flowers <- as.numeric(traits_2022_exploration$number_flowers)

## several silene dioica plants have more than 100 flowers
## did they count stems or all flowers
## sildio can have many flowers


# date --------------------------------------------------------------------
## change format of date
traits_2022_exploration <- traits_2022_exploration %>% 
  mutate(date = as.Date(date, "%d.%m.%y"))



# leaf length -------------------------------------------------------------

## plot leaf length against leaf width
ggplot(data = traits_2022_exploration, aes(leaf_length, leaf_width, color = treatment, shape = site))+
  geom_point()+
  facet_wrap(~ species)

## plalan has very long leaves



# filter per species Plantago ------------------------------------------------------

plalan <- traits_2022_exploration %>%  filter(.,(species == "plalan"))
head(plalan)
plalan$height_vegetative_str
class(plalan$height_vegetative_str)

## plot leaf length against leaf width
ggplot(data = plalan, aes(leaf_length, leaf_width, color = treatment))+
  geom_point()



# detect and delete outliers for leaf length ---------------------------------------------------------
## https://www.r-bloggers.com/2020/01/how-to-remove-outliers-in-r/

## it probably removes the upper and lower 25% of the data points


## only plalan
# Create a boxplot of the dataset, outliers are shown as two distinct points
boxplot(plalan$leaf_length)$out

#Create a boxplot that labels the outliers  
ggbetweenstats(plalan, treatment, leaf_length, outlier.tagging = TRUE)



## use whole dataset
# Create a boxplot of the dataset, outliers are shown as two distinct points
boxplot(traits_2022_exploration$leaf_length)$out

#Create a boxplot that labels the outliers per treatment
ggbetweenstats(traits_2022_exploration, treatment, leaf_length, outlier.tagging = TRUE)
#per site
ggbetweenstats(traits_2022_exploration, site, leaf_length, outlier.tagging = TRUE)
#

Q <- quantile(traits_2022_exploration$leaf_length, probs=c(.25, .75), na.rm = T)
Q

iqr <- IQR(traits_2022_exploration$leaf_length, na.rm = T)
iqr

up <-  Q[2]+1.5*iqr # Upper Range  
low<- Q[1]-1.5*iqr # Lower Range

eliminated<- subset(traits_2022_exploration, traits_2022_exploration$leaf_length > (Q[1] - 1.5*iqr) 
                    & traits_2022_exploration$leaf_length < (Q[2]+1.5*iqr))
eliminated

ggbetweenstats(eliminated, treatment, leaf_length, outlier.tagging = TRUE) 


length(eliminated$leaf_length) ## 1668
length(traits_2022_exploration$leaf_length) ## 1800
## 132 values have been deleted

## plot leaf length against leaf width for all species
ggplot(data = eliminated, aes(leaf_length, leaf_width, color = treatment))+
  geom_point()+
  facet_wrap( ~ species)



## filter only plalan
plalan_eliminated <- eliminated %>%  filter(.,(species == "plalan"))
head(plalan_eliminated)
plalan_eliminated$leaf_length
max(plalan_eliminated$leaf_length)

## plot leaf length against leaf width
ggplot(data = plalan_eliminated, aes(leaf_length, leaf_width, color = treatment))+
  geom_point()

## the function deleted all the values above 300 mm for leaf length

## big question: can I delete these values that the function considered as outliers?
## problem: plants that are dead, so e.g. without height are now deleted?

## filter only pimsax
pimsax_eliminated <- eliminated %>%  filter(.,(species == "pimsax"))
head(pimsax_eliminated)
pimsax_eliminated$leaf_length
max(pimsax_eliminated$leaf_length)

## plot leaf length against leaf width
ggplot(data = pimsax_eliminated, aes(leaf_length, leaf_width, color = treatment))+
  geom_point()
## there are still values that look like outliers
## length 225 mm and width 3 mm



# load metadata for data entry Yearly demographics ----------------------------------------------------------

yearly_demographics <- read.csv("Data/Data_demographic_traits/RangeX_YearlyDemographics.csv")
head(yearly_demographics)
dput(colnames(yearly_demographics))



# adapt traits_2022 in the format of yearly demographics ------------------
## !! use traits_2022_exploration !! or eliminated !!

# ## add column with year
# traits_2022 <- traits_2022 %>%
#   add_column(year = "2022")
# head(traits_2022)
# 

## add all of these columns:
# "height_reproductive_str", "height_vegetative", "height_reproductive", 
# "vegetative_width", "vegetative_length", "stem_diameter", "leaf_length1", 
# "leaf_length2", "leaf_length3", "number_tillers", "number_branches", "number_leafclusters", 
# "mean_inflorescence_size", "herbivory"

## collector is not so easy, because its not recorded at the digitized table

traits_2022_exploration <- traits_2022_exploration %>%
  dplyr::mutate(
    collector = "NP", # have Nathan for now and update later
    height_reproductive_str = NA,
    height_vegetative = NA,
    height_reproductive = NA,
    vegetative_width = NA,
    height_total = NA,
    #vegetative_length = NA,
    stem_diameter = NA,
    leaf_length1 = NA,
    leaf_length2 = NA,
    leaf_length3 = NA,
    number_leaves = NA,
    # petiole_length1 = NA, 
    # petiole_length2 = NA,
    # petiole_length3 = NA,
    number_tillers = NA,
    number_branches = NA,
    number_leafclusters = NA,
    mean_inflorescence_size = NA,
    herbivory = NA
  )

dput(colnames(traits_2022_exploration))


na_plants <- traits_2022_exploration %>%
  filter(
    is.na(height_vegetative_str) &
      is.na(petiole_length) &
      is.na(leaf_length) &
      is.na(leaf_width) &
      is.na(number_flowers)
  ) %>%
  pull(unique_plant_ID) %>%
  unique()
na_plants
# 36 plants

## delete "region", "site", "block_ID_original", "plot_ID_original", 
## "position_ID_original","treat_warming", "treat_competition", 
## "added_focals", "block_ID", "position_ID", "unique_plot_ID"

rangex_traits_22 <- traits_2022_exploration %>%
  dplyr::select(-region, -site, -block_ID_original, -plot_ID_original, 
                -position_ID_original, -treat_warming, -treat_competition, 
                -added_focals, -block_ID, -position_ID, -unique_plot_ID) %>% 
  dplyr::ungroup()

## Adding missing grouping variables: `site`, `block_ID_original`, `plot_ID_original`
## WHY cant I delete them

dput(colnames(rangex_traits_22))
length(rangex_traits_22) # 30
length(yearly_demographics) # 23


## make correct order as in yearly_demographics
col_order_traits_22 <- c("site", "block_ID_original", "plot_ID_original","unique_plant_ID", 
                         "species", "functional_group", "date", "date_planting", "collector", 
                         "survival",
                         "height_vegetative_str", 
                         "height_reproductive_str", "height_vegetative", "height_reproductive", 
                         "vegetative_width", "height_total", "stem_diameter", "leaf_length", 
                         "leaf_length1", "leaf_length2", "leaf_length3", "leaf_width", 
                         "petiole_length", 
                         "number_leaves", "number_tillers", "number_branches", 
                         "number_flowers", "mean_inflorescence_size", "herbivory")

rangex_traits_22 <- rangex_traits_22[, col_order_traits_22]
rangex_traits_22

## put values from leaf_length in column leaf_length1
## same with petiole_length
rangex_traits_22 <- rangex_traits_22 %>%
  dplyr::mutate(leaf_length1 = dplyr::coalesce(leaf_length1, leaf_length)) %>%
  #dplyr::mutate(petiole_length1 = dplyr::coalesce(petiole_length1, petiole_length)) %>%
  dplyr::select(-leaf_length)

## delete site, block_ID_original, plot_ID_original
rangex_traits_22 <- rangex_traits_22 %>%
  dplyr::select(-site, -block_ID_original, -plot_ID_original)


# rename date -------------------------------------------------------------
rangex_traits_22 <- rangex_traits_22 |> 
  rename(date_measurement = date)


## now the data frame should have the correct format of yearly_demographics



# save csv file -----------------------------------------------------------
# write.csv(rangex_traits_22, "Data/Data_demographic_traits/Clean_YearlyDemographics/RangeX_clean_YearlyDemographics_NOR_2022.csv", row.names = FALSE)

## read cleaned data
data_nor_22 <- read.csv("Data/Data_demographic_traits/RangeX_clean_YearlyDemographics_NOR_2022.csv")























