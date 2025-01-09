

# RangeX data cleaning for traits 2021 -------------------------------------------------

## Data used: RangeX_raw_traits_high_2021.csv, RangeX_raw_traits_low_2021.csv, 
##            RangeX_Metadata.csv, RangeX_YearlyDemographics.csv
## Date:      22.06.23
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

# load data 2021 traits high ---------------------------------------------------------------
traits_high_21 <- read.csv2("Data/Data_demographic_traits/RangeX_raw_traits_high_2021.csv")
head(traits_high_21)

# check structure of data set ---------------------------------------------
str(traits_high_21)
length(traits_high_21) ## 27 columns

## get column names
dput(colnames(traits_high_21))

summary(traits_high_21)

# delete superfluous columns ----------------------------------------------
## columns x.1- x.15 have only NAs --> delete
traits_high_21 <- traits_high_21 %>%
  select(
    where(
      ~!all(is.na(.x))
    )
  )

length(traits_high_21) ## 20 columns



# rename columns ----------------------------------------------------------

## the headers are shifted to the left, because there are two additional columns in the beginning
## one is just continues numbers
## it's actually plotID and plantID on the paper sheet

traits_high_21 <- traits_high_21 %>%
  rename("block_ID_original" = "block",
         "plot_ID_original" = "treat",
         "A" = "species",
         "B" = "coord",
         "species" = "leaf_no_LIVE",
         "position_ID_original" = "leaf_no_DEAD",
         "number_leaves_LIVE" = "tillers",
         "number_leaves_DEAD" = "base.width",
         "number_tillers" = "height",
         "vegetative_width" = "petiole_1",
         "height_vegetative_str" = "petiole_2",
         "petiole_length1" = "petiole_3",
         "petiole_length2" = "leaf_L_1",
         "petiole_length3" = "leaf_L_2",
         "leaf_length1" = "leaf_L_3",
         "leaf_length2" = "sam",
         "leaf_length3" = "date",
         "sam" = "notes",
         "date" = "date_measured",
         "notes" = "notes.1")

head(traits_high_21)


summary(traits_high_21)


# add columns -------------------------------------------------------------

## add column with region = NOR for Norway
traits_high_21 <- traits_high_21 %>%
  add_column(region = "NOR")
traits_high_21

## add column with site = hi for high
traits_high_21 <- traits_high_21 %>%
  add_column(site = "hi")
traits_high_21

## add column year
traits_high_21 <- traits_high_21 %>%
  add_column(year = 2021)
traits_high_21

colnames(traits_high_21)

# delete columns ----------------------------------------------------------

traits_high_21 <- traits_high_21 %>%
  dplyr::select(-A, -B)

length(traits_high_21)



# load data 2021 traits low ---------------------------------------------------------------

traits_low_21 <- read.csv2("RangeX_raw_traits_low_2021.csv")
head(traits_low_21)

# check structure of data set ---------------------------------------------
str(traits_low_21)
length(traits_low_21) ## 19 columns
length(traits_low_21$block) ## 960 rows --> only supposed to have 600

# delete superfluous columns with NAs -------------------------------------
traits_low_21 <- traits_low_21 %>%
  select(
    where(
      ~!all(is.na(.x))
    )
  )
head(traits_low_21)

# delete superfluous rows with NAs ----------------------------------------
## remove rows at the end, which have only NAs or nothing in it
traits_low_21 <- traits_low_21 %>% drop_na(block)
length(traits_low_21$block) ## 600 rows



# add columns -------------------------------------------------------------

## add column with region = NOR for Norway
traits_low_21 <- traits_low_21 %>%
  add_column(region = "NOR")
traits_low_21

## add column with site = lo for low
traits_low_21 <- traits_low_21 %>%
  add_column(site = "lo")
traits_low_21

## add column year
traits_low_21 <- traits_low_21 %>%
  add_column(year = 2021)
traits_low_21



# rename columns ----------------------------------------------------------
colnames(traits_low_21)

traits_low_21 <- traits_low_21 %>%
  rename("block_ID_original" = "block",
         "plot_ID_original" = "treat",
         "position_ID_original" = "coord",
         "number_leaves_LIVE" = "leaf_no_LIVE",
         "number_leaves_DEAD" = "leaf_no_DEAD",
         "vegetative_width" = "base.width",
         "height_vegetative_str" = "height",
         "petiole_length1" = "petiole_1",
         "petiole_length2" = "petiole_2",
         "petiole_length3" = "petiole_3",
         "leaf_length1" = "leaf_L_1",
         "leaf_length2" = "leaf_L_2",
         "leaf_length3" = "leaf_L_3",
         "number_tillers" = "tillers")
traits_low_21


# merge data traits high with traits low ---------------------------------------

# check if data frames are equal
all.equal(traits_high_21, traits_low_21)

## get column names
dput(colnames(traits_high_21))
dput(colnames(traits_low_21))

## combine high and low site
traits_21 <- rbind(traits_high_21, traits_low_21)
head(traits_21)

# sort after site, block, plot, position
traits_21 <- traits_21 %>%
  group_by(site, block_ID_original , plot_ID_original) %>%
  arrange(block_ID_original,plot_ID_original, position_ID_original, .by_group = TRUE)

summary(traits_21)

# load metadata file for all countries ------------------------------------------------------
metadata <- read.csv2("RangeX_Metadata.csv")
head(metadata)
dput(colnames(metadata))

## filter only NOR
metadata_NOR <- metadata %>%
  filter(grepl('NOR', region))
head(metadata_NOR)


# improve traits_2021 data set --------------------------------------------

# change species name in one plot -----------------------------------------
## block 9, b, d3 = sucpra instead of leuvul

traits_21[traits_21$site == "lo" & traits_21$block_ID_original == "9" 
          & traits_21$plot_ID_original == "b" 
          & traits_21$position_ID_original == "d3", "species"] <- "sucpra"

# leaf length -------------------------------------------------------------
## 4e, cyncri, i4, leaf_length2 is a typo --> 171 instead of 1711
traits_21[traits_21$site == "hi" & traits_21$block_ID_original == "4" 
          & traits_21$plot_ID_original == "e" 
          & traits_21$position_ID_original == "i4", "leaf_length2"] <- 171


# merge metadata with trait data 21 ------------------------------------------

dput(colnames(metadata_NOR))
dput(colnames(traits_high_21))

traits_2021 <- left_join(traits_21, metadata_NOR,
                         by = c("region", "site", "block_ID_original", "plot_ID_original",
                                "position_ID_original", "species"))


# reorder column names ----------------------------------------------------
## to get it in the metadata format
dput(colnames(traits_2021))

col_order_21 <- c("region", "site", "block_ID_original", "plot_ID_original", 
                  "position_ID_original", "species", "year", "treat_warming", "treat_competition", 
                  "added_focals", "block_ID", "position_ID", "unique_plot_ID", 
                  "unique_plant_ID", "height_vegetative_str", "vegetative_width", 
                  "petiole_length1", "petiole_length2", "petiole_length3", 
                  "leaf_length1", "leaf_length2", "leaf_length3", 
                  "number_tillers", "sam", "date", "notes")

traits_2021 <- traits_2021[, col_order_21]
traits_2021


# vegetative height -------------------------------------------------------

## what has been measured for height_vegetative_str
## in the read_me it says: stretched vegetative plant height (not including inflorescence)
## but on the paper sheet: stem height and plant height (this column is not digitized)
## why did they cross the column plant height - it would make more sense to have this 
## why is there no height for every plant for stem height?

## what does (s) and (f) mean in stem height?

traits_high_21$height_vegetative_str

traits_2021 <- traits_2021 %>% 
  separate(height_vegetative_str, c("height_vegetative_str", "B", "C", "D", "E", "F"))

class(traits_2021$height_vegetative_str)

## replace the "broken" in plot hi, 1, b, c6, plalan

traits_2021[traits_2021$site == "hi" & traits_2021$block_ID_original == "1" 
            & traits_2021$plot_ID_original == "b" 
            & traits_2021$position_ID_original == "c6", "height_vegetative_str"] <- NA


## delete columns B-F
traits_2021 <- traits_2021 %>%
  dplyr::select(-B, -C, -D, -E, -F)

traits_2021$height_vegetative_str <- as.numeric(traits_2021$height_vegetative_str)
class(traits_2021$height_vegetative_str)

# date --------------------------------------------------------------------
## change format of date
traits_2021 <- traits_2021 %>% 
  mutate(date = as.Date(date, "%d.%m.%y"))


# leaf length -------------------------------------------------------------
class(traits_2021$leaf_length2)
traits_2021$leaf_length1 <- as.numeric(traits_2021$leaf_length1)
traits_2021$leaf_length2 <- as.numeric(traits_2021$leaf_length2)
traits_2021$leaf_length3 <- as.numeric(traits_2021$leaf_length3)


# Check every column for NAs. ... -----------------------------------------------------------

summary(traits_2021)

# filter all rows with NA's for height_vegetative_str
traits_2021_height_na <- traits_2021 %>% 
  filter(is.na(height_vegetative_str)) 

length(traits_2021_height_na$height_vegetative_str) ## 
#

# filter all rows with NA's for leaf_length
traits_2021_leaf_length_na <- traits_2021 %>% 
  filter(is.na(leaf_length3)) 

length(traits_2021_leaf_length_na$leaf_length3) ## 1
#
# hi, 8, b, d7, pimsax
## check on raw data sheet


# data exploration --------------------------------------------------------

# add column treatment ----------------------------------------------------
## you need a column with the site + treatments: hi_warm_vege
traits_2021_exploration <- traits_2021

traits_2021_exploration$treatment <- paste(traits_2021_exploration$site, 
                                           traits_2021_exploration$treat_warming,
                                           traits_2021_exploration$treat_competition,
                                           sep = "_")

unique(traits_2021_exploration$treatment)


# plot figures to explore data --------------------------------------------
## vegetative height stretched for all species and all treatments
ggplot(data = traits_2021_exploration, aes(species, height_vegetative_str, fill = treatment))+
  geom_boxplot()

## we only have height values for hypmac and plalan

## that's not so good

## plot leaf length1
ggplot(data = traits_2021_exploration, aes(treatment, leaf_length1, fill = treatment))+
  geom_boxplot()+
  facet_wrap(~species)

## it does not look like big differences between treatments
## remember, its the year, in which they have been plated


# filter per species Plantago ------------------------------------------------------

plalan21 <- traits_2021_exploration %>%  filter(.,(species == "plalan"))
head(plalan21)
plalan21$height_vegetative_str
class(plalan21$height_vegetative_str)

## plot leaf length 
ggplot(data = plalan21, aes(treatment,leaf_length1, color = treatment))+
  geom_point()

## calculate a mean of leaf_length1, 2 and 3 at some point

# plalan_21_leaf_length <- plalan21 %>%
#   mutate(mean_leaf_length = rowMeans(select(., leaf_length1, leaf_length2, leaf_length3), na.rm = TRUE))
# 
# class(plalan21$leaf_length3)
# 
# plalan_21_leaf_length <- plalan21 %>%
#   select(leaf_length1, leaf_length2, leaf_length3, treatment)
# 
# 
# plalan_21_leaf_length <- plalan_21_leaf_length %>%
#   mutate(mean_leaf_length = rowMeans(select(., leaf_length1, leaf_length2, leaf_length3), na.rm = TRUE, dims = 1))
# 






# load metadata for data entry Yearly demographics ----------------------------------------------------------

yearly_demographics <- read.csv("RangeX_YearlyDemographics.csv")
head(yearly_demographics)
dput(colnames(yearly_demographics))


# adapt traits_2022 in the format of yearly demographics ------------------
## !! use traits_2021 !!

## write mail to Evelin about
## adding sam, petiole_length2 and 3 to the yearly demographics


traits_2021 <- traits_2021 %>%
  dplyr::mutate(
    collector = NA,
    height_reproductive_str = NA,
    height_vegetative = NA,
    height_reproductive = NA,
    vegetative_width = NA,
    vegetative_length = NA,
    leaf_width = NA,
    stem_diameter = NA,
    number_leaves = NA,
    number_tillers = NA,
    number_branches = NA,
    number_leafclusters = NA,
    number_flowers = NA,
    mean_inflorescence_size = NA,
    herbivory = NA
  )

dput(colnames(traits_2021))

## delete "region", "site", "block_ID_original", "plot_ID_original", 
## "position_ID_original","treat_warming", "treat_competition", 
## "added_focals", "block_ID", "position_ID", "unique_plot_ID"

rangex_traits_21 <- traits_2021 %>%
  dplyr::select(-region, -site, -block_ID_original, -plot_ID_original, 
                -position_ID_original, -treat_warming, -treat_competition, 
                -added_focals, -block_ID, -position_ID, -unique_plot_ID) %>% 
  dplyr::ungroup()

dput(colnames(rangex_traits_21))
length(rangex_traits_21) # 31
length(yearly_demographics) # 23

## make correct order as in yearly_demographics
col_order_traits_21 <- c("site", "block_ID_original", "plot_ID_original","unique_plant_ID", 
                         "species", "year", "collector", "height_vegetative_str", 
                         "height_reproductive_str", "height_vegetative", "height_reproductive", 
                         "vegetative_width", "vegetative_length", "stem_diameter",
                         "leaf_length1", "leaf_length2", "leaf_length3", "leaf_width",
                         "petiole_length1", "petiole_length2", "petiole_length3",
                         "number_leaves", "number_tillers", "number_branches", "number_leafclusters", 
                         "number_flowers", "mean_inflorescence_size", "sam", "herbivory")

rangex_traits_21 <- rangex_traits_21[, col_order_traits_21]
rangex_traits_21

## delete site, block_ID_original, plot_ID_original
rangex_traits_21 <- rangex_traits_21 %>%
  dplyr::select(-site, -block_ID_original, -plot_ID_original)
rangex_traits_21
## now the data frame should have the correct format of yearly_demographics



# save csv file -----------------------------------------------------------

# write.csv(rangex_traits_21, "C:/Users/nadin/OneDrive - University of Bergen/PhD_RangeX/R codes/RangeX_data_cleaning/Data_traits/RangeX_clean_traits_2021.csv",
#           row.names = FALSE)

## read cleaned data
data_21 <- read.csv("RangeX_clean_traits_2021.csv")














