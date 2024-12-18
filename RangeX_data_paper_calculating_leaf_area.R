

# RangeX functional traits calculating leaf area --------------------------

## Data used: leaf scans raw in: C:/Users/naart3294/OneDrive - University of Bergen/PhD_RangeX/DATA_2023/focal_level/Functional_traits/Leaf_area_scans
## RangeX_Metadata.csv / RangeX_YearlyDemographics.csv
## Date: 11.11.24
## Author: Nadine Arzt
## Purpose: Calculate leaf area of focal species


# load packages -----------------------------------------------------------
library(LeafArea)
library(tidyverse)


# Function to run several files -------------------------------------------

loop.files <-  function(files){
  file.copy(files, new.folder)
  if(grepl("-NA$", files)){
    newfile <- basename(files)
    file.rename(paste0(new.folder, "/", newfile), paste0(new.folder,
                                                         "/", gsub("-NA$", "", newfile)))
  }
  print(files)
  area <- try(run.ij(path.imagej = "C:/Users/naart3294/OneDrive - University of Bergen/Desktop/ImageJ/", set.directory = new.folder, distance.pixel = 237, known.distance = 2, log = TRUE, low.size = 0.005, trim.pixel = 60, trim.pixel.right = 150, save.image = TRUE))
  # more cropping
  #area <- try(run.ij(set.directory = new.folder, distance.pixel = 237, known.distance = 2, log = TRUE, low.size = 0.005, trim.pixel = 200, trim.pixel2 = 0, save.image = TRUE))
  if(inherits(area, "try-error")){
    return(data.frame(LeafArea = NA))
  }
  file.copy(dir(new.folder, full.names = TRUE, pattern = "\\.tif"), output.folder)
  Sys.sleep(0.1)
  if(any(!file.remove(dir(new.folder, full.names = TRUE) ))) stop()
  res <- data.frame(dir = dirname(files), ID = names(unlist(area[[2]])), LeafArea = (unlist(area[[2]])))
  return(res)
}

# new.folder is temporary and stays the same for every folder with scans
new.folder <- "C:/Users/naart3294/OneDrive - University of Bergen/PhD_RangeX/R codes/RangeX_data_paper_cleaning/temp"


# test run.ij ----------------------------------------------------------------

dd <- run.ij(path.imagej = "C:/Users/naart3294/OneDrive - University of Bergen/Desktop/ImageJ/", set.directory = "C:/Users/naart3294/OneDrive - University of Bergen/PhD_RangeX/R codes/RangeX_data_paper_cleaning/Data/Test", distance.pixel = 237, known.distance = 2, log = TRUE, low.size = 0.1, trim.pixel = 58, trim.pixel.right = 150, save.image = TRUE)
dd$summary

# sample total.leaf.area
# 1 FRS5727           5.451
# 2 FRT5305           1.919
# 3 FRU0110           5.810
# 4 FRV5534           1.752
# 5 FRW2929          18.611
# 6 FRX6087          11.573
# 7 FRY5906           3.999

# seems to work


# Calculate leaf area from 2023 data --------------------------------------

# make a list of files, temporary folder and output folder


# 1. RangeX_leaf_scans_high_28.07.23 --------------------------------------

# always run this list.of.files first
list.of.files <- dir(path = paste0("Data/Leaf_area_scans/RangeX_leaf_scans_high_28.07.23/"), pattern = "jpeg|jpg", recursive = TRUE, full.names = TRUE)

# new.folder is temporary and stays the same for every folder with scans
new.folder <- "C:/Users/naart3294/OneDrive - University of Bergen/PhD_RangeX/R codes/RangeX_data_paper_cleaning/temp"

# create a different output folder for every folder of scans
output.folder <- "C:/Users/naart3294/OneDrive - University of Bergen/PhD_RangeX/R codes/RangeX_data_paper_cleaning/Data/Output/Output_RangeX_leaf_scans_high_28.07.23"

# this does the magic of calculating the leaf area for all scans in the specified folder
LA_1 <- plyr::ldply(list.of.files, loop.files)
LA_1

# write.csv(LA_1, file = "Data/RangeX_raw_functional_traits_leaf_area_high_28.07.23.csv")


# 2. RangeX_leaf_scans_low_06.07.23 ---------------------------------------

# always run this list.of.files first
list.of.files <- dir(path = paste0("Data/Leaf_area_scans/RangeX_leaf_scans_low_06.07.23/"), pattern = "jpeg|jpg", recursive = TRUE, full.names = TRUE)

# new.folder is temporary and stays the same for every folder with scans
new.folder <- "C:/Users/naart3294/OneDrive - University of Bergen/PhD_RangeX/R codes/RangeX_data_paper_cleaning/temp"

# create a different output folder for every folder of scans
output.folder <- "C:/Users/naart3294/OneDrive - University of Bergen/PhD_RangeX/R codes/RangeX_data_paper_cleaning/Data/Output/Output_RangeX_leaf_scans_low_06.07.23/"

# this does the magic of calculating the leaf area for all scans in the specified folder
LA_2 <- plyr::ldply(list.of.files, loop.files)
LA_2

# write.csv(LA_2, file = "Data/RangeX_raw_functional_traits_leaf_area_low_06.07.23.csv")



# 3. RangeX_leaf_scans_low_07.07.23 ---------------------------------------

# always run this list.of.files first
list.of.files <- dir(path = paste0("Data/Leaf_area_scans/RangeX_leaf_scans_low_07.07.23/"), pattern = "jpeg|jpg", recursive = TRUE, full.names = TRUE)

# new.folder is temporary and stays the same for every folder with scans
new.folder <- "C:/Users/naart3294/OneDrive - University of Bergen/PhD_RangeX/R codes/RangeX_data_paper_cleaning/temp"

# create a different output folder for every folder of scans
output.folder <- "C:/Users/naart3294/OneDrive - University of Bergen/PhD_RangeX/R codes/RangeX_data_paper_cleaning/Data/Output/Output_RangeX_leaf_scans_low_07.07.23/"

# this does the magic of calculating the leaf area for all scans in the specified folder
LA_3 <- plyr::ldply(list.of.files, loop.files)
LA_3

# write.csv(LA_3, file = "Data/RangeX_raw_functional_traits_leaf_area_low_07.07.23.csv")




# 4. RangeX_leaf_scans_DarthVader ---------------------------------------

# always run this list.of.files first
list.of.files <- dir(path = paste0("Data/Leaf_area_scans/RangeX_leaf_scans_DarthVader/"), pattern = "jpeg|jpg", recursive = TRUE, full.names = TRUE)

# new.folder is temporary and stays the same for every folder with scans
new.folder <- "C:/Users/naart3294/OneDrive - University of Bergen/PhD_RangeX/R codes/RangeX_data_paper_cleaning/temp"

# create a different output folder for every folder of scans
output.folder <- "C:/Users/naart3294/OneDrive - University of Bergen/PhD_RangeX/R codes/RangeX_data_paper_cleaning/Data/Output/Output_RangeX_leaf_scans_DarthVader/"

# this does the magic of calculating the leaf area for all scans in the specified folder
LA_4 <- plyr::ldply(list.of.files, loop.files)
LA_4

# write.csv(LA_4, file = "Data/RangeX_raw_functional_traits_leaf_area_DarthVader.csv")




# combine LA_1, LA_2, LA_3, LA_4 ----------------------------------------------

# load leaf areas
LA_1 <- read.csv("Data/RangeX_raw_functional_traits_leaf_area_high_28.07.23.csv")

LA_2 <- read.csv("Data/RangeX_raw_functional_traits_leaf_area_low_06.07.23.csv")

LA_3 <- read.csv("Data/RangeX_raw_functional_traits_leaf_area_low_07.07.23.csv")

LA_4 <- read.csv("Data/RangeX_raw_functional_traits_leaf_area_DarthVader.csv")


Leaf_area_all <- bind_rows(LA_1, LA_2, LA_3, LA_4)


# calculate leaf area per leaf --------------------------------------------

# calculate sums
leaf_area <- Leaf_area_all |>
  # extract everything before point
  mutate(ID = sub("\\..*", "", ID)) |>
  group_by(dir, ID) |>
  summarise(n = n(),
            leaf_area = sum(LeafArea))
# save data as csv
dim(leaf_area)


head(leaf_area)
table(leaf_area$ID)


# check for duplicates ----------------------------------------------------

duplicates <- leaf_area[duplicated(leaf_area$ID), ]
duplicates


# remove duplicates -------------------------------------------------------

leaf_area_NOR <- leaf_area[!duplicated(leaf_area$ID), ]
leaf_area_NOR


write.csv(leaf_area_NOR, file = "Data/RangeX_raw_functional_traits_leaf_area_NOR_2023.csv")













