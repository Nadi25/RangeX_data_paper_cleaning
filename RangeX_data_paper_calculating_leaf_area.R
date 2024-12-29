
# RangeX functional traits calculating leaf area --------------------------

## Leaf scans raw in: C:\Users\naart3294\OneDrive - University of Bergen\PhD_RangeX\R codes\RangeX_data_paper_cleaning\Data\Leaf_area_scans\RangeX_leaf_scans_all
## Code used:    Calculate leaf area with ImageJ function 
##               in Leaf_area_calculation_function.R
## Leaf area in: Data/RangeX_raw_functional_traits_leaf_area_all.csv
## Date:         11.11.24
## Author:       Nadine Arzt
## Purpose:      Calculate leaf area of focal species


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
# 1 FMO8773          32.588
# 2 FMR8490           7.847
# 3 FNH0448           8.408
# 4     git           0.000

# seems to work except GDR8049


# Calculate leaf area from 2023 data --------------------------------------

# make a list of files, temporary folder and output folder

# always run this list.of.files first
list.of.files <- dir(path = paste0("Data/Leaf_area_scans/RangeX_leaf_scans_all/"), pattern = "jpeg|jpg", recursive = TRUE, full.names = TRUE)

# new.folder is temporary and stays the same for every folder with scans
new.folder <- "C:/Users/naart3294/OneDrive - University of Bergen/PhD_RangeX/R codes/RangeX_data_paper_cleaning/temp"

# create a different output folder for every folder of scans
output.folder <- "C:/Users/naart3294/OneDrive - University of Bergen/PhD_RangeX/R codes/RangeX_data_paper_cleaning/Data/Output/Output_RangeX_leaf_scans_all/"

# this does the magic of calculating the leaf area for all scans in the specified folder
LA_1 <- plyr::ldply(list.of.files, loop.files)
LA_1

# write.csv(LA_1, file = "Data/RangeX_raw_functional_traits_leaf_area_all.csv")

# calculate leaf area per leaf --------------------------------------------

# calculate sums
leaf_area <- LA_1 |>
  # extract everything before point
  mutate(ID = sub("\\..*", "", ID)) |>
  group_by(dir, ID) |>
  summarise(n = n(),
            leaf_area = sum(LeafArea))
# save data as csv
# write.csv(leaf_area, file = "Data/RangeX_raw_functional_traits_leaf_area_all.csv")

dim(leaf_area)

head(leaf_area)
table(leaf_area$ID)

area <- read.csv("Data/RangeX_raw_functional_traits_leaf_area_all.csv")


# check for duplicates ----------------------------------------------------
duplicates <- leaf_area[duplicated(leaf_area$ID), ]
duplicates
# 0 duplicates


