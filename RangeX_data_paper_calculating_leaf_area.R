
# RangeX functional traits calculating leaf area --------------------------

## Leaf scans raw in: C:\Users\naart3294\OneDrive - University of Bergen\PhD_RangeX\R codes\RangeX_data_paper_cleaning\Data\Leaf_area_scans\RangeX_leaf_scans_all
## Code used:    Calculate leaf area with ImageJ function 
##               in Leaf_area_calculation_function.R
## Leaf area in: Data/RangeX_raw_functional_traits_leaf_area_all.csv
## Date:         11.11.24
## Author:       Nadine Arzt
## Purpose:      Calculate leaf area of focal species


# Adjusting min area to be counted as leaf not debris ---------------------
# smallest leaf area: 0.146
# 0.010 is the debris that is on many scans
# adjusted: low.size = 0.020 should not count that anymore
# run again when the rest is colored

# 70 leaves were colored with paint3D to get the whole leaf area
# sucpra, cennig and cyncri often have whitish parts
# and 13 hypmac leaves were recolored to get area without the two small leaves


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
  area <- try(run.ij(path.imagej = "C:/Users/naart3294/OneDrive - University of Bergen/Desktop/ImageJ/", set.directory = new.folder, distance.pixel = 237, known.distance = 2, log = TRUE, low.size = 0.020, trim.pixel = 60, trim.pixel.right = 150, save.image = TRUE))
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
output.folder <- "C:/Users/naart3294/OneDrive - University of Bergen/PhD_RangeX/R codes/RangeX_data_paper_cleaning/Data/Output/Output_RangeX_leaf_scans_all_without_debris//"

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

area <- read.csv("Data/Data_functional_traits/RangeX_raw_functional_traits_leaf_area_all.csv")


# check for duplicates ----------------------------------------------------
duplicates <- leaf_area[duplicated(leaf_area$ID), ]
duplicates
# 0 duplicates



# using Richards shiny app to get leaf area for bad scans -----------------
# gives different result then ImageJ
# Minimum area mm = 2
# Threshold Otsu = 0,59 
# FMQ6120: n = 2, la1 = 747.73, la2 = 2686.32 --> leaf_area = 3434.05 mm2
# FPI1781: n = 1, leaf_area = 2393.56 mm2
# FSK5791: n = 8, leaf_area = 734.54 mm2
# GDR8049: n = 4, leaf_area = 660.11 mm2


# recolored scans again ------------------------------------
list.of.files <- dir(path = paste0("Data/Leaf_area_scans/RangeX_leaf_scans_recolored_paint_paint3D/"), pattern = "jpeg|jpg", recursive = TRUE, full.names = TRUE)

# create a different output folder for every folder of scans
output.folder <- "C:/Users/naart3294/OneDrive - University of Bergen/PhD_RangeX/R codes/RangeX_data_paper_cleaning/Data/Output/Output_RangeX_leaf_scans_recolored/"

# this does the magic of calculating the leaf area for all scans in the specified folder
LA_2 <- plyr::ldply(list.of.files, loop.files)
LA_2

# calculate leaf area sums per leaf
leaf_area_recolored <- LA_2 |>
  # extract everything before point
  mutate(ID = sub("\\..*", "", ID)) |>
  group_by(dir, ID) |>
  summarise(n = n(),
            leaf_area = sum(LeafArea))
# save data as csv
# write.csv(leaf_area_recolored, file = "Data/Data_functional_traits/RangeX_raw_functional_traits_leaf_area_recolored.csv")

LA_recolored <- read.csv("Data/Data_functional_traits/RangeX_raw_functional_traits_leaf_area_recolored.csv")


# hypmac recolored again ------------------------------------
list.of.files <- dir(path = paste0("Data/Leaf_area_scans/RangeX_leaf_scans_hypmac_recolored/"), pattern = "jpeg|jpg", recursive = TRUE, full.names = TRUE)

# create a different output folder for every folder of scans
output.folder <- "C:/Users/naart3294/OneDrive - University of Bergen/PhD_RangeX/R codes/RangeX_data_paper_cleaning/Data/Output/Output_RangeX_leaf_scans_hypmac_recolored/"

# this does the magic of calculating the leaf area for all scans in the specified folder
LA_3 <- plyr::ldply(list.of.files, loop.files)
LA_3

# calculate leaf area sums per leaf
leaf_area_hypmac_recolored <- LA_3 |>
  # extract everything before point
  mutate(ID = sub("\\..*", "", ID)) |>
  group_by(dir, ID) |>
  summarise(n = n(),
            leaf_area = sum(LeafArea))
# save data as csv
# write.csv(leaf_area_hypmac_recolored, file = "Data/Data_functional_traits/RangeX_raw_functional_traits_leaf_area_hypmac_recolored.csv")

LA_hypmac_recolored <- read.csv("Data/Data_functional_traits/RangeX_raw_functional_traits_leaf_area_hypmac_recolored.csv")



# get final data set -------------------------------------------------------
leaf_area_final <- leaf_area |> 
  left_join(leaf_area_recolored, by = "ID", 
            suffix = c("", "_recolored")) |> 
  left_join(leaf_area_hypmac_recolored, by = "ID",
            suffix = c("", "_hypmac_recolored")) |> 
  mutate(leaf_area = coalesce(leaf_area_recolored,
                              leaf_area_hypmac_recolored, leaf_area)) |> 
  select(dir, ID, n, leaf_area)


# save as csv
# write.csv(leaf_area_final, file = "Data/Data_functional_traits/RangeX_raw_functional_traits_leaf_area_all_final.csv")

leaf_area_NOR <- read.csv("Data/Data_functional_traits/RangeX_raw_functional_traits_leaf_area_all_final.csv")



























