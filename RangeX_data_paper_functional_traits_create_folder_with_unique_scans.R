
# Create one folder with leaf area images ---------------------------------

## Data used: RangeX_leaf_scans_DarthVader, RangeX_leaf_scans_high_28.07.23.
##            RangeX_leaf_scans_low_06.07.23, RangeX_leaf_scans_low_07.07.23
## Output in: RangeX_leaf_scans_all
## Date:      29.12.2024
## Author:    Nadine Arzt
## Purpose:   Have one folder with unique images instead of 4 with overlap


# load library ------------------------------------------------------------
library(fs)
library(purrr)

# create folder paths -----------------------------------------------------
folder_paths <- c("Data/Leaf_area_scans/RangeX_leaf_scans_DarthVader/",
                  "Data/Leaf_area_scans/RangeX_leaf_scans_high_28.07.23/",
                  "Data/Leaf_area_scans/RangeX_leaf_scans_low_06.07.23/",
                  "Data/Leaf_area_scans/RangeX_leaf_scans_low_07.07.23/")
output_folder <- "Data/Leaf_area_scans/RangeX_leaf_scans_all/"

# # Create the output folder if it doesn't exist
# if (!dir_exists(output_folder)) {
#   dir_create(output_folder)
# }


# get a list of all jpeg images in the folders ----------------------------
image_files <- folder_paths |> 
  lapply(list.files, pattern = "\\.jpe?g$", full.names = TRUE) |> 
  unlist()
# 910


# extract just the file names ---------------------------------------------
# without the paths to avoid overlap
image_filenames <- basename(image_files)


# get unique file names ---------------------------------------------------
unique_filenames <- unique(image_filenames) # 600


# map unique file names back to their full paths --------------------------
unique_images <- unique_filenames |> 
  sapply(function(file) {
    # Find the first occurrence of the file in the original list of paths
    image_files[which(image_filenames == file)[1]]
  })


# copy unique images to the output folder using walk() --------------------
unique_images |> 
  walk(~ {
    file_name <- basename(.x)  # Extract the filename
    destination_path <- file.path(output_folder, file_name)
    
    # Copy only if the file doesn't already exist in the destination
    if (!file_exists(destination_path)) {
      file_copy(.x, destination_path)
    }
  })


# print the total number of unique images ---------------------------------
cat("Total unique images:", length(unique_images), "\n")
cat("Images successfully copied to:", output_folder, "\n")

# IHW4848 is just a test scan of an envelope -> deleted in Darth Vader folder


# would go faster then walk
# Copy unique images to the output folder
# for (image_path in unique_images) {
#   file_name <- basename(image_path)  # Extract the filename
#   destination_path <- file.path(output_folder, file_name)
#   
#   # Copy only if the file doesn't already exist in the destination
#   if (!file_exists(destination_path)) {
#     file_copy(image_path, destination_path)
#   }
# }














