

# Check species names using TNRS -----------------------------------------------------
# Date:    27.01.2025
# Author:  Brad Boyle (bboyle@arizona.edu)
# Purpose: Check correct spelling of all species names
# See also "tnrs_api_example2.R" for a more function-based approach.


#################################
# Parameters & libraries
#################################

##################
# Base URL 
##################

url = "https://tnrsapi.xyz/tnrs_api.php"	

##################
# Libraries
##################

library(httr)		# API requests
library(jsonlite) # JSON coding/decoding
library(tidyverse)
library(openxlsx)

##################
# Input data (taxonomic names) 
##################

# Use external file or data frame (created below)? 
# Options: "file"|"df"
names_src<-"file"
names_src<-"df"

#
names_file <- 
  "http://bien.nceas.ucsb.edu/bien/wp-content/uploads/2019/07/tnrs_testfile.csv"


##################
# Misc parameters
##################

# Header for api call
headers <- list('Accept' = 'application/json', 'Content-Type' = 'application/json', 'charset' = 'UTF-8')

# API variables to clear before each API call
# Avoids spillover between calls
api_vars <- c("mode", "sources", "class", "matches", "acc", 
              "opts", "opts_json", "input_json")

# Response variables to clear
# Avoids spillover of previous results if API call fails
response_vars <- c("results_json", "results_raw", "results")

#################################
# Import the raw data
#################################

species_to_check <- read.csv("Data/Data_community/Species_names_to_check.csv")

species_to_check <- species_to_check |> 
  select(ID, species_submitted)

# Convert the data to JSON
data_json <- jsonlite::toJSON(unname(species_to_check))

#################################
# Example 1: Resolve mode, best match only
#################################

# Clear existing variables
suppressWarnings( rm( list = Filter( exists, c(response_vars, api_vars ) ) ) )

# Set the TNRS options
sources <- "wcvp,wfo"	# Taxonomic sources
class <- "wfo"			# Family classification. Only current option: "wfo"
mode <- "resolve"			# Processing mode
matches <- "best"			# Return best match only

# Convert the options to data frame and then JSON
opts <- data.frame(c(sources),c(class), c(mode), c(matches))
names(opts) <- c("sources", "class", "mode", "matches")
opts_json <-  jsonlite::toJSON(opts)
opts_json <- gsub('\\[','',opts_json)
opts_json <- gsub('\\]','',opts_json)

# Combine the options and data into single JSON object
input_json <- paste0('{"opts":', opts_json, ',"data":', data_json, '}' )

# Send the API request
results_json <- POST(url = url,
                     add_headers('Content-Type' = 'application/json'),
                     add_headers('Accept' = 'application/json'),
                     add_headers('charset' = 'UTF-8'),
                     body = input_json,
                     encode = "json")

# Convert JSON results to a data frame
results_raw <- fromJSON(rawToChar(results_json$content)) 
results <- as.data.frame(results_raw)

# Inspect the results
head(results, 10)

# Display header plus one row vertically
# to better compare the output fields
results.t <- as.data.frame( t( results[,1:ncol(results)] ) )
results.t[,3,drop =FALSE]

# Display just the main results fields
max.rows <- as.integer( min(nrow(results), 10) )
results $match.score <- format(round(as.numeric(results $Overall_score),2), nsmall=2)
results[ 1:max.rows, c('Name_submitted', 'match.score', 'Name_matched', 'Taxonomic_status', 
                       'Accepted_name', 'Unmatched_terms')
]



# save results of check ---------------------------------------------------

# write.xlsx(results, "Data/Data_community/Species_names_checked_results.xlsx")

species_names_results <- read.xlsx("Data/Data_community/Species_names_checked_results.xlsx")

# filter the unmatched names ----------------------------------------------
# 1 means the species name is correct
species_to_correct <- species_names_results |> 
  filter(Overall_score != 1)
# 124 occurrences need to be fixed
# these species don't fit with the correct names


# save species to correct -------------------------------------------------
# write.xlsx(species_to_correct, "Data/Data_community/Species_names_to_correct.xlsx")




