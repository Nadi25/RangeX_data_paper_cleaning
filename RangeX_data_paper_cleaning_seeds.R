
# Seeds and seed germination NOR --------------------------------------------

## Data used: RangeX_raw_seed_weight_odd_NOR_2023.csv,
##            RangeX_raw_seed_weight_even_NOR_2023.xlsx
## Date:      05.02.2025
## Author:    Nadine Arzt
## Purpose:   Clean seed data (seedweight and seed number)


# comments ----------------------------------------------------------------
# weight_even: flowers: how many infructescences = flowers collected separately, e.g. 3
# weight_even: number_infructescence: how many infructescences per filter, e.g. 2
# problem: weight_odd: and odd doesn't have number_infructescence

# weight_odd: date in odd: not transferred correct -> fixed
# weight_odd: not sure what why we have 2 seeds_number_4
# weight_odd: FYG5543: e6 (and c4?)	3 of e6, 2 of other?
# position_ID_old: means that it is a different plant than the one we have functional traits on

# seed_weight: calculated the mean of all collected infructescences per plant to get seed_weight
# no_seeds: calculated the mean of all collected infructescences per plant to get no_seeds

# flowers: seed_weight_odd: sometimes flowers = 4 
# because there were more>1 in the same filter
# 41 cases
# should we add a column indicating these cases?

# date still has some issues but might be ok since we are using date_collected

# couter: IE = Ingrid Espeland, TD = Timothée Darbois

# check: 
# NOR.hi.ambi.vege.wf.09.27 pimsax: high value for weight: 0.69400000
# NOR.hi.warm.bare.wf.03.11 pimsax: no_seeds: 239.00000
# NOR.hi.warm.bare.wf.05.15 leuvul: no_seeds: 237.50000



# for usage notes: silene should be taken out because there were not many seeds

# load library ------------------------------------------------------------
library(conflicted)
conflict_prefer_all("dplyr", quiet = TRUE)
library(tidyverse)
library(openxlsx)
library(ggplot2)

# import data -------------------------------------------------------------
# seed_weight_odd <- read.xlsx("Data/Data_seeds/RangeX_raw_seed_weight_odd_NOR_2023.xlsx", sheet = 1)
seed_weight_odd <- read.csv2("Data/Data_seeds/RangeX_raw_seed_weight_odd_NOR_2023.csv")

seed_weight_even <- read.xlsx("Data/Data_seeds/RangeX_raw_seed_weight_even_NOR_2023.xlsx", sheet = 1)

metadata <- read.csv("Data/Metadata/RangeX_clean_MetadataFocal_NOR.csv")


# merge odd and even data sets ------------------------------------------
names(seed_weight_odd)
names(seed_weight_even)


# rename column names in weight even to match odd --------------------------------------------

# Rename columns automatically to make them unique
colnames(seed_weight_odd) <- make.names(colnames(seed_weight_odd), unique = TRUE)

seed_weight_odd <- seed_weight_odd |> 
  rename("flowers" = "X.flowers",
         "seeds_number_1" = "X.seeds",
         "weight_1" = "weight.in.gram",
         "date_agar_1" = "date.in.agar.fridge",
         "collected_1" = "collected.from.field",
         "comment_1" = "comment",
         "seeds_number_2" = "X.seeds.1",
         "weight_2" = "weight",
         "date_agar_2" = "date.in.agar.fridge.1",
         "collected_2" = "collected",
         "comment_2" = "comment.1",
         "seeds_number_3" = "X.seeds.2",
         "weight_3" = "weight.1",
         "date_agar_3" = "date.in.agar.fridge.2",
         "collected_3" = "collected.1",
         "comment_3" = "comment.2",
         "seeds_number_4" = "seeds",
         "weight_4" = "weight.2",
         "collected_4" = "collected.2")

colnames(seed_weight_odd)
# not sure what why we have 2 seeds_number_4

# delete "seeds_number_4", "weight_4", "collected_4"? not actually empty!    
seed_weight_odd <- seed_weight_odd |> 
  select(where(~ any(!is.na(.))))

# delete empty rows
seed_weight_odd <- seed_weight_odd |> 
  filter(if_any(everything(), ~ !is.na(.) & . != ""))
# 303 rows


# clean position ID in odd ------------------------------------------------

seed_weight_odd$positionID

seed_weight_odd <- seed_weight_odd %>%
  mutate(
    positionID_old = str_extract(positionID, "\\(.*\\)"),  # Extract text in brackets
    positionID = str_trim(str_remove(positionID, "\\s*\\(.*\\)"))  # Remove brackets and clean spaces
  )


# fix date columns ----------------------------------------------------------------
# not the same format
seed_weight_odd$date_agar_1

# Define a function to clean and standardize dates
clean_dates <- function(date_column) {
  case_when(
    str_detect(date_column, "^\\d{1,2}\\.\\d{1,2}\\.\\d{2,4}$") ~ dmy(date_column),  # Full format (DD.MM.YYYY)
    str_detect(date_column, "^\\d{1,2}\\.\\d{1,2}$") ~ {  
      # Extract day and month
      parts <- str_split_fixed(date_column, "\\.", 2)
      day <- as.numeric(parts[, 1])
      month <- as.numeric(parts[, 2])
      
      # Assign correct year based on the month
      date_final <- make_date(
        year = ifelse(month >= 12, 2023, 2024),  # December and earlier → 2023, otherwise → 2024
        month = month,
        day = day
      )
      
      date_final
    },
    date_column == "" ~ NA_Date_,  # Convert empty strings to NA
    TRUE ~ as.Date(NA)  # Handle unexpected cases
  )
}

# apply function to all date columns
seed_weight_odd <- seed_weight_odd %>%
  mutate(across(c(date_agar_1, date_agar_2, date_agar_3, 
                  collected_1, collected_2, collected_3, collected_4), clean_dates))

colnames(seed_weight_odd)


# fix date_collected odd --------------------------------------------------
# some dates collected are 2024 
# because it was written only e.g. 24.11
# r changed it to 2024 but it must be 2023
seed_weight_odd <- seed_weight_odd |>
  mutate(across(
    starts_with("collected"),
    ~ if_else(
      lubridate::year(.x) == 2024,
      lubridate::make_date(
        year = 2023,
        month = lubridate::month(.x),
        day = lubridate::day(.x)
      ),
      .x
    )
  ))




# add number_infructescences columns --------------------------------------
# odd doesn't have "number_infructescence" which is about how many seedheads were collected per filter
# but it is written in the comments
seed_weight_odd <- seed_weight_odd |> 
  mutate(number_infructescence_1 = NA,
         number_infructescence_2 = NA,
         number_infructescence_3 = NA)


# clean names weight_even -------------------------------------------------
colnames(seed_weight_even)

seed_weight_even <- seed_weight_even |> 
  rename("positionID_old" = "positionID",
         "positionID" = "positionID_clean")

# delete calculation columns
seed_weight_even <- seed_weight_even |> 
  select(-c("weight.seed", "seeds_number.inf", "weight.inf"))


# merge seed weight with metadata -----------------------------------------

# find out which column names are not matching
colnames(seed_weight_odd)
colnames(seed_weight_even)

# keep only necessary columns for now -------------------------------------
rx_seed_weight_odd <- seed_weight_odd |> 
  select("ID", "date", "site", "species", "treat1",
         "treat2", "blockID", "plotID", "positionID", "flowers",
         "seeds_number_1", "weight_1", "number_infructescence_1", "date_agar_1", "collected_1", "comment_1",
         "seeds_number_2", "weight_2", "number_infructescence_2", "date_agar_2", "collected_2", "comment_2", "seeds_number_3",
         "weight_3", "number_infructescence_3", "date_agar_3", "collected_3", "comment_3", "seeds_number_4",
         "weight_4", "collected_4")

rx_seed_weight_even <- seed_weight_even |> 
  select("ID", "date", "site", "species", "treat1",
         "treat2", "blockID", "plotID", "positionID", "flowers",
         "seeds_number_1", "weight_1", "number_infructescence_1", "date_agar_1", "collected_1", "comment_1",
         "seeds_number_2", "weight_2", "number_infructescence_2", "date_agar_2", "collected_2", "comment_2", "seeds._number_3",
         "weight_3", "number_infructescence_3", "date_agar_3", "collected_3", "comment_3", "seeds_number_4",
         "weight_4", "collected_4")

rx_seed_weight_even <- rx_seed_weight_even |> 
  rename(seeds_number_3 = seeds._number_3)



# fix date column in even -------------------------------------------------
# date has a weird format from excel
# change that by making numeric than specify origin that windows excel uses
rx_seed_weight_even <- rx_seed_weight_even |>
  mutate(across(
    matches("^collected|^date"),
    ~ case_when(
      is.numeric(.x) ~ as.Date(.x, origin = "1899-12-30"),
      TRUE           ~ as.Date(.x, tryFormats = c("%d.%m.%Y", "%Y-%m-%d"))
    )
  ))

# the last 2024 in collected columns are typos
# change them manually
rx_seed_weight_even <- rx_seed_weight_even |>
  mutate(across(
    starts_with("collected"),
    ~ if_else(
      lubridate::year(.x) == 2024,
      lubridate::make_date(
        year = 2023,
        month = lubridate::month(.x),
        day = lubridate::day(.x)
      ),
      .x
    )
  ))






# combine even and odd into one data set --------------------------------------------
seed_data <- rbind(rx_seed_weight_odd, rx_seed_weight_even)


# rename columns and match with metadata ----------------------------------------------------------
seed_data <- seed_data |> 
  rename("block_ID_original" = "blockID",
         "plot_ID_original" = "plotID",
         "position_ID_original" = "positionID",
         "treat_warming" = "treat1",
         "treat_competition" = "treat2")


# to match species
seed_data <- seed_data |> 
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
seed_data <- seed_data |> 
  mutate(site = case_when(
    site == "LS" ~ "lo",
    site == "HS" ~ "hi"
  ))

# to match plot_ID_original
seed_data <- seed_data |> 
  mutate(plot_ID_original = case_when(
    plot_ID_original == "A" ~ "a",
    plot_ID_original == "B" ~ "b",
    plot_ID_original == "C" ~ "c",
    plot_ID_original == "D" ~ "d",
    plot_ID_original == "E" ~ "e",
    plot_ID_original == "F" ~ "f"
  ))

# to match treat_warming
seed_data <- seed_data |> 
  mutate(treat_warming = case_when(
    site == "lo" & treat_warming == "" ~ "ambi",  # For site "lo" and empty cells
    treat_warming == "warm" ~ "warm",  
    treat_warming == "ambient" ~ "ambi" 
  ))



# merge with meta data ----------------------------------------------------
# need to be same, e.g. chr, to merge

metadata <- metadata |> 
  mutate(across(c(block_ID_original, plot_ID_original, position_ID_original), as.character))

seed_data <- seed_data |> 
  mutate(across(c(block_ID_original, plot_ID_original, position_ID_original), as.character))


rx_seed_raw <- left_join(metadata, seed_data, 
                         by = c( "site", "block_ID_original",
                                 "plot_ID_original", "position_ID_original", 
                                 "species", "treat_warming", "treat_competition"))


nrow(metadata) # 1800
nrow(rx_seed_raw) # 1801
setdiff(rx_seed_raw$unique_plant_ID, metadata$unique_plant_ID)
setdiff(metadata$unique_plant_ID, rx_seed_raw$unique_plant_ID)


# check for duplicates ----------------------------------------------------
rx_seed_raw |> 
  count(unique_plant_ID) |> 
  filter(n > 1)

# unique_plant_ID n
# NOR.hi.warm.bare.wf.07.10 2

# delete row with ID = FVG1548 but keep rows with NAs
rx_seed_raw <- rx_seed_raw |> 
  filter(ID != "FVG1548" | is.na(ID)) 



# add counter -------------------------------------------------------------
rx_seed_raw <- rx_seed_raw |> 
  mutate(counter = ifelse(block_ID_original %in% c(1, 3, 5, 7, 9),
                               "IE", "TD"))


# fix flower column -------------------------------------------------------
# weight and seed number should be numeric
rx_seed_raw <- rx_seed_raw |> 
  mutate(across(c(weight_1, weight_2, weight_3, weight_4, flowers), as.numeric))

rx_seed_raw <- rx_seed_raw |> 
  mutate(across(c(seeds_number_1, seeds_number_2, seeds_number_3),
                as.numeric))

rx_seed_raw <- rx_seed_raw |> 
  mutate(flowers_fixed = rowSums(!is.na(across(c(weight_1, weight_2, weight_3, weight_4)))))

# rx_seed_raw <- rx_seed_raw |> 
#   mutate(flowers_fixed = if_else(block_ID_original %in% c(2,4,6,8,10),  rowSums(!is.na(across(c(number_infructescence_1, number_infructescence_2, number_infructescence_3))))))

rx_seed_raw <- rx_seed_raw |>
  mutate(flowers_fixed = if_else(
    block_ID_original %in% c(2, 4, 6, 8, 10),
    rowSums(!is.na(across(c(number_infructescence_1, number_infructescence_2, number_infructescence_3)))),
    flowers  # what to keep when condition is FALSE
  ))


rx_seed_raw <- rx_seed_raw |> 
  mutate(flowers_fixed = if_else(block_ID_original %in% c(2,4,6,8,10), 
                                 rowSums(across(c(number_infructescence_1, number_infructescence_2, number_infructescence_3)), na.rm = TRUE), 
                                 flowers))



# calculate weight for one seed -------------------------------------------
# many NAs in the weight columns
rx_seed_raw <- rx_seed_raw |> 
  mutate(seedweight = rowSums(across(c(weight_1, weight_2, weight_3, weight_4)), na.rm = TRUE) / flowers_fixed)


# calculate no_seeds as mean per plant ------------------------------------------------------
rx_seed_raw <- rx_seed_raw |> 
  mutate(no_seeds = rowSums(across(c(seeds_number_1, seeds_number_2, seeds_number_3, seeds_number_4)), na.rm = TRUE) / flowers_fixed)


# new column indicating more than 1 seed head per filter -------------------
# yes, no
rx_seed_raw <- rx_seed_raw |> 
  mutate(several_infruct_filter = if_else(flowers_fixed %in% c(1,2,3), "no", "yes"))


# filter only necessary columns for OSF -----------------------------------
# unique_plant_ID,"species","date_collection","counter","inflorescence_size","no_seeds","seedweight"

# use coalesce to choose the first date available
rangex_seed_raw <- rx_seed_raw |> 
  select(unique_plant_ID, species, collected_1, collected_2,
         collected_3, collected_4, counter, 
         no_seeds, seedweight) |> 
  mutate(date_collection = coalesce(collected_1, collected_2,
                                    collected_3, collected_4)) |> 
  select(-collected_1, -collected_2, -collected_3, -collected_4)

# add column inflorescence_size
rangex_seed_raw <- rangex_seed_raw |> 
  mutate(inflorescence_size = NA)


# delete empty rows -------------------------------------------------------
# and no_seeds and seedweight should not be 0
rangex_seed_clean <- rangex_seed_raw |> 
  filter((!is.na(no_seeds) & no_seeds != 0) | 
           (!is.na(seedweight) & seedweight != 0) | 
           !is.na(date_collection))
# 336 rows now

# fix order ---------------------------------------------------------------
rangex_seed_clean <- rangex_seed_clean |> 
  select(unique_plant_ID, species, date_collection, counter,
         inflorescence_size, no_seeds, seedweight)


# control plotting --------------------------------------------------------
# use rx_seed_raw
# pivot longer the table

# combined treatment column
rx_seed_raw <- rx_seed_raw |> 
  mutate(treat_comp_warm = paste(treat_warming, treat_competition, sep = "_"))

seed_plot <- rx_seed_raw |> 
  pivot_longer(cols = c(seedweight, no_seeds), names_to = "variable", values_to = "values")


# plotting all ------------------------------------------------------------
ggplot(data = seed_plot[seed_plot$site == "lo",], aes(x = variable, y = values)) +
  geom_boxplot() +
  facet_grid(species~treat_comp_warm, scales = "free")

ggplot(data = seed_plot, aes(x = variable, y = values, fill = site)) +
  geom_boxplot() +
  facet_grid(species~treat_comp_warm, scales = "free")


# seed weight only --------------------------------------------------------
seed_weight <- seed_plot |> 
  filter(variable == "seedweight") 

ggplot(data = seed_weight[seed_weight$site == "hi",], aes(x = variable, y = values)) +
  geom_jitter()

ggplot(data = seed_weight[seed_weight$site == "lo",], aes(x = variable, y = values)) +
  geom_jitter()

ggplot(data = seed_weight, aes(x = variable, y = values, fill = site)) +
  geom_boxplot() +
  facet_grid(species~treat_comp_warm, scales = "free")

ggplot(data = seed_weight[seed_weight$site == "lo",], aes(x = variable, y = values)) +
  geom_boxplot() +
  geom_jitter()+
  facet_grid(species~treat_comp_warm, scales = "free")

ggplot(data = seed_weight[seed_weight$site == "hi",], aes(x = variable, y = values)) +
  geom_boxplot() +
  geom_jitter()+
  facet_grid(species~treat_comp_warm, scales = "free")


ggplot(data = seed_weight[seed_weight$site == "hi",], aes(x = variable, y = values)) +
  geom_jitter()+
  facet_grid(species~treat_comp_warm, scales = "free")

ggplot(data = seed_weight[seed_weight$site == "hi",], aes(x = values, fill = site)) +
  geom_histogram(binwidth = 0.01, color = "black", alpha = 0.7) +  
  facet_grid(species ~ treat_comp_warm) +
  labs(x = "Seed Weight", y = "Frequency") +
  theme_minimal()

ggplot(data = seed_weight[seed_weight$site == "lo",], aes(x = values, fill = site)) +
  geom_histogram(binwidth = 0.01, color = "black", alpha = 0.7) +  
  facet_grid(species ~ treat_comp_warm) +
  labs(x = "Seed Weight", y = "Frequency") +
  theme_minimal()



# number of seeds only ----------------------------------------------------
seed_number <- seed_plot |> 
  filter(variable == "no_seeds") 

ggplot(data = seed_number[seed_number$site == "hi",], aes(x = variable, y = values)) +
  geom_jitter()

ggplot(data = seed_number[seed_number$site == "hi",], aes(x = values)) +
  geom_histogram()

ggplot(data = seed_number[seed_number$site == "lo",], aes(x = variable, y = values)) +
  geom_jitter()

ggplot(data = seed_number[seed_number$site == "lo",], aes(x = values)) +
  geom_histogram()



# save clean data set -----------------------------------------------------
write.csv(rangex_seed_clean, "Data/Data_seeds/CleanSeedTraits/RangeX_clean_seeds_2023_NOR.csv", row.names = FALSE)

seed <- read_csv("Data/Data_seeds/CleanSeedTraits/RangeX_clean_seeds_2023_NOR.csv")

