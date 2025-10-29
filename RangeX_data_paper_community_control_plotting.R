

# library -----------------------------------------------------------------
library(conflicted)
conflict_prefer_all("dplyr", quiet = TRUE)
library(tidyverse)
library(readr)

library(lubridate)

theme_set(theme_bw())


# import community data ---------------------------------------------------
veg_survey_general <- read_csv("Data/Data_community/RangeX_clean_VegSurveyGeneral_21_22_23_NOR.csv")

veg_survey_general <- veg_survey_general |> 
  select(unique_plot_ID, date_measurement, species, cover, collector, only_focals)


veg_survey_nor <- read_csv("Data/Data_community/RangeX_clean_VegSurveyNOR_21_22_23_NOR.csv")

veg_survey_nor <- veg_survey_nor |> 
  select(unique_plot_ID, date_measurement, species, subplot, cover, collector, reproductive_capacity)


# import general metadata file -------------------------------------------------
metadata_plot <- read.csv("Data/RangeX_metadata_plot_NOR.csv", header = TRUE)

metadata_plot <- metadata_plot |> 
  select(region, site, block_ID_original, plot_ID_original, treat_warming, 
         treat_competition, added_focals, block_ID, unique_plot_ID)

# we don't need bare plots here
metadata_NOR_com <- metadata_plot |> 
  filter(treat_competition != "bare")
# 50 plots now

metadata_NOR_com <- metadata_NOR_com |> 
  mutate(block_ID_original = as.character(block_ID_original))


# merge metadata with community data --------------------------------------
community_data_clean_plot <- left_join(veg_survey_general, metadata_NOR_com,
                                    by = c("unique_plot_ID"))


community_data_clean_subplots <- left_join(veg_survey_nor, metadata_NOR_com,
                                       by = c("unique_plot_ID"))

# filter high site --------------------------------------------------------
community_data_clean_1m2_high <- community_data_clean_plot |> 
  filter(site == "hi")

community_data_clean_16_subplots_high <- community_data_clean_subplots |> 
  filter(site == "hi")


# add column year ---------------------------------------------------------
community_data_clean_1m2_high <- community_data_clean_1m2_high |> 
  mutate(year = year(ymd(date_measurement)))

community_data_clean_16_subplots_high <- community_data_clean_16_subplots_high |> 
  mutate(year = year(ymd(date_measurement)))


# Plot: Number of unique species per warming treatment per year -----------
community_data_clean_1m2_high |> 
  group_by(year, treat_warming, species) |> 
  summarise() |> 
  group_by(year, treat_warming) |> 
  summarise(n_species = n()) |> 
  ggplot(aes(x = factor(year), y = n_species, fill = treat_warming)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Number of species warmed vs not warmed",
       x = "Year", y = "Number of Species") +
  theme_bw()

# Plot: Number of unique species per added focals treatment per year
community_data_clean_1m2_high |> 
  group_by(year, added_focals, species) |> 
  summarise() |> 
  group_by(year, added_focals) |> 
  summarise(n_species = n()) |> 
  ggplot(aes(x = factor(year), y = n_species, fill = added_focals)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Number of Unique Species per Competition Treatment by Year",
       x = "Year", y = "Number of Species") +
  theme_minimal()


community_data_clean_1m2_high |> 
  group_by(year, plot_ID_original, species) |> 
  summarise() |> 
  group_by(year, plot_ID_original) |> 
  summarise(n_species = n()) |> 
  ggplot(aes(x = factor(year), y = n_species, fill = plot_ID_original)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "",
       x = "Year", y = "Number of Species") +
  theme_bw()



# column with treatment ---------------------------------------------------
community_data_clean_1m2_high <- community_data_clean_1m2_high |> 
  mutate(treatment_combined = paste(treat_warming, treat_competition, added_focals, sep = "_"))

community_data_clean_1m2_high |> 
  group_by(year, treatment_combined, species) |> 
  summarise() |> 
  group_by(year, treatment_combined) |> 
  summarise(n_species = n()) |> 
  ggplot(aes(x = factor(year), y = n_species, fill = treatment_combined)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "",
       x = "Year", y = "Number of Species")


community_data_clean_1m2_high |>
  group_by(unique_plot_ID, year, treatment_combined) |>  # Assuming 'plot_id' identifies each sampling unit
  summarise(n_species = n_distinct(species), .groups = "drop") |> 
  ggplot(aes(x = factor(year), y = n_species, fill = treatment_combined)) +
  geom_boxplot() +
  labs(title = "Species Richness per Plot",
       x = "Year", y = "Number of Species")





example_plot <- "NOR.hi.ambi.vege.nf.01" 
community_data_clean_16_subplots_high |> 
  filter(unique_plot_ID == example_plot) |> 
  ggplot(aes(x = factor(subplot), y = fct_rev(factor(species)), fill = cover)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(option = "C") +
  facet_wrap(~ year) +
  labs(
    x = "Subturf", y = "Species", fill = "Cover",
    title = glue::glue("Heatmap: {example_plot}")
  ) +
  theme_minimal(base_size = 10) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  )



community_data_clean_1m2_high <- community_data_clean_1m2_high |> 
  mutate(
    cover = str_replace(cover, "<1", "1"),        # replace "<1" with "1"
    cover = as.numeric(cover)                     # convert to numeric
  )

comm_mat <- community_data_clean_1m2_high |>
  group_by(unique_plot_ID, species) |>
  summarise(cover = sum(cover, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = species, values_from = cover, values_fill = 0) |>
  column_to_rownames("unique_plot_ID")

library(vegan)
nmds <- metaMDS(comm_mat, distance = "bray", k = 2, trymax = 100)

# Extract NMDS scores
site_scores <- scores(nmds, display = "sites")
scores_df <- as_tibble(site_scores) |>
  mutate(unique_plot_ID = rownames(site_scores))
# Join metadata
metadata <- community_data_clean_1m2_high |>
  distinct(unique_plot_ID, treatment_combined, collector, site, year)

nmds_plot_data <- left_join(scores_df, metadata, by = "unique_plot_ID")

ggplot(nmds_plot_data, aes(x = NMDS1, y = NMDS2,
                           color = treatment_combined,
                           shape = collector)) +
  geom_point(size = 3, alpha = 0.8) +
  facet_wrap(~ site) +
  theme_minimal() +
  labs(title = "NMDS of vegetation cover (Bray-Curtis)",
       subtitle = "Grouped by treatment and collector")





































