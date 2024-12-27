
# RangeX functional traits data exploration ----------------------------------

## Data used: RangeX_raw_functional_traits_2023.csv,
## RangeX_raw_functional_traits_leaf_area_NOR_2023.csv and RangeX_Metadata.csv 
## Date: 26.12.2024
## Author: Nadine Arzt
## Purpose: Control plotting of data


# Check these! ------------------------------------------------------------
# GMJ6176: cyncri: drymass: 77.60 but wet mass: 77.60
# FVX6947: cyncri: leaf_thickness: 0.09933333
# FMX1959: plalan: wetmass and drymass
# FNW0998: luzmul: wetmass: 326.50 but dry mass: 7.88
# FNN3644: luzmul: wetmass: 310.50 but dry mass: 7.99
# FOI4279: luzmul: wetmass: 205.10 but dry mass: 5.10
# FVN2828: tripra: LDMC: 20.89034
# FVN2828: tripra: low dry mass: 10.23 but high wet mass: 489.70
# GGL5838: sildio: high wet mass: 953.00 but normal dry mass: 15.21


# load library ------------------------------------------------------------
library(purrr)

# source clean functional trait data file from cleaning R script ---------------------------------
source("RangeX_data_paper_cleaning_functional_traits.R")


# control plotting --------------------------------------------------------

# create treat column -----------------------------------------------------

functional_traits_NOR_23 <- functional_traits_NOR_23 |> 
  mutate(treat = paste(site, treat_warming, treat_competition, sep = "_"))


# prepare plotting data set -----------------------------------------------

traits_plotting <- functional_traits_NOR_23 |> 
  select(unique_plant_ID, ID, site, treat, species, date, wet_mass, wet_mass_g, dry_mass, leaf_thickness, leaf_area, SLA, LDMC, sto_density_top, sto_density_bot)

traits_plotting_long <- traits_plotting |> 
  pivot_longer(cols = c(wet_mass, dry_mass, leaf_thickness, leaf_area,
                        SLA, LDMC, sto_density_top, sto_density_bot),
               names_to = "trait_variable", values_to = "values")



# plotting overview ----------------------------------------------------------------
#  
ggplot(data = traits_plotting_long[traits_plotting_long$site == "lo",], aes(x = trait_variable, y = values)) +
  geom_boxplot() +
  facet_grid(species~treat, scales = "free")

ggplot(data = traits_plotting_long[traits_plotting_long$site == "hi",], aes(x = trait_variable, y = values, fill = treat)) +
  geom_boxplot() +
  geom_jitter()+
  facet_wrap(~species, scales = "free") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(data = traits_plotting_long[traits_plotting_long$site == "hi",], aes(x = trait_variable, y = values, colour = treat)) +
  geom_point() +
  facet_wrap(~species, scales = "free") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(data = traits_plotting_long[traits_plotting_long$site == "hi",], aes(x = species, y = values, colour = treat)) +
  geom_point() +
  facet_wrap(~trait_variable, scales = "free") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


# plotting details --------------------------------------------------------

# wet mass (mg) vs dry mass (mg)
ggplot(traits_plotting, aes(x = dry_mass, y = wet_mass, colour = treat))+
  geom_point()

traits_plotting$dry_mass

# leaf thickness vs leaf area
ggplot(traits_plotting, aes(x = leaf_thickness, y = leaf_area, colour = treat))+
  geom_point()

# SLA vs LDMC
ggplot(traits_plotting, aes(x = SLA, y = LDMC))+
  geom_point()

# SLA
ggplot(traits_plotting, aes(x = species, y = SLA, fill = species)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 1) +
  theme_minimal()+
  geom_jitter()

# dry mass
ggplot(traits_plotting, aes(x = species, y = dry_mass, fill = species)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 1) +
  theme_minimal()+
  geom_jitter()

# wet mass
ggplot(traits_plotting, aes(x = species, y = wet_mass, fill = species)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 1) +
  theme_minimal()+
  geom_jitter()

# leaf thickness
ggplot(traits_plotting, aes(x = species, y = leaf_thickness, fill = site)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 1) +
  theme_minimal()+
  geom_jitter()

# leaf area
ggplot(traits_plotting[traits_plotting$site == "lo",], aes(x = species, y = leaf_area, fill = species)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 1) +
  theme_minimal()+
  geom_jitter()


# LDMC
ggplot(traits_plotting, aes(x = species, y = LDMC, fill = site)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 1) +
  theme_minimal()+
  geom_jitter()


# leaf area vs SLA
ggplot(traits_plotting, aes(x = leaf_area, y = SLA))+
  geom_point()



# leuvul ------------------------------
leuvul <- traits_plotting |> 
  filter(species == "leuvul")

# wet mass (mg) vs dry mass (mg)
ggplot(leuvul, aes(x = dry_mass, y = wet_mass, colour = treat))+
  geom_point()

# dry_mass
ggplot(leuvul, aes(x = species, y = dry_mass, fill = treat)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 1) +
  theme_minimal()+
  geom_jitter()

# wet_mass
ggplot(leuvul, aes(x = species, y = wet_mass, fill = treat)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 1) +
  theme_minimal()+
  geom_jitter()

# LDMC
ggplot(leuvul, aes(x = species, y = LDMC, fill = treat)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 1) +
  theme_minimal()+
  geom_jitter()

# SLA
ggplot(leuvul, aes(x = species, y = SLA, fill = treat)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 1) +
  theme_minimal()+
  geom_jitter()

# leaf area
ggplot(leuvul, aes(x = species, y = leaf_area, fill = treat)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 1) +
  theme_minimal()+
  geom_jitter()

ggplot(leuvul, aes(x = species, y = leaf_area, fill = treat)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 1) +
  theme_minimal()+
  geom_jitter()

# plalan ------------------------------
plalan <- traits_plotting |> 
  filter(species == "plalan")

# wet mass (mg) vs dry mass (mg)
ggplot(plalan, aes(x = dry_mass, y = wet_mass, colour = site))+
  geom_point()

# leaf area
ggplot(plalan, aes(x = species, y = leaf_area, fill = treat)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 1) +
  theme_minimal()+
  geom_jitter()

# dry_mass
ggplot(plalan, aes(x = species, y = dry_mass, fill = treat)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 1) +
  theme_minimal()+
  geom_jitter()

# wet_mass
ggplot(plalan, aes(x = species, y = wet_mass, fill = treat)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 1) +
  theme_minimal()+
  geom_jitter()

# LDMC
ggplot(plalan, aes(x = species, y = LDMC, fill = treat)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 1) +
  theme_minimal()+
  geom_jitter()

# SLA
ggplot(plalan, aes(x = species, y = SLA, fill = treat)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 1) +
  theme_minimal()+
  geom_jitter()

# leaf_thickness
ggplot(plalan, aes(x = species, y = leaf_thickness, fill = treat)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 1) +
  theme_minimal()+
  geom_jitter()



# sucpra outlier ------------------------------------------------------------------
# NOR.lo.ambi.vege.wf.01.21: dry mass = 468.18
# NOR.lo.ambi.vege.wf.02.09: dry mass = 421.72



# write function to get subset per species --------------------------------
# filter per species and only rows with values
filter_species <- function(traits_plotting, species_name, date) {
  print(paste("Filtering for species:", species_name))
  filtered_data <- traits_plotting |> 
    filter(species == species_name & !is.na(date))
  print(paste("Number of rows after filtering:", nrow(filtered_data)))
  return(filtered_data)
}
# sucpra <- filter_species(traits_plotting, "sucpra")
# leuvul <- filter_species(traits_plotting, "leuvul")


# lapply to loop through function to filter datasets per species ----------
# Get a list of unique species
unique_species <- unique(traits_plotting$species)

# Create a list to store the filtered datasets
filtered_species_datasets <- list()

# loop through all 10 species
filtered_species_datasets <- lapply(unique_species[1:10], function(species_name) {
  filter_species(traits_plotting, species_name)
})

# Name the list elements with the species names
names(filtered_species_datasets) <- unique_species[1:10]

# Print the names of the datasets created
print(names(filtered_species_datasets))


# Access a specific dataset by species name -------------------------------
sucpra <- filtered_species_datasets[["sucpra"]]
leuvul <- filtered_species_datasets[["leuvul"]]
cennig <- filtered_species_datasets[["cennig"]]
cyncri <- filtered_species_datasets[["cyncri"]]
pimsax <- filtered_species_datasets[["pimsax"]]
luzmul <- filtered_species_datasets[["luzmul"]]
plalan <- filtered_species_datasets[["plalan"]]
sildio <- filtered_species_datasets[["sildio"]]
tripra <- filtered_species_datasets[["tripra"]]
hypmac <- filtered_species_datasets[["hypmac"]]


# Use map to create a list of filtered datasets ---------------------------
# Get a list of unique species
unique_species <- unique(traits_plotting$species)

# map to loop through 10 species
filtered_datasets_species <- map(unique_species[1:10], ~ filter_species(traits_plotting, .x))

# Name the list elements with the species names
names(filtered_datasets_species) <- unique_species[1:10]

# Print the names of the datasets created
print(names(filtered_datasets_species))

# Access a specific dataset by species name -------------------------------
sucpra <- filtered_datasets_species[["sucpra"]]
leuvul <- filtered_datasets_species[["leuvul"]]
cennig <- filtered_datasets_species[["cennig"]]
cyncri <- filtered_datasets_species[["cyncri"]]
pimsax <- filtered_datasets_species[["pimsax"]]
luzmul <- filtered_datasets_species[["luzmul"]]
plalan <- filtered_datasets_species[["plalan"]]
sildio <- filtered_datasets_species[["sildio"]]
tripra <- filtered_datasets_species[["tripra"]]
hypmac <- filtered_datasets_species[["hypmac"]]



# function and loop for plots per species --------------------------------------------------------

filter_plot_species <- function(traits_plotting, species_name) {
  filtered_data <- traits_plotting |> 
    filter(species == species_name & !is.na(date))
  
  # Create plots
  plots <- list(
    leaf_area = ggplot(filtered_data, aes(x = treat, y = leaf_area, fill = treat)) +
      geom_boxplot(outlier.color = "red", outlier.shape = 1) +
      theme_bw() +
      geom_jitter(width = 0.2)+
      theme(legend.position = "none",
            axis.text.x = element_text(angle = 45, hjust = 1)),
    
    dry_mass = ggplot(filtered_data, aes(x = treat, y = dry_mass, fill = treat)) +
      geom_boxplot(outlier.color = "red", outlier.shape = 1) +
      theme_bw() +
      geom_jitter(width = 0.2)+
      theme(legend.position = "none",
            axis.text.x = element_text(angle = 45, hjust = 1)),
    
    wet_mass = ggplot(filtered_data, aes(x = treat, y = wet_mass, fill = treat)) +
      geom_boxplot(outlier.color = "red", outlier.shape = 1) +
      theme_bw() +
      geom_jitter(width = 0.2)+
      theme(legend.position = "none",
            axis.text.x = element_text(angle = 45, hjust = 1)),
    
    LDMC = ggplot(filtered_data, aes(x = treat, y = LDMC, fill = treat)) +
      geom_boxplot(outlier.color = "red", outlier.shape = 1) +
      theme_bw() +
      geom_jitter(width = 0.2)+
      theme(legend.position = "none",
            axis.text.x = element_text(angle = 45, hjust = 1)),
    
    SLA = ggplot(filtered_data, aes(x = treat, y = SLA, fill = treat)) +
      geom_boxplot(outlier.color = "red", outlier.shape = 1) +
      theme_bw() +
      geom_jitter(width = 0.2)+
      theme(legend.position = "none",
            axis.text.x = element_text(angle = 45, hjust = 1)),
    
    leaf_thickness = ggplot(filtered_data, aes(x = treat, y = leaf_thickness, 
                                                fill = treat)) +
      geom_boxplot(outlier.color = "red", outlier.shape = 1) +
      theme_bw() +
      geom_jitter(width = 0.2)+
      theme(legend.position = "none",
            axis.text.x = element_text(angle = 45, hjust = 1)),
    
    # wet mass (mg) vs dry mass (mg)
    wetmass_drymass = ggplot(filtered_data, aes(x = dry_mass, y = wet_mass, 
                                                colour = treat))+
      geom_point()+
      theme_bw()
  )
  
  return(list(data = filtered_data, plots = plots))
}


# Get a list of unique species
unique_species <- unique(traits_plotting$species)

# Use map to create a list of filtered datasets and plots
filtered_datasets <- map(unique_species[1:10], ~ filter_plot_species(traits_plotting, .x))

# Name the list elements with the species names
names(filtered_datasets) <- unique_species[1:10]

# Print the names of the datasets created
print(names(filtered_datasets))


# sucpra: access dataset and its plots by species name ---------------------
sucpra_data <- filtered_datasets[["sucpra"]]$data
sucpra_plots <- filtered_datasets[["sucpra"]]$plots

# Display leaf area plot
print(sucpra_plots$leaf_area)
# FPT2142: 6188.1 looks correct --> checked with shiny_leaf

# dry mass
print(sucpra_plots$dry_mass)

# wet mass
print(sucpra_plots$wet_mass)

# LDMC
print(sucpra_plots$LDMC)

# SLA
print(sucpra_plots$SLA)

# leaf_thickness
print(sucpra_plots$leaf_thickness)

# wet_mass vs dry_mass
print(sucpra_plots$wetmass_drymass)


# leuvul ---------------------
leuvul_data <- filtered_datasets[["leuvul"]]$data
leuvul_plots <- filtered_datasets[["leuvul"]]$plots

# Display leaf area plot
print(leuvul_plots$leaf_area)
# FSA3216: 1157.0 looks correct --> checked with shiny_leaf

# dry mass
print(leuvul_plots$dry_mass)

# wet mass
print(leuvul_plots$wet_mass)

# LDMC
print(leuvul_plots$LDMC)

# SLA
print(leuvul_plots$SLA)

# leaf_thickness
print(leuvul_plots$leaf_thickness)

# wet_mass vs dry_mass
print(leuvul_plots$wetmass_drymass)


# cennig ---------------------
cennig_data <- filtered_datasets[["cennig"]]$data
cennig_plots <- filtered_datasets[["cennig"]]$plots

# Display leaf area plot
print(cennig_plots$leaf_area)

# dry mass
print(cennig_plots$dry_mass)

# wet mass
print(cennig_plots$wet_mass)

# LDMC
print(cennig_plots$LDMC)

# SLA
print(cennig_plots$SLA)

# leaf_thickness
print(cennig_plots$leaf_thickness)

# wet_mass vs dry_mass
print(cennig_plots$wetmass_drymass)


# pimsax ---------------------
pimsax_data <- filtered_datasets[["pimsax"]]$data
pimsax_plots <- filtered_datasets[["pimsax"]]$plots

# Display leaf area plot
print(pimsax_plots$leaf_area)

# dry mass
print(pimsax_plots$dry_mass)

# wet mass
print(pimsax_plots$wet_mass)

# LDMC
print(pimsax_plots$LDMC)

# SLA
print(pimsax_plots$SLA)

# leaf_thickness
print(pimsax_plots$leaf_thickness)

# wet_mass vs dry_mass
print(pimsax_plots$wetmass_drymass)

# hypmac ---------------------
hypmac_data <- filtered_datasets[["hypmac"]]$data
hypmac_plots <- filtered_datasets[["hypmac"]]$plots

# Display leaf area plot
print(hypmac_plots$leaf_area)

# dry mass
print(hypmac_plots$dry_mass)

# wet mass
print(hypmac_plots$wet_mass)

# LDMC
print(hypmac_plots$LDMC)

# SLA
print(hypmac_plots$SLA)

# leaf_thickness
print(hypmac_plots$leaf_thickness)

# wet_mass vs dry_mass
print(hypmac_plots$wetmass_drymass)


# plalan ---------------------
plalan_data <- filtered_datasets[["plalan"]]$data
plalan_plots <- filtered_datasets[["plalan"]]$plots

# Display leaf area plot
print(plalan_plots$leaf_area)

# dry mass
print(plalan_plots$dry_mass)

# wet mass
print(plalan_plots$wet_mass)

# LDMC
print(plalan_plots$LDMC)

# SLA
print(plalan_plots$SLA)

# leaf_thickness
print(plalan_plots$leaf_thickness)

# wet_mass vs dry_mass
print(plalan_plots$wetmass_drymass)
# 2 points fall a bit out of the "line"
# check: FMX1959


# cyncri ---------------------
cyncri_data <- filtered_datasets[["cyncri"]]$data
cyncri_plots <- filtered_datasets[["cyncri"]]$plots

# Display leaf area plot
print(cyncri_plots$leaf_area)

# dry mass
print(cyncri_plots$dry_mass)
# GMJ6176: 77.60 but wet mass: 77.60

# wet mass
print(cyncri_plots$wet_mass)

# LDMC
print(cyncri_plots$LDMC)
# GMJ6176: 10000

# SLA
print(cyncri_plots$SLA)

# leaf_thickness
print(cyncri_plots$leaf_thickness)
# FVX6947: 0.09933333

# wet_mass vs dry_mass
print(cyncri_plots$wetmass_drymass)


# luzmul ---------------------
luzmul_data <- filtered_datasets[["luzmul"]]$data
luzmul_plots <- filtered_datasets[["luzmul"]]$plots

# Display leaf area plot
print(luzmul_plots$leaf_area)

# dry mass
print(luzmul_plots$dry_mass)

# wet mass
print(luzmul_plots$wet_mass)
# FNW0998: 326.50 but dry mass: 7.88
# FNN3644: 310.50 but dry mass: 7.99
# FOI4279: 205.10 but dry mass: 5.10

# LDMC
print(luzmul_plots$LDMC)

# SLA
print(luzmul_plots$SLA)

# leaf_thickness
print(luzmul_plots$leaf_thickness)

# wet_mass vs dry_mass
print(luzmul_plots$wetmass_drymass)



# tripra ---------------------
tripra_data <- filtered_datasets[["tripra"]]$data
tripra_plots <- filtered_datasets[["tripra"]]$plots

# Display leaf area plot
print(tripra_plots$leaf_area)

# dry mass
print(tripra_plots$dry_mass)

# wet mass
print(tripra_plots$wet_mass)

# LDMC
print(tripra_plots$LDMC)
# FVN2828: 20.89034

# SLA
print(tripra_plots$SLA)

# leaf_thickness
print(tripra_plots$leaf_thickness)

# wet_mass vs dry_mass
print(tripra_plots$wetmass_drymass)
# FVN2828: low dry mass: 10.23 but high wet mass: 489.70
 

# sildio ---------------------
sildio_data <- filtered_datasets[["sildio"]]$data
sildio_plots <- filtered_datasets[["sildio"]]$plots

# Display leaf area plot
print(sildio_plots$leaf_area)

# dry mass
print(sildio_plots$dry_mass)

# wet mass
print(sildio_plots$wet_mass)
# GGL5838: high wet mass: 953.00 but normal dry mass

# LDMC
print(sildio_plots$LDMC)

# SLA
print(sildio_plots$SLA)

# leaf_thickness
print(sildio_plots$leaf_thickness)

# wet_mass vs dry_mass
print(sildio_plots$wetmass_drymass)

















