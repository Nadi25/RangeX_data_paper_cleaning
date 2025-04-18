
# RangeX functional traits data exploration & control plotting ----------------------------------

## Data used: RangeX_raw_functional_traits_NOR_2023.csv,
##            RangeX_raw_functional_traits_leaf_area_NOR_2023.csv, 
##            RangeX_Metadata.csv 
## Date:      26.12.2024
## Author:    Nadine Arzt
## Purpose:   Control plotting of data


# Check these! ------------------------------------------------------------
# GMJ6176: cyncri: drymass: 77.60 but wet mass: 77.60 --> dry = 0.02398 --> fixed
# FVX6947: cyncri: leaf_thickness: 0.09933333 --> correct on envelope 
# FMX1959: plalan: wetmass and drymass --> correct on envelope 
# FNW0998: luzmul: wetmass: 326.50 but dry mass: 7.88 --> correct on envelope 
# FNN3644: luzmul: wetmass: 310.50 but dry mass: 7.99 --> correct on envelope 
# FOI4279: luzmul: wetmass: 205.10 but dry mass: 5.10 --> correct on envelope 
# FVN2828: tripra: LDMC: 20.89034
# FVN2828: tripra: low dry mass: 10.23 but high wet mass: 489.70 --> correct on envelope 
# GGL5838: sildio: high wet mass: 953.00 but normal dry mass: 15.21
# sucpra: NOR.lo.ambi.vege.wf.01.21: dry mass = 468.18
# sucpra: NOR.lo.ambi.vege.wf.02.09: dry mass = 421.72
# FSF6631: hypmac: only drymass, no wet mass, no thickness --> delete?



# load library ------------------------------------------------------------
library(purrr)
library(openxlsx)

# source clean functional trait data file from cleaning R script ---------------------------------
source("RangeX_data_paper_cleaning_functional_traits.R")


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

# function to get subset per species and plot all traits separate ---------

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



# Get a list of unique species --------------------------------------------
unique_species <- unique(traits_plotting$species)


# loop through all species ------------------------------------------------
# Use map to create a list of filtered datasets and plots
filtered_datasets <- map(unique_species[1:10], ~ filter_plot_species(traits_plotting, .x))

# Name the list elements with the species names
names(filtered_datasets) <- unique_species[1:10]

# Print the names of the datasets created
print(names(filtered_datasets))


# access dataset and its plots by species name ----------------------------

# sucpra ------------------------------------------------------------------
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

# save hypmac dataset
# write.xlsx(hypmac_data, "Data/RangeX_functional_traits_hypmac.xlsx")

# Display leaf area plot
print(hypmac_plots$leaf_area)

# dry mass
print(hypmac_plots$dry_mass)

# wet mass
print(hypmac_plots$wet_mass)
# FSF6631: wet mass missing

# LDMC
print(hypmac_plots$LDMC)
# GFF4648: low LDMC: 166.4865

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









