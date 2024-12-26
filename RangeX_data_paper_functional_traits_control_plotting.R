
# RangeX functional traits data exploration ----------------------------------

## Data used: RangeX_raw_functional_traits_2023.csv,
## RangeX_raw_functional_traits_leaf_area_NOR_2023.csv and RangeX_Metadata.csv 
## Date: 26.12.2024
## Author: Nadine Arzt
## Purpose: Control plotting of data


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



# write function to go through all species --------------------------------

radius <- function(r){
  2*pi*r
}

radius(3)
2*pi*3

