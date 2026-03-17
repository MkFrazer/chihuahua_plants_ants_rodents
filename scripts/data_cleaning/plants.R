####### plants raw -> clean
###### Filtered to YEAR 1989-2002
library(dplyr)
library(tidyr)
library(here)


#read in files to combine
plants_1 <- read.csv(here("data", "data_raw", "Plants_raw", "Portal_plant_summer_annual_19892002.csv"))
plants_2 <- read.csv(here("data", "data_raw", "Plants_raw", "Portal_plant_summer_perennial_19892002.csv"))
plants_3 <- read.csv(here("data", "data_raw", "Plants_raw", "Portal_plant_winter_annual_19892002.csv"))
plants_4 <- read.csv(here("data", "data_raw", "Plants_raw", "Portal_plant_winter_perennial_19892002.csv"))

#find column names that were new from 1988-2002 to exclude
colnames(plants_1)
colnames(plants_2)
colnames(plants_3)
colnames(plants_4)

###calculate total abundance per plot per year

#pivot to long data
long_plants_1 <- plants_1 %>%
  pivot_longer(
    cols = !c("Season", "Life.History", "Year", "Plot", "Quadrat"),
    names_to = "species",
    values_to = "abundance")
long_plants_1 <- long_plants_1 %>% mutate(abundance = na_if(abundance, -99))

long_plants_2 <- plants_2 %>%
  pivot_longer(
    cols = !c("Season", "Life.History", "Year", "Plot", "Quadrat"),
    names_to = "species",
    values_to = "abundance")
long_plants_2 <- long_plants_2 %>% mutate(abundance = na_if(abundance, -99))

long_plants_3 <- plants_3 %>%
  pivot_longer(
    cols = !c("Season", "Life.History", "Year", "Plot", "Quadrat"),
    names_to = "species",
    values_to = "abundance")
long_plants_3 <- long_plants_3 %>% mutate(abundance = na_if(abundance, -99))

long_plants_4 <- plants_4 %>%
  pivot_longer(
    cols = !c("Season", "Life.History", "Year", "Plot", "Quadrat"),
    names_to = "species",
    values_to = "abundance")
long_plants_4 <- long_plants_4 %>% mutate(abundance = na_if(abundance, -99))

#create a column for total species abundance per plot per year
total_abundance_1 <- long_plants_1 %>% 
  group_by(Plot, Year) %>% 
  summarize(
    total_abundance = sum(abundance, na.rm = TRUE),
    n_species = n(),
    .groups = "drop"
  )

total_abundance_2 <- long_plants_2 %>% 
  group_by(Plot, Year) %>% 
  summarize(
    total_abundance = sum(abundance, na.rm = TRUE),
    n_species = n(),
    .groups = "drop"
  )

total_abundance_3 <- long_plants_3 %>% 
  group_by(Plot, Year) %>% 
  summarize(
    total_abundance = sum(abundance, na.rm = TRUE),
    n_species = n(),
    .groups = "drop"
  )

total_abundance_4 <- long_plants_4 %>% 
  group_by(Plot, Year) %>% 
  summarize(
    total_abundance = sum(abundance, na.rm = TRUE),
    n_species = n(),
    .groups = "drop"
  )

#combine into 1 data frame, adding all number of species and abundances
plants <- merge(total_abundance_1, total_abundance_2, by = c("Plot","Year"), all = TRUE)
plants <- plants %>%  mutate(total_abundance = total_abundance.x + total_abundance.y, .keep = "unused")
plants <- plants %>%  mutate(total_species = n_species.x + n_species.y, .keep = "unused")

plants <- merge(plants, total_abundance_3, by = c("Plot", "Year"), all = TRUE)
plants <- plants %>%  mutate(abundance = total_abundance.x + total_abundance.y, .keep = "unused")
plants <- plants %>%  mutate(total_species = n_species + total_species, .keep = "unused")

plants <- merge(plants, total_abundance_4, by = c("Plot", "Year"), all = TRUE)
plants <- plants %>%  mutate(total_abundance = total_abundance + abundance, .keep = "unused")
plants <- plants %>%  mutate(total_species = n_species + total_species, .keep = "unused")

#create an "average abundance per species" column
plants <- plants %>% mutate(avg_abundance = total_abundance/total_species)

#limit to years 1989-2002
plants_clean <- filter(plants, Year >= 1989)

#export to .csv
write.csv(plants_clean, file = "data/data_processed/plants_clean.csv", row.names = FALSE)
