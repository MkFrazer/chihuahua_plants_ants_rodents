####### Ants raw -> clean
###### Filtered to variables YEAR, PLOT, from 1977-2002
library(dplyr)
library(here)


#read in files to combine
<<<<<<< HEAD
ants_1 <- read.csv(here("data", "data_raw", "Ants_raw", "Portal_ant_colony_19771987.csv"))
ants_2 <- read.csv(here("data", "data_raw", "Ants_raw", "Portal_ant_colony_19882002.csv"))
=======
ants_1 <- read.csv(here("Mini-Project", "data", "data_raw", "Ants_raw", "Portal_ant_colony_19771987.csv"))
ants_2 <- read.csv(here("Mini-Project", "data", "data_raw", "Ants_raw", "Portal_ant_colony_19882002.csv"))
>>>>>>> f313600d2af9339e3a12ad073df326f073a790b5

#find column names that were new from 1988-2002 to exclude
colnames(ants_1)
colnames(ants_2)

#Filter 1988-2002 data to columns we want to keep 
ants_2.1 <- ants_2 %>% select(-c(MONTH, DAY, STAKE, PHMI, PHMIO, PHYE, PHYEO, POPI, POPIO, UNKSP, UNKSPO))
ants_2.1 <- ants_2 %>% select(-MONTH, -DAY, -STAKE, -PHMI, -PHMIO, -PHYE, -PHYEO, -POPI, -POPIO, -UNKSP, -UNKSPO)

#rename species codes that were not identical
names(ants_2.1)[names(ants_2.1) == 'SOLA.pres.1.'] <- 'SOLA'
names(ants_2.1)[names(ants_2.1) == 'SOLB.pres.1.'] <- 'SOLB'

#combine both data sets into one data frame
ants_clean <- full_join(ants_1, ants_2.1)

#limit to years 1983-2002
ants_clean <- filter(ants_clean, YEAR >= 1983)

<<<<<<< HEAD
write.csv(ants_clean, file = "data/data_processed/ants_clean.csv")
=======
#export to .csv
output_path <- here("Mini-Project", "data", "data_processed")
write.csv(ants_clean, output_path, row.names = FALSE)
>>>>>>> f313600d2af9339e3a12ad073df326f073a790b5

