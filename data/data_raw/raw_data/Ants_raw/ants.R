####### Ants raw -> clean
###### Filtered to variables YEAR, PLOT, from 1977-2002
library(dplyr)

#read in files to combine
ants_1 <- read.csv("C:/Users/madel/OneDrive - UBC/UBCO/Courses/Reproducibility/Mini-Project/Data/raw_data/Ants_raw/Portal_ant_colony_19771987.csv")
ants_2 <- read.csv("C:/Users/madel/OneDrive - UBC/UBCO/Courses/Reproducibility/Mini-Project/Data/raw_data/Ants_raw/Portal_ant_colony_19882002.csv")

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

#export to .csv
write.csv(ants_clean, "C:/Users/madel/OneDrive - UBC/UBCO/Courses/Reproducibility/Mini-Project/Data/clean_data/ants_clean.csv", row.names = FALSE)
