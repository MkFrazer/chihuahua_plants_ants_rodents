####### Precipitation raw -> clean
###### Filtered to YEAR 1989-2002

#load packages
library(dplyr)
library(here)

#read in files to combine
precip_1 <- read.csv(here("data", "data_raw", "Precip_raw", "Portal_precipitation_19801989.csv"))
precip_2 <- read.csv(here("data", "data_raw", "Precip_raw", "Portal_precipitation_19892002.csv"))


#find column names that were new from 1989-2002 to exclude
colnames(precip_1)
colnames(precip_2)

#Filter and rename columns to be identical between data frames 
precip_2 <- precip_2 %>% select(-c(Hour, Day))
precip_1 <- rename(precip_1, Precipitation = precipitation)

#create average column to represent the average precipitation per year
precip_1 <- precip_1 %>% group_by(Year) %>% 
            mutate(Annual_avg_precip = mean(Precipitation, na.remove = TRUE)) %>% 
            ungroup()

precip_2 <- precip_2 %>% group_by(Year) %>% 
  mutate(Annual_avg_precip = mean(Precipitation, na.remove = TRUE)) %>% 
  ungroup()

#select Year and Annual_Average columns
precip_1 <- precip_1 %>% select(Year, Annual_avg_precip)
precip_2 <- precip_2 %>% select(Year, Annual_avg_precip)

#combine both data sets into one data frame
precip_clean <- full_join(precip_1, precip_2)
precip_clean <- precip_clean %>% distinct()

#limit to years 1983-2002
precip_clean <- filter(precip_clean, Year >= 1989)

#export to .csv
write.csv(precip_clean, file = "data/data_processed/precip_clean.csv", row.names = FALSE)
