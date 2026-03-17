#this script is for model comparison using AICc to determine which factors best predict plant abundance

####AGGREGATION

#create data frame from components precip, plants, and treatments

precip <- read.csv(here("data", "data_processed", "precip_clean.csv"))
plants <- read.csv(here("data", "data_processed", "plants_clean.csv"))
treatments <- read.csv(here("data", "data_processed", "treatments.csv"))

#fix treatments column names to match
treatments <- rename(treatments, Plot = PLOT, Year = YEAR, Large_rodents = LARGE_RODENTS, All_rodents = ALL_RODENTS, PORU_ants = PORU_ANTS, All_ants = ALL_ANTS)

#combine data frames
data <- full_join(precip, plants, by = "Year")
data <- full_join(data, treatments, by = c("Plot", "Year"))

#order columns, filter to >=1989
data <- data[, c("Plot", "Year", "total_species", "total_abundance", "avg_abundance", "Large_rodents", "All_rodents", "PORU_ants", "All_ants", "Annual_precip")]
data<- filter(data, Year >= 1989)
data<- data[order(data$Plot), ]

#make presence/absence treatments binary categories
data$Large_rodents<- as.factor(data$Large_rodents)
data$All_rodents<- as.factor(data$All_rodents)
data$PORU_ants<- as.factor(data$PORU_ants)
data$All_ants<- as.factor(data$All_ants)


####VISUALIZATION

#plot total species abundance as a factor of precip
plot(data$Annual_precip, data$total_abundance)

#plot total species abundance as a factor of large rodent presence/absence
plot(data$Large_rodents, data$total_abundance)

#plot total species abundance as a factor of all rodent presence/absence
plot(data$All_rodents, data$total_abundance)

#plot total species abundance as a factor of PORU presence/absence
plot(data$PORU_ants, data$total_abundance)

#plot total species abundance as a factor of all ants presence/absence
plot(data$All_ants, data$total_abundance)


#### model comparison

m1<- glm()