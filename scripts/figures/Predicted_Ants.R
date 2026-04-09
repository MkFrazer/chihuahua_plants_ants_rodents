#plot model prediction of precip on top of data

library(here)
library(dplyr)

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

#make presence/absence treatments binary categories, plot as factor
data$Large_rodents<- as.factor(data$Large_rodents)
data$All_rodents<- as.factor(data$All_rodents)
data$PORU_ants<- as.factor(data$PORU_ants)
data$All_ants<- as.factor(data$All_ants)
data$Plot <-as.factor(data$Plot)


library(ggplot2)

ggplot(data, aes(x = factor(All_ants), y = total_abundance)) +
  ggtitle("Predicted Plant Spescies Abundance from Ant Exclusion") +
  labs(x = "Presence/Absence of All Ants", y = "Total Species Abundance") +
  geom_jitter(width = 0.1) + 
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "red") + 
  stat_summary(fun = mean, geom = "point", color = "red", size = 4) +
  annotate("text", x = 1.5, y = 3500, label = "Slope = -0.3170", col = "red")


