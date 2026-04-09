library(here)
library(dplyr)
library(DHARMa)

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

data$Annual_precip_s <- as.vector(scale(data$Annual_precip))


#model
library(glmmTMB)

model_complex <- glmmTMB(total_abundance ~ All_ants + All_rodents + Annual_precip_s + (1|Year) + (1|Plot),
                         data = data,
                         family = nbinom2(link="log"),
                         dispformula = ~ Annual_precip_s + All_ants * All_rodents,
                         na.action = "na.fail", 
                         REML = FALSE)

#generate DHARMa residuals
sim_complex <- simulateResiduals(model_complex)
plot(sim_complex)