#this script visualizes the data to determine what models best represent the data, if any
#autocorrelation exists, and if residuals match expectations

#create data frame from components precip, plants, and treatments

precip <- read.csv(here("data", "data_processed", "precip_clean.csv"))
plants <- read.csv(here("data", "data_processed", "plants_clean.csv"))
treatments <- read.csv(here("data", "data_processed", "treatments.csv"))

#fix treatements column names to match
treatments <- rename(treatments, Plot = PLOT, Year = YEAR, Large_rodents = LARGE_RODENTS, All_rodents = ALL_RODENTS, PORU_ants = PORU_ANTS, All_ants = ALL_ANTS)

data <- full_join(precip, plants, by = "Year")
data <- full_join(data, treatments, by = c("Plot", "Year"))

data <- data[, c("Plot", "Year", "total_species", "total_abundance", "avg_abundance", "Large_rodents", "All_rodents", "PORU_ants", "All_ants", "Annual_avg_precip")]
data<- data[order(data$Plot), ]
data<- data %>% distinct()
