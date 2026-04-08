#################################################################################
########## This script is the final model and figure generation based on ########
######### the work and trial and error done in the script, "workflow.R". ########
################################################################################

######## Packages ###########################

library(here)
library(dplyr)
library(glmmTMB)

library(nlme)
library(lme4)
library(MuMIn)
library(MASS)
library(DHARMa)

library(ggeffects)
library(ggplot2)

######## Pull in data and clean it ##########

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

#scale precipitation data to fit with other data
data$Annual_precip_s <- scale(data$Annual_precip)



######### Model ######################

# Model to estimate plant abundance from the presence/absence of all ants, all rodents, the total annual precipitation,
# with random factors of year and plot (crossed)
# Negative Binomial model, log link
# Variance included in the "dispformula" section, variance from all fixed variabels


model <- glmmTMB(total_abundance ~ All_ants + All_rodents + Annual_precip_s + (1|Year) + (1|Plot),
                         data = data,
                         family = nbinom2(link="log"),
                         dispformula = ~ Annual_precip_s + All_ants * All_rodents,
                         na.action = "na.fail", 
                         REML = FALSE)

saveRDS(model, file = "Documents/RDSmodel.rds")


########## Figures #####################

# Marginal effect of ant presence (averaged over precipitation)
eff_ants <- ggpredict(model_complex, terms = "All_ants")
plot(eff_ants)

# Marginal effect of precipitation (averaged over ant presence)
eff_precip <- ggpredict(model_complex, terms = "Annual_precip_s")
plot(eff_precip)

# Both together - predicted abundance across precip range, split by ant presence
eff_both <- ggpredict(model_complex, terms = c("Annual_precip_s", "All_ants"))
plot(eff_both)