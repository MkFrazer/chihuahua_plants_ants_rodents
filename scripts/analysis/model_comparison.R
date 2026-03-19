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
library(nlme)
library(MuMIn)
library(MASS)

#create Poisson global model
global_model_P<- glm(total_abundance ~ Large_rodents + All_rodents + PORU_ants + All_ants + Annual_precip + (1|Year/Plot), 
         data = data,
         family = poisson(link="log"),
         na.action = "na.fail")
#create negative binomical global model
global_model_NB <- glm.nb(total_abundance ~ Large_rodents + All_rodents + PORU_ants + All_ants + Annual_precip + (1|Year/Plot), 
                       data = data,
                       link = log,
                       na.action = "na.fail")
#compare AIC values between Poisson and Negative Binomial models
AIC(global_model_P, global_model_NB)

#compare AIC values of all possible models from global NegBi model
all_models_NB <- dredge(global_model_NB)
print(all_models_NB)
###either all_ants or PORU_ants need to be included in final model, and Annual precip
model0 <- glm.nb(total_abundance ~ All_ants + Annual_precip + (1|Year/Plot), 
                 data = data,
                 link = log,
                 na.action = "na.fail")
#check for temporal autocorrelation
plot(data$Year, model0$residuals)
all_resid <- residuals(model0, type = "pearson")
acf(all_resid)

#check for temporal autocorrelation within each plot
library(DHARMa)
#simulated residuals
sim_res <- simulateResiduals(model0)
norm_resid <- sim_res$scaledResiduals

data$norm_resid <- norm_resid

unique_plots <- unique(data$Plot)

par(mfrow = c(4, 6))
for (p in unique_plots) {
  plot_data <- data[data$Plot == p, ]
  plot_data <- data[order(plot_data$Year),]
  
  if (nrow(plot_data) > 5) {
    acf(plot_data$norm_resid)
  }
}
#actual residuals

data$all_resid <- all_resid

unique_plots <- unique(data$Plot)

par(mfrow = c(4, 6))
for (p in unique_plots) {
  plot_data <- data[data$Plot == p, ]
  plot_data <- data[order(plot_data$Year),]
  
  acf(plot_data$all_resid)
}

sim <- simulateResiduals(fittedModel = model0)
plot(sim)


###acf shows no temporal autocorrelation

# spatial autocorrelation
# plot as random effect
# precip as factor
# random slope of annual precip?
#   temporal autocorrealtion, but look plot by plot ove ryear
# spatial autocorrelation: look at spatial correlation in each year
# (1|year/plot)