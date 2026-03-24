#this script is for model comparison using AICc to determine which factors best predict plant abundance,
#and to correct for potential autocorrelation

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

###heteroskedasticity
plot(model0$fitted.values, model0$residuals)
#compare residuals against the factors in the model that aren't autocorrelation
plot(data$All_ants, model0$residuals)
plot(data$Annual_precip, model0$residuals)


###Autocorrelation

#check for temporal autocorrelation
plot(data$Year, model0$residuals)
all_resid <- residuals(model0, type = "pearson")
acf(all_resid)

#check for temporal autocorrelation within each plot
library(DHARMa)

data$all_resid <- all_resid

unique_plots <- unique(data$Plot)

par(mfrow = c(4, 6))
for (p in unique_plots) {
  plot_data <- data[data$Plot == p, ]
  plot_data <- data[order(plot_data$Year),]
  
  acf(plot_data$all_resid, main = paste("Plot ", p))
}

sim <- simulateResiduals(fittedModel = model0)
plot(sim)
par(mfrow = c(1,1))
plotQQunif(sim) 
plotResiduals(sim)
###acf shows no temporal autocorrelation per plot 

#plot residuals versus annual precip
plotResiduals(sim, form = data$Year)
plotResiduals(sim, form = data$Annual_precip)

plot(data$Annual_precip, data$total_abundance)
###increasing precip associated with increasing total abundance

#add variance structure for precip using model comparison
#fixed, power, exponential, but I have to change to glmmTMB to do that
library(glmmTMB)

#scale the variables to try and fix issues with glmmTMB
data$Annual_precip_s <- scale(data$Annual_precip)

model_tmb<- glmmTMB(total_abundance ~ All_ants + Annual_precip_s + (1|Year/Plot), 
                data = data,
                family = nbinom2(link = "log"),
                na.action = "na.fail",
                REML = FALSE)
model_fixed <-glmmTMB(total_abundance ~ All_ants + Annual_precip_s + (1|Year/Plot),
                      data = data,
                      family = nbinom2(link="log"),
                      dispformula = ~Annual_precip_s,
                      na.action = "na.fail", 
                      REML = FALSE)
model_power <- glmmTMB(total_abundance ~ All_ants + Annual_precip_s + (1|Year/Plot),
                       data = data,
                       family = nbinom2(link="log"),
                       dispformula = ~log(Annual_precip),
                       na.action = "na.fail", 
                       REML = FALSE)
model_exp <- glmmTMB(total_abundance ~ All_ants + Annual_precip_s + (1|Year/Plot),
                     data = data,
                     family = nbinom2(link="log"),
                     dispformula = ~(Annual_precip)^2,
                     na.action = "na.fail", 
                     REML = FALSE)

AIC(model_tmb, model_fixed, model_power)
###fixed variance is the better model

par(mfrow = c(1,1))
par(mar = c(5.1, 4.1, 4.1, 2.1))
plot(fitted(model_fixed), residuals(model_fixed, type = "pearson"))

fixed_sim <- simulateResiduals(model_fixed)
plot(fixed_sim)
###still correlation, likely spatial

#check for spatial autocorrelation within each year
plot(data$Plot, residuals(model_fixed))

plotResiduals(fixed_sim, form = data$Plot)
plotResiduals(fixed_sim, form = data$Year)
plotResiduals(fixed_sim, form = data$Annual_precip)



par(mfrow = c(4, 6))
for (p in unique_plots) {
  plot_data <- data[data$Plot == p, ]
  plot_data <- data[order(plot_data$Year),]
  
  acf(plot_data$all_resid, main = paste("Plot ", p))
}
