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
data$Plot <-as.factor(data$Plot)

#remove NAs
data<- na.omit(data)


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
library(lme4)
library(MuMIn)
library(MASS)

#first scale Annual_precip
data$Annual_precip_s <- scale(data$Annual_precip)

#create Poisson global model
global_model_P<- glmer(total_abundance ~ Large_rodents + All_rodents + PORU_ants + All_ants + Annual_precip_s + (1|Year) + (1|Plot), 
         data = data,
         family = poisson(link="log"),
         na.action = "na.fail")
#create negative binomial global model
global_model_NB <- glmer.nb(total_abundance ~ Large_rodents + All_rodents + PORU_ants + All_ants + Annual_precip_s + (1|Year) +(1|Plot), 
                       data = data,
                       na.action = "na.fail")
#compare AIC values between Poisson and Negative Binomial models
AIC(global_model_P, global_model_NB)

#compare AIC values of all possible models from global NegBi model
all_models_NB <- dredge(global_model_NB)
print(all_models_NB)
###either all_ants or PORU_ants need to be included in final model, and Annual precip
model0 <- glmer.nb(total_abundance ~ All_ants + Annual_precip_s + (1|Year) + (1|Plot), 
                 data = data,
                 na.action = "na.fail")

#Compare Random effects, nested to normal
model1<- glmer.nb(total_abundance ~ All_ants + Annual_precip_s + (1|Year/Plot), 
                  data = data,
                  na.action = "na.fail")
AIC(model0,model1)

###heteroskedasticity
plot(fitted(model0), residuals(model0))
#compare residuals against the factors in the model that aren't autocorrelation
plot(data$All_ants, residuals(model0))
plot(data$Annual_precip, residuals(model0))


###Autocorrelation

#check for temporal autocorrelation
plot(data$Year, residuals(model0, type="pearson"))
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
##acf shows no temporal autocorrelation per plot

#what does DHARMA say about residuals?
sim <- simulateResiduals(fittedModel = model0)
plot(sim)
par(mfrow = c(1,1))
###still strong deviations = model is systematically biased

#plot residuals versus annual precip
plotResiduals(sim, form = data$Year)
plotResiduals(sim, form = data$Annual_precip)

plot(data$Annual_precip, data$total_abundance)
###increasing precip associated with increasing total abundance

#add variance structure for precip using model comparison
#fixed, power, exponential, but I have to change to glmmTMB to do that
library(glmmTMB)

model_tmb<- glmmTMB(total_abundance ~ All_ants + Annual_precip_s + (1|Year) + (1|Plot), 
                data = data,
                family = nbinom2(link = "log"),
                na.action = "na.fail",
                REML = FALSE)
model_fixed <-glmmTMB(total_abundance ~ All_ants + Annual_precip_s + (1|Year) + (1|Plot),
                      data = data,
                      family = nbinom2(link="log"),
                      dispformula = ~Annual_precip_s,
                      na.action = "na.fail", 
                      REML = FALSE)
model_power <- glmmTMB(total_abundance ~ All_ants + Annual_precip_s + (1|Year) + (1|Plot),
                       data = data,
                       family = nbinom2(link="log"),
                       dispformula = ~log(Annual_precip),
                       na.action = "na.fail", 
                       REML = FALSE)
model_exp <- glmmTMB(total_abundance ~ All_ants + Annual_precip_s + (1|Year) + (1|Plot),
                     data = data,
                     family = nbinom2(link="log"),
                     dispformula = ~ Annual_precip_s + I(Annual_precip_s^2),
                     na.action = "na.fail", 
                     REML = FALSE)

AIC(model_tmb, model_fixed, model_power, model_exp)
###fixed variance is the better model

par(mfrow = c(1,1))
par(mar = c(5.1, 4.1, 4.1, 2.1))
plot(fitted(model_fixed), residuals(model_fixed, type = "pearson"))

fixed_sim <- simulateResiduals(model_fixed)
plot(fixed_sim, main = "Fixed variance")
###Looks better than the previous DHARMa comparison
###but correlation still present. Maybe spatial?

#check for spatial autocorrelation within each year
plot(data$Plot, residuals(model_fixed))

plot(fixed_sim, form = data$Plot)
plot(fixed_sim, form = data$Year)
plot(sim, form = data$Annual_precip)
plot(fixed_sim, form = data$Annual_precip)

#adding AR1 structure
model_both <- glmmTMB(total_abundance ~ All_ants + Annual_precip_s + ar1(as.factor(Year) + 0 | Plot) + (1|Year) + (1|Plot),
                      data = data,
                      family = nbinom2(link="log"),
                      dispformula = ~Annual_precip_s,
                      na.action = "na.fail", 
                      REML = FALSE)
AIC(model_fixed, model_temp, model_both)
#fixed model without AR1 was still best

sim_both<- simulateResiduals(model_both)
plot(sim_both, main = "AR1 and variation")

#what about adding a squared term
model_squared <-glmmTMB(total_abundance ~ All_ants + Annual_precip_s + I(Annual_precip_s ^2) + (1|Year) + (1|Plot),
                        data = data,
                        family = nbinom2(link="log"),
                        dispformula = ~Annual_precip_s,
                        na.action = "na.fail", 
                        REML = FALSE)
AIC(model_fixed, model_squared)
sim_square<- simulateResiduals(model_squared)
plot(sim_square, main = "Square term")

#what if we try adding back in a rodents term
model_rodents <-glmmTMB(total_abundance ~ All_ants + All_rodents + Annual_precip_s + (1|Year) + (1|Plot),
                        data = data,
                        family = nbinom2(link="log"),
                        dispformula = ~Annual_precip_s,
                        na.action = "na.fail", 
                        REML = FALSE)
AIC(model_fixed, model_rodents)
sim_rod <- simulateResiduals(model_rodents)
plot(sim_rod, main = "Rodents")

#what if we add a squared term and rodents?
model_rodents_squared <- glmmTMB(total_abundance ~ All_ants + All_rodents + Annual_precip_s + I(Annual_precip_s^2) + (1|Year) + (1|Plot),
                              data = data,
                              family = nbinom2(link="log"),
                              dispformula = ~Annual_precip_s,
                              na.action = "na.fail", 
                              REML = FALSE)
AIC(model_fixed, model_rodents, model_squared, model_rodents_squared)
sim_rod_square <- simulateResiduals(model_rodents_squared)
plot(sim_rod_square, main = "Rodents + Square term")

#what if without annual precip?
model_noratsprecip <- glmmTMB(total_abundance ~ All_ants + (1|Year) + (1|Plot),
                            data = data,
                            family = nbinom2(link="log"),
                            na.action = "na.fail", 
                            REML = FALSE)
model_noprecip <- glmmTMB(total_abundance ~ All_ants + All_rodents + (1|Year) + (1|Plot),
                          data = data,
                          family = nbinom2(link="log"),
                          na.action = "na.fail", 
                          REML = FALSE)

AIC(model_noratsprecip, model_noprecip, model_rodents)
sim_noratsprecip <- simulateResiduals(model_noratsprecip)
plot(sim_noratsprecip)

sim_noprecip <-simulateResiduals(model_noprecip)
plot(sim_noprecip)

###but no spatial coordinates provided to account for spatial autocorrelation\

###SUMMARY OF MODEL
summary(model_rodents)


#### adding complex variance structure for ants and rodent
model_vants <- glmmTMB(total_abundance ~ All_ants + All_rodents + Annual_precip_s + (1|Year) + (1|Plot),
                     data = data,
                     family = nbinom2(link="log"),
                     dispformula = ~ Annual_precip_s + All_ants * All_rodents,
                     na.action = "na.fail", 
                     REML = FALSE)
sim_vants <- simulateResiduals(model_vants)
plot(sim_vants)
