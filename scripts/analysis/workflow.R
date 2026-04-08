######################################################################
#this script is for model comparison using AICc to determine which factors #
#best predict plant abundance, and to correct for potential autocorrelation #

############################### AGGREGATION #################################

#create data frame from components precip, plants, and treatments
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


######################## Explore Data ##################################

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

#detect potential outliers using z-scores
z_scores <- scale(data$total_abundance)
outliers <- data$total_abundance[abs(z_scores) > 3]
print(outliers)


############################ Model Choice ###############################################

#load packages
library(nlme)
library(lme4)
library(MuMIn)
library(MASS)

#scale predictors to help optimizer converge
data$Annual_precip_s <- scale(data$Annual_precip)

#### Choose model: Poisson or Negative Binomial for this count data?

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
#Negative binomial model is best


######################### Choose Factors #############################################
#use dredge to compare all possible factor combinations
all_models_NB <- dredge(global_model_NB)
print(all_models_NB)
###either all_ants or PORU_ants, and Annual precip

#Year and Plot as random factors
model0 <- glmer.nb(total_abundance ~ All_ants + Annual_precip_s + (1|Year) + (1|Plot), 
                   data = data,
                   na.action = "na.fail")

#Compare Random effects, are Year and Plot nested, or crossed?
model1<- glmer.nb(total_abundance ~ All_ants + Annual_precip_s + (1|Year/Plot), 
                  data = data,
                  na.action = "na.fail")
AIC(model0,model1)
### better with crossed random factors, which makes sense

#however, because there is only a single precipitation value for each year, they are confounded.
#Is it better to include Year as a random factor, or is most of the year-to-year variance
#captured in Annual_precip?

#exclude Year as a random factor
model2<- glmer.nb(total_abundance ~ All_ants + Annual_precip_s + (1|Plot), 
                     data = data,
                     na.action = "na.fail")
AIC(model0, model2)
### the model with Year as a random factor is significantly better than without

fixef(model2)
fixef(model0)
###the coefficient of Annual Precipitation is about the same between the two models
###(0.47 without Year as a random factor, 0.41 with Year as a random factor), so we can
### report this value with relative confidence, even with confounding effects

########################### Check Residuals #####################################

###### DHARMa residuals ######

#install DHARMa package
library(DHARMa)

#compare simulated residuals to actual residuals
sim <- simulateResiduals(fittedModel = model0)
plot(sim)
###these residuals are all over the place. high deviation.

#check to see if a zero-inflation would be a better model
testZeroInflation(model0)
###No, all values <1

#check to see if adding another variable corrects for residual structure. 
#We saw in Model Selection Table that including All_rodents was only a delta of 1.57.
model_rodents <- glmer.nb(total_abundance ~ All_ants + All_rodents + Annual_precip_s + (1|Year) + (1|Plot), 
                          data = data,
                          na.action = "na.fail")
sim_rodents <- simulateResiduals(model_rodents)
plot(sim_rodents)
###these residuals seem much cleaner and smoother than before, now it's almost quadratic.

###### Normal residuals ######

#check residuals vs fitted
plot(fitted(model_rodents), residuals(model_rodents))
###we see an unbalanced x. possible need a quadratic

###### Find cause of residual structure ######

#compare residuals against each thing in the model to see structure

#Annual precip 
plot(data$Annual_precip, residuals(model_rodents))
###Residual variance increases with precipitation, 
###Strong negative skew at higher precipitation values

#All_ants
plot(data$All_ants, residuals(model_rodents))
###wider range when ants are present, both have long negative tails

#All_rodents
plot(data$All_rodents, residuals(model_rodents))
###size and range is similar between groups. both have long negative tails

###need to add variance structure

################################ Add Variance Structure #################################

#compare fixed, power, and exponential variance structures

#install glmmTMB package
library(glmmTMB)

model_tmb<- glmmTMB(total_abundance ~ All_ants + All_rodents + Annual_precip_s + (1|Year) + (1|Plot), 
                    data = data,
                    family = nbinom2(link = "log"),
                    na.action = "na.fail",
                    REML = FALSE)

model_fixed <-glmmTMB(total_abundance ~ All_ants + All_rodents + Annual_precip_s + (1|Year) + (1|Plot),
                      data = data,
                      family = nbinom2(link="log"),
                      dispformula = ~offset(log(Annual_precip)),
                      na.action = "na.fail", 
                      REML = FALSE)

model_power <- glmmTMB(total_abundance ~ All_ants + All_rodents + Annual_precip_s + (1|Year) + (1|Plot),
                       data = data,
                       family = nbinom2(link="log"),
                       dispformula = ~log(Annual_precip),
                       na.action = "na.fail", 
                       REML = FALSE)

model_exp <- glmmTMB(total_abundance ~ All_ants + All_rodents + Annual_precip_s + (1|Year) + (1|Plot),
                     data = data,
                     family = nbinom2(link="log"),
                     dispformula = ~ Annual_precip_s,
                     na.action = "na.fail", 
                     REML = FALSE)

model_constpower <- glmmTMB(total_abundance ~ All_ants + All_rodents + Annual_precip_s + (1|Year) + (1|Plot),
                         data = data,
                         family = nbinom2(link="log"),
                         dispformula = ~ Annual_precip_s + I(Annual_precip_s^2),
                         na.action = "na.fail", 
                         REML = FALSE)

AIC(model_tmb, model_fixed, model_power, model_exp, model_constpower)
### Exponential variance is the better model (or constant power, but they are identical)

#plot residuals of new model including exponential variance structure
plot(fitted(model_exp), residuals(model_exp, type = "pearson"))
sim_exp <- simulateResiduals(model_exp)
plot(sim_exp)

#add ants and rodents to see if that captures some variation in residuals
model_complex <- glmmTMB(total_abundance ~ All_ants + All_rodents + Annual_precip_s + (1|Year) + (1|Plot),
                       data = data,
                       family = nbinom2(link="log"),
                       dispformula = ~ Annual_precip_s + All_ants * All_rodents,
                       na.action = "na.fail", 
                       REML = FALSE)
sim_comp <- simulateResiduals(model_complex)
plot(sim_comp)
###no significant deviation in QQ plot, and smoother lines in DHARMa residual plot



############################# Autocorrelation ##########################################

plot(data$Year, residuals(model_complex, type = "pearson"))

all_resid <- residuals(model_complex, type = "pearson")
acf(all_resid)   #there might be some sort of temporal autocorrelation

#check for temporal autocorrelation within each plot

data$all_resid <- all_resid   #put residuals into original data frame

unique_plots <- unique(data$Plot)   #new df with each plot number

par(mfrow = c(4, 6)) #24 plots
for (p in unique_plots) {
  plot_data <- data[data$Plot == p, ]
  plot_data <- data[order(plot_data$Year),]
  
  acf(plot_data$all_resid, main = paste("Plot ", p)) #include plot number in title
}
#No significant autocorrelation by plot, autocorrelation must be from something else
par(mfrow = c(1,1))



############################ Check Outliers ############################################

# use Pearson residuals and flag anything beyond ±3
pearson_resids <- residuals(model_complex, type = "pearson")
outlier_idx <- which(abs(pearson_resids) > 3)

# View the suspicious observations
data[outlier_idx, ]

# Plot to see their location
plot(fitted(model_complex), pearson_resids)
###one clear point 6 sd away

#use DHARMa to see the effects of the outlier on the model
sim_complex <- simulateResiduals(model_complex)
testOutliers(sim_complex, type = "bootstrap")

# Visual - outliers flagged in red
plot(sim_complex)

model_no_outlier <- glmmTMB(total_abundance ~ All_ants + All_rodents + Annual_precip_s + (1|Year) + (1|Plot),
                            data = data[-outlier_idx, ],
                            family = nbinom2(link = "log"),
                            dispformula = ~ Annual_precip_s + All_ants * All_rodents,
                            na.action = "na.fail",
                            REML = FALSE)
summary(model_no_outlier)
summary(model_complex)
###coefficients are basically the same, so the outlier isn't affecting the model much

#check performance
performance::r2(model_complex)
############################## Interpretation #############################################

summary(model_complex)
#### in conditional model, the intercept and All_ants are significant predictors of total abundance
#### ants negatively impact total abundance by -0.3170 (p = 0.00569). 
#### in dispersion model, Annual precip (scaled) significantly affected the dispersion of the model

############################## Visualize ################################################
plot(data$All_ants, data$total_abundance,
     main = "Species abundance by ant presence/absence",
     xlab = "Ant presence, absence",
     ylab = "Plant species abundance")

plot(data$Annual_precip_s, data$total_abundance,
     main = "Species abundancy by anmual precip",
     xlab = "Annual precipitation (scaled)",
     ylab = "Plant species abundance")

lines(data$Year, predict(model_complex), col = "blue")

data$pred <- predict(model_complex)
ggplot(data, aes(x = factor(All_ants), y = total_abundance)) +
  geom_boxplot() +
  geom_point(aes(y = pred), color = "red", size = 3) 

#from claude
library(ggeffects)
library(ggplot2)

# Marginal effect of ant presence (averaged over precipitation)
eff_ants <- ggpredict(model_complex, terms = "All_ants")
plot(eff_ants)

# Marginal effect of precipitation (averaged over ant presence)
eff_precip <- ggpredict(model_complex, terms = "Annual_precip_s")
plot(eff_precip)

# Both together - predicted abundance across precip range, split by ant presence
eff_both <- ggpredict(model_complex, terms = c("Annual_precip_s", "All_ants"))
plot(eff_both)

#plot model prediction of precip on top of data
ggplot(data, aes(x = factor(All_ants), y = total_abundance)) +
  ggtitle("Predicted Plant Sp. Abundance from Annual Precipitation") +
  labs(x = "Presence/Absence of All Ants", y = "Total Species Abundnace") +
  geom_jitter(width = 0.1) + 
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "red") + 
  stat_summary(fun = mean, geom = "point", color = "red", size = 4) +
  annotate("text", x = 1.5, y = 3500, label = "Slope = -0.3170", col = "red")
  