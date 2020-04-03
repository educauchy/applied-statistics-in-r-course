Sys.setlocale(category = "LC_ALL", locale = "UTF-8")
path <- "~/Yandex.Disk.localized/Университет/Магистратура/2semester/R/Lecture-4/Datasets/"

library(car)
library(MASS)
library(leaps)

# BLOOD
blood <- read.csv(file = paste0(path, "blood.csv"), header = T, col.names = c("pressure", "age", "weight"), dec = ".", sep = ";")

model <- lm(pressure ~ age + weight, data = blood)
summary(model)
# 1 all coefficients are significant | p-value < 0.05 => significant
# 2 R^2 is significant | p-value = 0 < 0.5 => significant

# residuals
rsd <- model$residuals
plot(rsd) # randomized but tend to increase

ncvTest(model)
# 3 the residuals of the residuals is constant | p-value = 0.7 > 0.05 => homoscedasticity
# it is true because positive residuals compensate negative residuals,
# so in average mean of residuals = 0

durbinWatsonTest(model)
# 4 residuals are not autocorrelated | p-value = 0.8 > 0.05 => not autocorrelated
# there is a flat trend in residuals nevertheless each of them is independent from others




# KUIPER
kuiper <- read.csv(file = paste0(path, "kuiper.csv"), header = T, col.names = c("price", "mileage", "make", "model", "trim", "type", "cylinder", "liter", "doors", "cruise", "sound", "leather"), dec = ",", sep = ";")
model <- lm(price ~ mileage + liter + cruise + sound + leather, data = kuiper)

# 1 build models by stepwise regression
leaps <- regsubsets(price ~ mileage + liter + cruise + sound + leather, data = kuiper, nbest = 5, intercept = T)
plot(leaps, scale = "adjr2")
# opacity - level of significance
# blace - significance on 0.05, grey - 0.1, ...

AIC(lm(price ~ mileage + liter + cruise + sound + leather, data = kuiper))
AIC(lm(price ~ mileage + liter + cruise + leather, data = kuiper)) # without sound
AIC(lm(price ~ liter + cruise + sound + leather, data = kuiper)) #without mileage
AIC(lm(price ~ mileage + liter + cruise + sound, data = kuiper)) # without leather
AIC(lm(price ~ mileage + liter + cruise, data = kuiper)) # without sound, leather
# the best model is with all factors

# but let's check it using built-in function
model_aic <- stepAIC(model)
# the best model is still with all factors

# 2 significance of the model
summary(model) # model is significant | p-value = 0 < 0.05

# residuals
rsd <- model$residuals
plot(rsd)

ncvTest(model)
# 3 the residuals of the residuals is constant | p-value = 0 < 0.05 => heteroscedasticity
# most residuals lay under zero, to mean is significantly different from 0

durbinWatsonTest(model)
# 4 residuals are not autocorrelated | p-value = 0 < 0.05 => autocorrelated
# we can see two ridges and two vertical lines, so most of observations are autocorrelated




# CIGARETTES
cig <- read.table(file = paste0(path, "cigarettes.dat.txt"), header = F, col.names = c("brand", "carbon", "tar", "nicotine", "weight"), dec = ".")
model <- lm(carbon ~ tar + nicotine + weight, data = cig)
summary(model)
# 1 nicotine is not significant | p-value = 0.94 > 0.05 => not significant
# 2 R^2 is significant | p-value = 0 < 0.5 => significant

# residuals
rsd <- model$residuals
plot(rsd) # randomized but tend to increase

ncvTest(model)
# 3 the residuals of the residuals is constant | p-value = 0.5 > 0.05 => homoscedasticity
# obvious to see without explanation

durbinWatsonTest(model)
# 4 residuals are not autocorrelated | p-value = 0.5 > 0.05 => not autocorrelated
# residuals randomly distributed near mean = 0