Sys.setlocale(category = "LC_ALL", locale = "UTF-8")
path <- "~/Yandex.Disk.localized/Университет/Магистратура/2semester/R/Lecture-6/Datasets/"

library(MASS)
library(quantreg)
library(ridge)
library(glmnet)

compare <- function(model, df, actual) {
  predicted <- predict(model, df)
  compare <- cbind(actual, predicted)
  acrc <- mean(apply(compare, 1, min) / apply(compare, 1, max))
  return(acrc)
}

# BLOOD
blood <- read.csv(file = paste0(path, "blood.csv"), header = T, col.names = c("pressure", "age", "weight"), dec = ".", sep = ";")
cor(blood)

#X <- matrix(c(rep(1, nrow(blood)), blood$age, blood$weight), ncol = 3)
#X
#det(t(X) %*% X)

# estimate ridge regression
est <- linearRidge(pressure ~ age + weight, data = blood, lambda = seq(0.1, 10, 0.1))
est
summary(est)
plot(est)

# optimal lambda could be found by cross-validation method but it's beyond the scope of our course
# so choose lambda = 10
brm <- linearRidge(pressure ~ age + weight, data = blood, lambda = 10)
summary(brm)
brm_accuracy <- compare(brm, blood, blood$pressure)
brm_accuracy # 0.939

blm <- lm(pressure ~ age + weight, data = blood)
summary(blm)
#plot(blm)
blm_accuracy <- compare(blm, blood, blood$pressure)
blm_accuracy # 0.988

# OSL better than ridge according to accuracy measurement

bqm <- rq(pressure ~ age + weight, data = blood, tau = 1:9/10)
plot(bqm)



# KUIPER
kuiper <- read.csv(file = paste0(path, "kuiper.csv"), header = T, col.names = c("price", "mileage", "make", "model", "trim", "type", "cylinder", "liter", "doors", "cruise", "sound", "leather"), dec = ",", sep = ";")
cor(kuiper[,c('price', 'mileage', 'liter', 'cruise', 'sound', 'leather')])

# build models by stepwise regression
par(mfrow=c(1,1))
leaps <- regsubsets(price ~ mileage + liter + cruise + sound + leather, data = kuiper, nbest = 5, intercept = T)
plot(leaps, scale = "adjr2")
# opacity - level of significance: black - significance on 0.05, grey - 0.1, ...
# choose all factors

# estimate ridge regression
est <- linearRidge(price ~ mileage + liter + cruise + sound + leather, data = kuiper, lambda = seq(0.1, 10, 0.1))
summary(est)
plot(est)

# optimal lambda could be found by cross-validation method but it's beyond the scope of our course
# so choose lambda = 10
brm <- linearRidge(price ~ mileage + liter + cruise + sound + leather, data = kuiper, lambda = 10)
summary(brm)
brm_accuracy <- compare(brm, kuiper, kuiper$price)
brm_accuracy # 0.727

blm <- lm(price ~ mileage + liter + cruise + sound + leather, data = kuiper)
summary(blm)
#plot(blm)
blm_accuracy <- compare(blm, kuiper, kuiper$price)
blm_accuracy # 0.779

# OSL better than ridge according to accuracy measurement

bqm <- rq(price ~ mileage + liter + cruise + sound + leather, data = kuiper, tau = 1:9/10)
plot(bqm)



# CIGARETTES
cig <- read.table(file = paste0(path, "cigarettes.dat.txt"), header = F, col.names = c("brand", "carbon", "tar", "nicotine", "weight"), dec = ".")
cor(cig[,c('carbon', 'tar', 'nicotine', 'weight')])

# estimate ridge regression
est <- linearRidge(carbon ~ tar + nicotine + weight, data = cig, lambda = seq(0.1, 10, 0.1))
summary(est)
plot(est)

# optimal lambda could be found by cross-validation method but it's beyond the scope of our course
# so choose lambda = 10
brm <- linearRidge(carbon ~ tar + nicotine + weight, data = cig, lambda = 10)
summary(brm)
brm_accuracy <- compare(brm, cig, cig$carbon)
brm_accuracy # 0.745

blm <- lm(carbon ~ tar + nicotine + weight, data = cig)
summary(blm)
#plot(blm)
blm_accuracy <- compare(blm, cig, cig$carbon)
blm_accuracy # 0.896

bqm <- rq(carbon ~ tar + nicotine + weight, data = cig, tau = 1:9/10)
plot(bqm)
