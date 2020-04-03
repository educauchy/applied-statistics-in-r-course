Sys.setlocale(category = "LC_ALL", locale = "UTF-8")
path <- "~/Yandex.Disk.localized/Университет/Магистратура/2semester/R/Lecture-1/Datasets/"

library(psych)
library(dplyr)
library(readtext)

#Descriptive statistics
#Numerical characteristics
#Divide set for two by gender
#Plots
#Formulate hypothesis
#Write report (not official)


bb <- read.table(file = paste0(path, "babyboom.dat.txt"), header = F, col.names = c("time", "sex", "weight", "minutes"))
bb$sex <- as.factor(bb$sex)
male <- subset(bb, sex == "1")
female <- subset(bb, sex == "2")

describe(bb$weight)
summary(bb$weight)

hist(bb$minutes, breaks = 10, xlab = "Minutes", main = "Frequencies of minutes born")

hist(bb$weight, breaks = 9, xlab = "Weight (in gramms)", main = "Frequencies of weight")
boxplot(bb$weight, ylab = "Weight (in gramms)", main = "Boxplot of weight")

# hypothesis H0: mean weight male = mean weight female
# hypothesis H0: uniform distribution of male and female number of births
# hypothesis H0: uniform distribution number of births during the hours (24 intervals)


ew <- read.table(file = paste0(path, "euroweight.dat.txt"), header = F, col.names = c("id", "weight", "batch"), dec = ".")
print(head(ew))
# 8 packages

describe(ew$weight)
summary(ew$weight)

hist(ew$weight, breaks = 50, xlab = "Weight (in gramms)", main = "Frequencies of weight")
boxplot(ew$weight, ylab = "Weight (in gramms)", main = "Boxplot of weight")
qqnorm(ew$weight)

# hypothesis H0: normal distribution in each package
# hypothesis H0: uniform distribution between mean of packages


a <- read.csv(file = paste0(path, "airport.csv"), col.names = c("airport", "city", "schd", "perform", "pass", "freight", "mail"), header = F, sep = ";", dec = ".")
print(head(a))

plot(a$schd, main = "Scheduled departures", ylab = "Departures")
plot(a$perform, main = "Performed departures", ylab = "Departures")

plot(a$pass, main = "Enplaned passengers", ylab = "Passengers")
plot(a$freight, main = "Enplaned revenue tons of freight", ylab = "Revenue tons")
plot(a$mail, main = "Enplaned revenue tons of mail", ylab = "Revenue tons")

la <- subset(a, city == "LOS ANGELES")

