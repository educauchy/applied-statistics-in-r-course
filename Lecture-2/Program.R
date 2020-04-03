Sys.setlocale(category = "LC_ALL", locale = "UTF-8")
path <- "~/Yandex.Disk.localized/Университет/Магистратура/2semester/R/Lecture-2/Datasets/"

library(ggplot2)
library(dplyr)

# Babyboom
boom <- read.table(file = paste0(path, "babyboom.dat.txt"), header = F, col.names = c("time", "sex", "weight", "minutes"))
boom$sex <- as.factor(boom$sex)
male <- subset(boom, sex == "1")
female <- subset(boom, sex == "2")

# Test a weight of babies for normality. First, for the whole dataset, then for group of boys and girls independently.
hist(boom$weight, main = "Babies weight distribution")
qqnorm(boom$weight)
shapiro.test(boom$weight) # p-value = 0 < 0.05 => not normal

hist(male$weight, main = "Male weight distribution")
qqnorm(male$weight)
shapiro.test(male$weight) # p-value = 0.017 < 0.050 => not normal

hist(female$weight, main = "Female weight distribution")
qqnorm(female$weight)
shapiro.test(female$weight) # p-value = 0.20 > 0.05 => normal

# Test the hypothesis if the mean of the weight of girls is the same as the weight of boys
# We can't use t-test for comparing means because only one sample is normally distributed

# Test the hypothesis if the variance of the weight of girls is the same as the weight of boys
# We can't checkout equity of variances because we failed with assumption of equity of means

# Test the hypothesis if the time between birthtime is distributed by exponential distribution
minutes_diff <- diff(boom$minutes)
hist(minutes_diff, main = "Minutes between births distribution")
ks.test(boom$minutes, "pexp") # p-value = 0 < 0.05 => not exponential

# Test the hypothesis if the births per hour for each hour is distributed by Poisson distribution
#boom_grouped <- cut(boom$minutes, c(0, seq(60, 1440, by = 60)), include.lowest = T, right = T) # 1440 minutes in 24 hours
#groups <- hist(boom, breaks = c(0, 60, 1440))
boom$group <- cut(boom$minutes, c(0, seq(60, 1440, by = 60)), include.lowest = T, right = T)
groups <- boom %>% group_by(group) %>% count()
groups$n <- as.numeric(groups$n)
ks.test(groups$n, "ppois", lambda = 1.5)



# Euro coins weight
coins <- read.table(file = paste0(path, "euroweight.dat.txt"), header = F, col.names = c("id", "weight", "batch"), dec = ".")
coins$batch <- as.factor(coins$batch)

# Test the weight of coins for normality (all coins and coins in packages)
hist(coins$weight, main = "Weights distribution")
qqnorm(coins$weight)
shapiro.test(coins$weight) # p-value = 0 < 0.05 => not normal

# Test the hypothesis that the mean of the weight of coins is the same in different packages.
# First, we need to check normality assumptions
shapiro.test(coins[which(coins$batch == 1), ]$weight) # p-value = 0.683 > 0.05 => normal
shapiro.test(coins[which(coins$batch == 2), ]$weight) # p-value = 0.122 > 0.05 => normal
shapiro.test(coins[which(coins$batch == 3), ]$weight) # p-value = 0 < 0.05 => not normal

# We can't use ANOVA, but I did ...
ggplot(coins, aes(x = batch, y = weight)) +
  geom_boxplot(fill = "grey80", colour = "blue") +
  scale_x_discrete() + xlab("Treatment Group") +
  ylab("Dried weight of plants")
summary(aov(weight ~ batch, data = coins)) # p-value = 0 < 0.05 => significant differencies



# Iris
iris <- read.table(file = paste0(path, "iris.txt"), header = F, col.names = c("sepal_length", "sepal_width", "petal_length", "petal_width", "type"), dec = ".", sep = ",")



# Height
height <- read.table(file = paste0(path, "height.csv"), header = T, sep = ";", dec = ",")

# Test the normality of heights of football and basketball players
hist(height$football, main = "Football players' height distribution")
qqnorm(height$football)
shapiro.test(height$football) # p-value = 0.016 < 0.050 => not normal

hist(height$basketball, main = "Basketball players' height distribution")
qqnorm(height$basketball)
shapiro.test(height$basketball) # p-value = 0.31 > 0.05 => normal

# Test the equity of means and variances of the heights of football and basketball players
# We can't check equity of means because only one sample is normally distributed
# We can't check equity of variances because we can't check equity of means

# Test if the distributions of the heights of football and basketball players are the same
ks.test(height$football, height$basketball) # p-value = 0.029 < 0.050 => distributions are different



# Surgery
surgery <- read.table(file = paste0(path, "surgery.csv"), header = T, sep = ";", dec = ",")
surgery <- na.omit(surgery)
surgery$output <- ifelse(surgery$v_l_before < surgery$v_l_after &
                           surgery$v_r_before < surgery$v_r_after, 1, 0)
binom.test(table(factor(levels = c(1, 0), x = surgery$output)), p = 0.7, alternative = "greater") # p-value = 0.06 > 0.05 => 

