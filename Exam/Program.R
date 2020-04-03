Sys.setlocale(category = "LC_ALL", locale = "UTF-8")
path <- "~/Yandex.Disk.localized/Университет/Магистратура/2semester/R/Exam/Datasets/"

library(ggpubr)
library(EnvStats)
library(nortest)
library(NbClust)
library(factoextra)
library(cluster)
library(ridge)
library(psych)
library(aod)
library(glmnet)
library(quantreg)
library(MASS)

#Elbow Method for finding the optimal number of clusters
elbow <- function(data) {
  set.seed(123)
  # Compute and plot wss for k = 2 to k = 15.
  k.max <- 15
  wss <- sapply(1:k.max, 
                function(k) {
                  kmeans(data, k, nstart=50, iter.max = 15)$tot.withinss
                })
  wss
  plot(1:k.max, wss,
       type="b", pch = 19, frame = FALSE, 
       xlab="Number of clusters K",
       ylab="Total within-clusters sum of squares")
}


# Mode function
mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}



# 1
v <- read.csv(file = paste0(path, "Task1.csv"), header = F, dec = ".", sep = ";")$V1
summary(v)
stat <- describe(v)
stat
mode(v)

plot(ecdf(v), xlab = "Value", main = "Cumulative distribution function")
ggboxplot(v, main = "Boxplot diagram of the dataset")
hist(v, xlab = "Value", breaks = 3, main = "Histogram of the dataset with 2 breaks")
hist(v, xlab = "Value", breaks = 6, main = "Histogram of the dataset with 6 breaks")
hist(v, xlab = "Value", breaks = 9, main = "Histogram of the dataset with 12 breaks")



# 2
n <- read.csv(file = paste0(path, "Task2.csv"), header = F, dec = ".", sep = ";")$V1
hist(n, xlab = "Value", breaks = 10, main = "Histogram of the dataset")
lillie.test(n)
pearson.test(n)
cvm.test(n)
ad.test(n)
shapiro.test(n)


# 3
# first it is necessary to check whether distributions are normal
shapiro.test(v) # not normal
shapiro.test(n) # not normal

dfs <- data.frame(
  v = c(v, n),
  dist = factor(c(rep("1st", length(v)), rep("2nd", length(n))))
)
ggplot(dfs, aes(x=v, fill=dist)) +
  geom_density(alpha=.3)

# 3-1
t.test(v, n) # cannot use

# 3-2
fisher.test(v, n) # cannot use

# 3-3
ks.test(v, n)

# 3-4
wilcox.test(v, n, conf.level = 0.9)



# 4
# 4-1
reg <- read.csv(file = paste0(path, "Task4-1.csv"), header = T, dec = ",", sep = ";")
model_osl <- lm(Y ~ ., data = reg)
summary(model_osl)

x_m <- as.matrix(reg[, 2:5])
lambdas <- 10^seq(3, -2, by = -.1)
cv.out <- cv.glmnet(x_m, reg$Y, alpha = 0, lambda = lambdas)
plot(cv.out)
lambda_opt <- cv.out$lambda.min
rdg <- glmnet(x_m, reg$Y, alpha = 0, lambda = lambda_opt)
summary(rdg)
plot(rdg)
rdg_prd <- predict(rdg, s = lambda_opt, newx = x_m)
rdg_prd
sst <- sum((reg$Y - mean(reg$Y))^2)
sse <- sum((rdg_prd - reg$Y)^2)
rdg_rsq <- 1 - sse / sst
print(rdg_rsq)
summary(linearRidge(Y ~ ., data = reg, lambda = lambda_opt))



# 4-2
bin <- read.csv(file = paste0(path, "Task4-2.csv"), header = T, dec = ".", sep = ",")
model_logit <- glm(Y ~ ., data = bin, family = binomial("logit"))
summary(model_logit)
wald.test(b = coef(model_logit), Sigma = vcov(model_logit), Terms = 1:4) # p-value = 0.15 > 0.05 => not significant

model_probit <- glm(Y ~ ., data = bin, family = binomial("probit"))
summary(model_probit)
wald.test(b = coef(model_probit), Sigma = vcov(model_probit), Terms = 1:4) # p-value = 0.08 > 0.05 => not significant



# 5
cl <- read.csv(file = paste0(path, "Task5.csv"), header = T, dec = ".", sep = ",")
cl <- na.omit(cl)
rownames(cl) <- cl[, 1]
cl[, 1] <- NULL
cl_dist <- dist(cl, method = "euclidean")

# 5-1
elbow(cl)
cl_kmeans <- kmeans(cl_dist, 4)
fviz_cluster(cl_kmeans, data = cl, ellipse.type = "convex")

# 5-2
cl_hclust <- hclust(cl_dist)
plot(cl_hclust)
rect.hclust(cl_hclust, k = 4, border = 1:8)
