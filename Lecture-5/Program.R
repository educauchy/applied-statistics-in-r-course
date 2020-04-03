Sys.setlocale(category = "LC_ALL", locale = "UTF-8")
path <- "~/Yandex.Disk.localized/Университет/Магистратура/2semester/R/Lecture-5/Datasets/"

library(aod)
library(InformationValue)

# AUC ROC - area under curve receiver operating characteristic

# DOCTOR 
doctor <- read.csv(file = paste0(path, "doctor.csv"), header = T, col.names = c("pregnancy", "x1", "x2", "x3", "x4", "x5"), dec = ",", sep = ";")
doctor$pregnancy <- factor(doctor$pregnancy)

# logit
# 1 construct logit model
model_logit <- glm(pregnancy ~ ., data = doctor, family = binomial("logit"))
summary(model_logit)

# 2 Wald test - checking if coefficients are significant (if adding coefficient causing improving the model)
wald.test(b = coef(model_logit), Sigma = vcov(model_logit), Terms = 1:6) # p-value = 0.038 < 0.05
# could be misleading result, because test is asymptotical and requires large sample
# interpratation of test: if vector of coefficients is = 0

# predicted values by model
predicted <- plogis(predict(model_logit, doctor))

# 3 optimal cutoff - not always 0.5 / 0.5 is good
logit_opt_cut <- optimalCutoff(doctor$pregnancy, predicted)
logit_opt_cut
# optimal cutoff: 0.49 / 0.51

# 4 confusion matrix using optimal cutoff
InformationValue::confusionMatrix(doctor$pregnancy, predicted, threshold = logit_opt_cut)

# 5 ROC curve
plotROC(doctor$pregnancy, predicted)



# probit
# 1 construct probit model
model_probit <- glm(pregnancy ~ ., data = doctor, family = binomial("probit"))
summary(model_probit)

# 2 Wald test - checking if coefficients are significant (if adding coefficient causing improving the model)
wald.test(b = coef(model_probit), Sigma = vcov(model_probit), Terms = 1:6) # p-value = 0.01 < 0.05
# could be misleading result, because test is asymptotical and requires large sample
# interpratation of test: if vector of coefficients is = 0

# predicted values by model
predicted <- plogis(predict(model_probit, doctor))

# 3 optimal cutoff - not always 0.5 / 0.5 is good
probit_opt_cut <- optimalCutoff(doctor$pregnancy, predicted)
probit_opt_cut
# optimal cutoff: 0.5 / 0.5

# 4 confusion matrix using optimal cutoff
InformationValue::confusionMatrix(doctor$pregnancy, predicted, threshold = probit_opt_cut)

# 5 ROC curve
plotROC(doctor$pregnancy, predicted)







# BINARY REGRESSION
br <- read.csv(file = paste0(path, "binary_regression.csv"), header = F, col.names = c("sleep", "study", "result"), dec = ".", sep = ";")
br$result <- factor(br$result)

# logit
# 1 construct logit model
model_logit <- glm(result ~ ., data = br, family = binomial("logit"))
summary(model_logit)

# 2 Wald test - checking if coefficients are significant (if adding coefficient causing improving the model)
wald.test(b = coef(model_logit), Sigma = vcov(model_logit), Terms = 1:3) # p-value = 0.0008 < 0.05
# could be misleading result, because test is asymptotical and requires large sample
# interpratation of test: if vector of coefficients is = 0

# predicted values by model
predicted <- plogis(predict(model_logit, br))

# 3 optimal cutoff - not always 0.5 / 0.5 is good
logit_opt_cut <- optimalCutoff(br$result, predicted)
logit_opt_cut
# optimal cutoff: 0.64 / 0.36

# 4 confusion matrix using optimal cutoff
InformationValue::confusionMatrix(br$result, predicted, threshold = logit_opt_cut)

# 5 ROC curve
plotROC(br$result, predicted)



# probit
# 1 construct probit model
model_probit <- glm(result ~ ., data = br, family = binomial("probit"))
summary(model_probit)

# 2 Wald test - checking if coefficients are significant (if adding coefficient causing improving the model)
wald.test(b = coef(model_probit), Sigma = vcov(model_logit), Terms = 1:3) # p-value = 0.14 > 0.05
# could be misleading result, because test is asymptotical and requires large sample
# interpratation of test: if vector of coefficients is = 0

# predicted values by model
predicted <- plogis(predict(model_probit, br))

# 3 optimal cutoff - not always 0.5 / 0.5 is good
probit_opt_cut <- optimalCutoff(br$result, predicted)
probit_opt_cut
# optimal cutoff: 0.6 / 0.4

# 4 confusion matrix using optimal cutoff
InformationValue::confusionMatrix(br$result, predicted, threshold = probit_opt_cut)

# 5 ROC curve
plotROC(br$result, predicted)
