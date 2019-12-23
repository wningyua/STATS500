# STATAS500HW6
# December 10, 2019

# Part 1

# libraries and packages
library(faraway)
data(fat)
library(leaps)
library(glmnet)
library(MASS)
library(dplyr)


summary(fat) # no mossing value and all values look fine

ind = seq(10, nrow(fat), 10) # index of testing set
test = fat[ind, -c(1, 3, 8)] # test dataset
train = fat[-ind, -c(1, 3, 8)] # training dataset
rmse <- function(x,y) sqrt(mean((x-y)^2))



# linear regression models
lm.full = lm(siri~., data = train) # full model
summary(lm.full)  

pred.full = predict(lm.full, test)

rmse(fitted(lm.full), train$siri) # 4.178651 for training 
rmse(pred.full, test$siri) # 4.395559 for testing 

# linear regression with variables selected using Mallows Cp
b = regsubsets(siri~. , data = train)
rs = summary(b)

plot(2: 9, rs$cp, xlab="No. of Parameters", ylab = "Cp")
abline(0, 1)
which.min(rs$cp) #7

lm.cp = lm(siri ~ age + weight + neck + abdom + 
             thigh + forearm + wrist, data = train )

summary(lm.cp)

pred.cp = predict(lm.cp, test)

rmse(fitted(lm.cp), train$siri) # 4.217687 for training 
rmse(pred.cp, test$siri) # 4.342456 for testing 


# linear regression with variables selected using adjusted R^2
plot(2: 9, rs$adjr2, xlab="No. of Parameters", ylab = "Adjusted R^2")
which.max(rs$adjr2) #8
lm.r2 = lm(siri ~ age + weight + neck + abdom + hip + thigh
           + forearm + wrist, data = train)

summary(lm.r2)

pred.r2 = predict(lm.r2, test)

rmse(fitted(lm.r2), train$siri) # 4.200863 for training 
rmse(pred.r2, test$siri) # 4.327248 for testing 

# ridge regression
rgmod = lm.ridge(siri ~ ., data = train, lambda = seq(0, 2, 1e-5))
select(rgmod) # automatic selection for lambda: 1.08701
which.min(rgmod$GCV) # 108702
rg.coef = coef(rgmod)[108702, ]

fit.rg = cbind(1, as.matrix(train[, -1])) %*% rg.coef

pred.rg = cbind(1, as.matrix(test[, -1])) %*% rg.coef


rmse(fit.rg, train$siri) # 4.183813 for training 
rmse(pred.rg, test$siri) # 4.282806 for testing 




#--------------------------------------------------------

# Part 2
data("pima") 

# clean data
pima = pima %>% filter( glucose > 0, diastolic > 0, bmi > 0) %>%
  select(test, pregnant, glucose, diastolic, bmi, diabetes, age)

# fit a binomial regression model 
logitm = glm(as.factor(test) ~., family = binomial(link = logit), data = pima)
summary(logitm)


# q1: can the deviance be used to test the goodness of fit here?
# Cannot, since ni = 1 which means only one observation in a trail 

# q2: what is the ratio of the odds of testing postive for two 
# women while fixed other predictors

# get the 1st and 3nd quantile of BMI
bmi_1 = 27.50 
bmi_3 = 36.60 
bmi_est = 0.090926
exp((bmi_3 - bmi_1) * bmi_est) #2.287425


# q3: do women who test postive for diabuets have higher diastolic blood pressure?
pima %>% group_by(as.factor(test)) %>% summarize( dbp = mean(diastolic))
# postive test women have a hgiher dbp than negative test results. 
# However, in the model, we see that diastolic is not statistical significant 
# in the logistical model with an estimate of -0.008916, which means that 
# the women with postive reuslts is less likely to have a higher dbp. 
# This may because that the other predictors in the model have a larger influence
# than dbp and we cannot judge the significant difference between two groups of 
# samples based on the mean values of dbp. 

# q4: predict the probability
pred = -8.962146 + 0.117863 + 0.035194 * 100 + (-0.008916) * 70 + 0.090926 * 25 + 0.960515 * 0.6 + 0.016944 * 30
ilogit(pred) # 0.06970537



