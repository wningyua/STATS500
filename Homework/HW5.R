##STATS500-HW5
## Author: Ningyuan Wang
## Date: November 12

## Question 1
library(faraway)
library(quantreg)
library(MASS)
library(leaps)

## OLS
g = lm(total ~ ratio + salary + expend, data = sat)
summary(g) # none of predictor is significant


## LAD
glad = rq(total ~ ratio + salary + expend, data = sat)
summary(glad)

## Huber's method
ghuber = rlm(total ~ takers + ratio + salary + expend, data = sat)
summary(ghuber)


## compare the results
coef(g)
coef(glad)
coef(ghuber)


## Question 2
# check the data and build the model
boxplot(prostate$lpsa)
full = lm(lpsa ~., data = prostate)
summary(full)

# backward elimination
lm = update(full, . ~ . - gleason) # remove gleason
summary(lm)

lm = update(lm, . ~ . - lcp) # remove lcp
summary(lm)

lm = update(lm, . ~ . - pgg45) # remove pgg45
summary(lm)

lm = update(lm, . ~ . - age)  #remove age
summary(lm)

lm = update(lm, . ~ . - lbph) #remove lbph 
summary(lm) # the model with the predicotrs: lcavol, lweight, svi, all significant
anova(lm, full) # not significant

# adjusted r^2
b = regsubsets(lpsa ~., data = prostate)
summary(b)

# plot adjusted R2 against p + 1, and find the model with largest R2
rs = summary(b)
plot(2:9, rs$adjr2, xlab = "Number of Parameters",
     ylab = "Adjusted Rsq")
which.max(rs$adjr2) # the model with lcavol, lweight, age, lbph, svi, lcp, and pgg45
mod = lm(lpsa ~ lcavol + lweight + age + lbph + svi + lcp + pgg45, data = prostate)
summary(mod)
anova(mod, full) # not significant with reduced model, R^2 = 0.6273


# Mallows' Cp
which.min(rs$cp)
plot(2:9, rs$cp, xlab = "Number of Parameters",
     ylab = "Cp")
abline(0 ,1)
m = lm(lpsa ~ lcavol + lweight + lbph + svi, data = prostate)
summary(m)
anova(m, full)









