# STATS500 HW3
# Author: Ningyuan Wang
# Date: Oct 11, 2019

# Read in data
library(faraway)
data(sat)

# Part A 
# 1.
lm1 = lm(total ~ takers + ratio + salary, data=sat)
summary(lm1)
# Since the r-squared for the model is .8124, it means that 81.24 % of 
# variance in response can be explained by the predictors, whcih indicates
# that the model is a good fit. 

# 2.
# Ho: Beta_salary = 0
# Ha: Beta_salary > 0
# test-statistics: 2.541
# p-value： 0.0145/2 = 0.00725, since we are looking for one-side only 
# p-value is less than alpha. Reject Ho. 
# conclusion: At alpha = .01, there is signifcant evidennce to state that 
# teachers' salary has a postive effect on the SAT scores. 

# 3. 
# Ho: Beta_ratio = 0
# Ha: Beta_ratio not equal to 0
# test-statistics: -2.187
# p-value: 0.0339
# p-value is bigger than alpha, do not reject Ho.
# conclusion: At alpha = .01, there is no significant evidence to state that 
# ratio has an effect on the SAT scores in the full model.

# 4.
# Ho: Beta_takers = Beta_ratio = Beta_salary = 0
# Ha: at least one beta is not eqal zero.
# test-statistics: 71.72
# p-value: < 2.2e-16
# p-value is smaller than alpha, reject Ho.
# conclusion: At alpha = .01, there is significant evidence to state that 
# at least one of three predictor has an effect on the SAT score.
# The null hypothesis means none of three predictors has an effect on the SAT score.

# 5.
confint(lm1, 'salary') #95%CI = (0.5304797, 4.5744605)
confint(lm1, 'salary', level = 0.99) #99%CI = (-0.146684 5.251624)
# With 95% confidence, we conclude that the true parameter of predictor salary falls between
# 0.53 and 4.57. 
# With 99% confidence, we conclude that the true parameter of predictor salary falls between 
# -0.15 and 5.25.

# 6. 需要和同学核查
library(ellipse)
# plot the confidence region 
plot(ellipse(lm1, c('ratio', 'salary')), type = "l", xlim = c(-1,0), main = "95% Joint Confidence Region")
# add the estimate to the plot
points(lm1$coefficients['ratio'], lm1$coefficients['salary'], pch = 18)
# add the origin to the plot
points(0, 0, pch = 1 )
# add the confidence interval for ratio
abline(v = confint(lm1, 'ratio'), lty = 2)
# add the confidence interval for salary
abline(h = confint(lm1, 'salary'), lty = 2)
# Ho: beta_ratio = beta_salary = 0
# Ha: at least one of predictor has an effect on SAT score. 
# Since the origin point is not in the confidence region, we conclude that at least one predictor has 
# an effect on SAT score. 

# 7. 
lm2 = lm(total ~ takers + ratio + salary + expend, data=sat)
summary(lm2)
# since r-squared value is less than the previous model. Adding the new variable expend does not 
# improve the goodness of fit. 

# 8.
lm0 = lm(total ~ takers, data=sat)
anova(lm0, lm2)
# Ho: beta_salary = beta_expend = beta_ratio = 0
# Ha: Ho is not true. 
# test-statistics: 3.2133
# p-value: 0.03165
# At alpha = 0.01, p-value > alpha, do not reject Ho. We conclude that there is no above predictor has an 
# effect on the response. 


# 以上已完成------------------------------------------------------------------------------
# Part B
# check the relationships between variables
pairs(~gamble + sex + status + income + verbal, data = teengamb)

# fit the model
lm3 = lm(gamble ~ factor(sex) + status + income, data = teengamb)
summary(lm3)

# check the constant variance assumption
#plot residuals vs fitted values
plot(lm3$fitted.values, lm3$residuals, xlab = "Fitted", ylab = "Residuals")
abline(h=0)
# The constant vairance assumption failed, because there is an obvious 
# pattern between the fitted values and residuals. 

#transform: take the square root of the response
lm4 = lm(sqrt(gamble) ~ factor(sex) + status + income, data = teengamb)
plot(lm4$fitted.values, lm4$residuals, xlab = "Fitted", ylab = "Residuals")
abline(h=0) #the constant variance is fine

# check the normality assumption
qqnorm(lm4$residuals, ylab = "Residuals")
qqline(lm4$residuals) #okay

# check for high leverage points
# half normal 和 studendized residuals 都是用于checking high levelrage points吗？ 如何分别看一块看
# 不确定是否是leverage point. check for large levelrage points: The points of 42 and 35 are considered as leverage points 
halfnorm(lm.influence(lm4)$hat, nlab = 2, ylab = "Leverages")
teengamb[c(42, 35), ]

# compute studentized residuals: identical,这里不需要吧？这样看起来没有high leverage point
lm.s <- summary(lm4)
sigma.s <- lm.s$sig
hat.s <- lm.influence(lm4)$hat
stud.res <- lm4$residuals / (sigma.s * sqrt(1-hat.s))
plot(stud.res, lm4$residuals, xlab="Studentized residuals",
     ylab="Raw residuals")


# check for outliers: p-value >  alpha/n, do not reject Ho. 
# No outlier
# compute externally studentized residuals
ti = rstudent(lm4)
max_ti = max(abs(ti)) #3.037005
which(ti == max_ti) #24
# compute p-value
2 * (1-pt((max_ti), df = 47 - 5 - 1)) #0.00414277
# compare to alpha / n
0.05 / 50


# check for influential points: no outstanding change in estimate of beta, 
# so we conclude no influential point
# compute Cook's distance
cook = cooks.distance(lm4)
halfnorm(cook, nlab = 3, ylab = "Cook's Distance")
teengamb[c(5, 39, 24), ]
# fit the model without the point with max cook， no big change in coefficients, we conclude 
# as no influential point in the data
lm4.24 = lm(sqrt(gamble) ~ factor(sex) + status + income,
            data = teengamb, subset = (cook < max(cook)) )
max(cook)
summary(lm4)
summary(lm4.24)
# compute changes in coefficients： sex, verbal, not a lot of change in 
# estimates, so we conclude that no influential point 
lm4.inf = lm.influence(lm4)
plot(lm4.inf$coef[,2], lm4.inf$coef[,3], 
     xlab = "Change in beta_sex",
     ylab = "Change in beta_status")
