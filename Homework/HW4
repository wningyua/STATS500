# STATS500 HW4

# Read in the data
library(faraway)
data("longley")
summary(longley)

#fit the model
lm1 = lm(Employed ~ GNP.deflator + GNP + Unemployed + Armed.Forces + Population + Year, 
         data = longley )
summary(lm1)

# compute the condition numbers 
X = model.matrix(lm1)[, -1]
e = eigen(t(X) %*% X)
round(sqrt(e$values[1]/e$values), 3)


# compute correlations between the predictors
round(cor(longley)[1:6, 1:6], 2)


# compute variance inflation factor 
round(vif(X), 3)

# fit a reduced model
lm2 = lm(Employed ~ Armed.Forces +  Unemployed + Year, data = longley)
summary(lm2)
anova(lm2, lm1) # the reduced model is good now

