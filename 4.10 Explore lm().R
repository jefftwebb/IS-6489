### Statistics and Predictive Analytics

# Tutorial topic:  Explore lm()

library(tidyverse)
library(arm) 

d <- read.csv("day.csv")

# Fit 2 models:
# 1. Model of ridership using temp as predictor
# 2. Model of ridership and temp and season as predictors

# First regression

summary(lm1 <- lm(cnt ~ temp, data = d))
# We can name the model object within the summary function

# Intercept:  Expected ridership when temp is 0.
# Temp:  2 interpretive possibilities
# 1. Expected ridership with a  1 unit increase in temp (counterfactual
#    interpretation)
# 2. Expected difference in ridership comparing 2 groups that differ
#    by one unit in temperature (predictive interpretation)
# Std. Error:  estimation uncertainty; 1.96 sd of sampling dist.
# t value:  coefficient divided by SE
# Pr(>|t|): the p-value, the proportion of values in the null distribution 
#           that are equal to or more extreme than the observed coefficient.
# Asterisks: unecessary information.
# Residual SE:  standard deviation of the residuals (lower is better)
# Degrees of freedom:  n - p - 1: 731 - 1 - 1 = 729
# Multiple R squared:  residual sum of squares/ total sum of squares.
# Adjusted R squared penalized for the number of predictors.
# F-statistic: a test statistic indicating whether the fitted
#               model explains the data better than the null model 
#               (with just an intercept).

# Stripped down output (from arm package):
display(lm1)

# second regression
display(lm(cnt ~ temp + season, data = d)) # or
display(lm(cnt ~ temp + factor(season), data = d))
display(lm(cnt ~ temp + as.character(season), data = d))

# Should we factor season?

# The temperature variable is a bit of a problem in both models:
# the data has no zero values, hence the intercept in this model
# is not interpretable.  We need to center.
# We also need to rescale because it is not possible to increase
# temp by 1 unit:

range(d$temp)
