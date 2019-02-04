### Statistics and Predictive Analytics

# Tutorial topic: Residual analysis

library(tidyverse)
library(MASS)
library(arm)
library(caret)

data(Boston)
b <- Boston

# residual plot:  fitted (x) vs. residuals (y)

base <- lm(medv ~ lstat, b)
display(base)

plot(base, which = 1)

# ouch!

# How do we improve this model?

# The original paper used all the variables, plus
# - rm^2
# - log(dis)
# - log(rad)
# - log(lstat)
# - log(medv)

# Plot the residuals from an improved model,
# but with no transformations.

plot(full_model1 <- lm(log(medv) ~ ., b), which = 1)

full_model2 <- update(full_model1, . ~ . 
                      + I(rm^2) 
                      - dis
                      + log(dis)
                      - rad
                      + log(rad)
                      - lstat
                      +log(lstat))

plot(full_model2, which = 1)
