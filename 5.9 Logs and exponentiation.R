### Statistics and Predictive Analytics

# Tutorial topic:   Working with logs and exponentiation

library(tidyverse)
library(MASS)
library(arm)
library(caret)

data(Boston)
b <- Boston

# Should we transform the outcome variable? Yes.

# Reason #1. Skewed data
hist(b$medv)
hist(log(b$medv))

# Reason #2. Explanatory power goes up.
display(lm(medv ~., b))
display(lm(log(medv) ~., b))

# Reason #3. residual plots look better.
plot(lm(medv ~., b), which = 1)
plot(lm(log(medv) ~., b), which = 1)

hist(residuals(lm(medv ~., b)))

# Interpret the coefficients in this model on the log scale.

display(log_model <- lm(log(medv) ~ nox + rm + lstat + dis, 
                        data = b))

# Interpret the coefficients in this model on the original scale.
# Remember: exp(beta) is the percentage change in y associated with a 
# 1 unit increase in x, compared to the baseline of 1.

# nox: -.71

1 - exp(-.71) # 1 unit increase in nox associated with, on average,
# a 51% decline in median home value, holding the other predictors constant.

# rm: .12

exp(.12) - 1 # 1 unit increase in rm associated with, on average,
# a 13% increase in median home value, holding the other predictors constant.

# The fitted values from this model are expressed in log(y) units.
# How would you put the fitted values back on the scale of the outcome?
# For example, perhaps you want to calculate RMSE.
# One idea would be to exponentiate the fitted values.  

# RMSE(exp(fitted(log_model)), b$medv)

# THIS IS WRONG. Why?
# exp(fitted values) will systematically under estimate the actual fitted
# values so we must make an adjustment with something called Duan's 
# smearing estimate, which is defined as mean(exp(residuals)).  
# So, here is the code for the re-transformation:

# exp(fitted(log_model)) * mean(exp(resid(log_model)))

# Now, compare:

(exp(fitted(log_model)) * mean(exp(resid(log_model)))) %>% head

exp(fitted(log_model)) %>% head

# And now we can calculate the RMSE correctly
RMSE((exp(fitted(log_model)) * mean(exp(resid(log_model)))), 
     b$medv)

# Would we want to log transform lstat?  How would we answer that question?

hist(b$lstat)
hist(log(b$lstat))

ggplot(b, aes(lstat, medv)) +
  geom_point() +
  geom_smooth()

ggplot(b, aes(lstat, log(medv))) +
  geom_point() +
  geom_smooth()

ggplot(b, aes(log(lstat), log(medv))) +
  geom_point() +
  geom_smooth()

# Is the log-log model better?

display(lm(log(medv) ~ nox + 
             rm + 
             lstat + 
             dis, 
           data = b)) # R2 = .69

display(lm(log(medv) ~ nox + 
             rm + 
             log(lstat) + 
             dis, 
           data = b)) # R2 = .71

# log(lstat): -.47.  A 1 percent increase in lstat is associated with, on average,
# a -.47% change in medv, holding the other predictors constant.