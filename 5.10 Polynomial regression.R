### Statistics and Predictive Analytics

# Tutorial topic:   Polynomial regression

library(tidyverse)
library(MASS)
library(arm)
library(caret)

data(Boston)
b <- Boston

# By "polynomial regression" I mean adding quadratic terms
# (or terms with larger than n = 1 degrees, such as cubic)
# to the model to capture non-linearity in the bivariate
# relationship between a predictor and an outcome.

# Start with a base model:

display(log_model <- lm(log(medv) ~ nox + 
                          rm + 
                          lstat + 
                          dis, 
                        data = b))

# Should we add a quadratic term for nox? Perhaps.

ggplot(b, aes(nox, log(medv))) +
  geom_point() + 
  geom_smooth()

# Fit the above log model with a quadratic term for nox: I(nox^2)

display(quad_model <- lm(log(medv) ~ nox + 
                           I(nox^2)  + 
                           rm + 
                           lstat + 
                           dis, 
                        data = b))

display(cube_model <- lm(log(medv) ~ nox + 
                           I(nox^2)  + 
                           I(nox^3)  + 
                           rm + 
                           lstat + 
                           dis, 
                         data = b))


# Does this addition improve the model?  R-square goes up slightly.

AIC(log_model)
AIC(quad_model)
AIC(cube_model)

# Inspect residual plots
plot(log_model, which = 1)
plot(quad_model, which = 1)
plot(cube_model, which = 1)

# Fit the above log model with a quadratic term for rm.  Why?

ggplot(b, aes(rm, log(medv))) +
  geom_point() + 
  geom_smooth()

display(new_quad_model <- lm(log(medv) ~ nox + 
                           I(nox^2) + 
                           rm + 
                           I(rm^2) +
                           lstat + 
                           dis, 
                         data = b))

# Does this addition improve the model? 

AIC(quad_model)
AIC(new_quad_model)

plot(new_quad_model, which = 1)

