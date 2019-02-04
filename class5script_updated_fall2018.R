# Class 5 script
# Linear regression topics
# IS 6489

library(tidyverse)
library(MASS)
library(arm)

data(Boston)
b <- Boston

###################################################################

## PRACTICE: Exploring data with linear regression

# Often in descriptive/inferential analysis we are interested in testing the 
# effect of one variable on the outcome, after controlling for possibly
# confounding variables.

# Consider an experimental situation.  We want to test the effect of a 
# binary treatment variable (treatment/control) on the outcome.

# In Lab 2 we were interested in whether nox is associated with 
# median home values.  But, a puzzle:

# The effect of nox is HUGELY dependent on the presence of other variables.
# 3 models:

display(lm(medv ~ nox + rm , b)) # nox is predictive

# Side note:  the scaling of nox is wacky, which produces a 
# huge regression coefficient.
summary(b)

display(lm(medv ~ nox + rm + lstat, b)) # the effect goes away

display(lm(medv ~ nox + rm + lstat + dis, b)) # the effect comes back

# Which effects are real?  They all are.  Use the information to understand
# relationships in your data, which can be confusing.

# Model 1.  nox predicts housing price differences among tracts with the 
# same average rooms.

# Model 2.  nox does not predict housing price differences among tracts with the 
# same average rooms and the same socioeconomic level. This is likely 
# because nox is correlated with socioeconomic level and so it has no 
# explanatory work to do after lstat is in the model.

# Model 3.  nox predict housing price differences among tracts with the 
# same average rooms, socioeconomic level and dis. dis introduces explanatory
# differences in nox across tracts with same rooms and socioeconomic level.

###################################################################

## PRACTICE: centering and scaling

lm(medv ~ nox + rm + lstat +dis + chas, data = b)

# Differences in scaling! (same point as above)
summary(b)

# Center and scale nox using rescale() and scale()
# example
b$nox %>% head
rescale(b$nox) %>% head
scale(b$nox) %>% head

# Now center and scale nox in the model

lm(medv ~ rescale(nox) + 
     rescale(rm) + 
     rescale(lstat) + 
     rescale(dis) + 
     chas, data = b)


# Center and scale the entire model using standardize().

standardize(lm(medv ~ nox + rm + lstat +dis + chas, data = b))

# Compare to standardize(binary.inputs = "leave.alone") 

standardize(lm(medv ~ nox + rm + lstat +dis + chas, data = b),
            binary.inputs = "leave.alone")

# What does the intercept represent in a centered model?

# The value of medv when all the predictors are average or 
# at the reference category.

# Now use caret to center and scale the same model.
library(caret)

train(medv ~ nox + rm + lstat + dis + chas,
      data = b,
      preProcess= c("center", "scale"),
      method = "lm") %>%
  summary

###################################################################

## PRACTICE:  working with logs and exponentiation

# Interpret the coefficients in this model.
# Remember: beta is the percentage change in y associated with a 
# 1 unit increase in x.

display(log_model <- lm(log(medv) ~ nox + 
                          rm + 
                          lstat + 
                          dis, data = b))

(1 - exp(-.71))*100 
# 1 unit increase in nox is associated with a 51% decrease in medv

exp(.12)
# 1 unit increase in nox is associated with a 13% increase in medv

exp(-.04)
# 1 unit increase in nox is associated with a 4% decrease in medv


# Now find the fitted values from this model.  How would you put the fitted values 
# back on the scale of the outcome?

# exponentiate!

fitted(log_model) %>% exp %>% head

# Would we want to log transform lstat?  How would we answer that question?

hist(b$lstat) # looks skewed

display(lm(log(medv) ~ nox + rm + log(lstat) + dis, data = b))
# R-squared is improved with log(lstat) 

# Interpret the coefficient for log(lstat).

# An increase of 1% in lstat is associated with .47% decrease in
# medv

###################################################################

## PRACTICE:  working with polynomial terms

display(lm(medv ~ nox, data = b))

# Fit the above log model with a quadratic term for nox: I(nox^2)

head(b$nox)
head(b$nox) * head(b$nox) # ^2 is just vecgtor multiplication

display(lm(medv ~ nox + I(nox^2), data = b))

# Does this addition improve the model?  

# Yes. R-square improves.

# Fit the above log model with a quadratic term for rm.

display(lm(medv ~ nox + I(nox^2) + rm + I(rm^2), data = b))

# Does this addition improve the model? 

# Yes. R-square improves.

# Illustrate the relationship

# There are probably better ways for doing this...
# We add the fitted values to the data set.
# Plots like this are illustrations of underlying statistical results.

b$fitted <- fitted(lm(medv ~ rm + I(rm^2), b))

ggplot(b, aes(rm, medv)) +
  geom_point() +
  geom_line(aes(rm, fitted), col = 2)


###################################################################

## PRACTICE:  Interactions

# Does the effect of nox on housing price vary with proximity to the charles?
# Use nox, rm, lstat, dis and chas and center/scale variables.

standardize(int_model <- lm(medv ~ rm + 
                              lstat + 
                              dis + 
                              chas*nox, data = b))

# Illustrate this result with a plot.

# 1. plot nox vs medv

ggplot(b, aes(nox, medv)) +
  geom_point() +
  stat_smooth(method = "lm", se = F)

# 2. Color points and regression lines by chas

ggplot(b, aes(nox, medv)) +
  geom_point() +
  stat_smooth(method = "lm", se = F) +
  facet_wrap(~chas)

ggplot(b, aes(nox, medv, col = factor(chas))) +
  geom_point() +
  stat_smooth(method = "lm", se = F)


# Interpret regression output in light of the plot.

# The problem is that this plot, because it does not reflect
# the multivariate result, is misleading.  The interaction
# coefficient in the above model is negative, which means
# that an increase in chas (from 0 to 1) should result in a
# more negative regression line for nox.  But this plot shows
# the opposite. That is because the regression with just chas and
# nox produces a positive interaction term:

standardize(lm(medv ~ chas*nox, data = b))

# Creating a plot that reflects the result from the earlier 
# regression requires ssome extra steps.

# First, use the model to predict values for chas = 1 and
# chas = 0.

# Create a prediction data frame that manipultes the 2 
# interacted variables:

pred_df <- data.frame(chas = c(0,1),
                      rm = mean(b$rm),
                      nox = b$nox,
                      lstat = mean(b$lstat),
                      dis = mean(b$dis))

# Use this frame to generate predictions
pred_df$int_pred <- predict(int_model, newdata = pred_df)

# Plot
ggplot(b, aes(nox, medv)) +
  geom_point() +
  geom_line(data = pred_df, aes(nox, int_pred, col = factor(chas)))

# This plot now shows what the regression says:  when chas increases
# by 1 (from 0 to 1) the the slope of nox becomes more negative.
# As noted in class, the interpretation is that pollution 
# has a bigger negative impact on prices of houses near the river
# than it does on houses away from the river.
