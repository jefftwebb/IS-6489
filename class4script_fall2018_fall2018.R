# Class 4 script
# Statistical inference and linear regression
# IS 6489

library(tidyverse)
library(arm) # Andrew Gelman's package

###################################################################
## PRACTICE:  bootstrap

# Steps
# 1. Initialize vector to store bootstrap distribution
# 2. Set up loop
# 3. Within each loop:
#    - take random sample of data set (with replacement)
#    - calculate test statistic with sample
# 4. Use bootstrap distribution to calculate SE and CIs.

# PROBLEM: use bootstrap to find CIs for the wt coefficient in the following
# regression model:  
# mpg ~ cyl + disp + hp + drat + wt + qsec

data(mtcars)

# Model
(model <- lm(mpg ~ cyl + hp + wt, data = mtcars))

#Extract coefficients
coef(model)

# Extract coeffcient for wt
coef(model)[4]

# Your code goes here

boot_dist <- NULL
for(i in 1:1000){
  new <- sample_frac(mtcars, 1, replace= T)
  mod <- lm(mpg ~ cyl + hp + wt, data = new)
  boot_dist[i] <- coef(mod)[4]
}

sd(boot_dist) # compare to
summary(lm(mpg ~ cyl + hp + wt, data = mtcars))
mean(boot_dist)
quantile(boot_dist, probs = c(.025, .975))
  

###################################################################
## EXPLORATION:  Explore caret package and knn regression +
## error metrics

library(caret)

# Train function in caret fits a large number of models.
?train

d <- read.csv("day.csv") # Get data (at Canvas in class 4 file)

# Linear model (train() is a wrapper for lm())
caret_lm <- train(cnt ~ temp,
                  method = "lm",
                  data = d)

caret_lm

# What does caret print to the screen? The cross-validation estimate of the 
# model's out of sample performance. More on that in later classes.

# We can get the model summary (and the actual in-sample model performance) 
# like this: 

summary(caret_lm) #which is identical to:
summary(lm(cnt ~ temp, data = d))

# It is easy to use a different model such as knn (same syntax).
# Must always center and scale for KNN.

caret_knn <- train(cnt ~ temp,
                  method = "knn",
                  preProcess = c("center", "scale"),
                  data = d)

caret_knn

summary(caret_knn) # Doesn't work:  there are no coefficients in a knn model!

# caret automatically finds the best value of k!
# Remember though:  the output caret prjts to the screen is an estimate
# of how the model will perform on new data.

###################################################################
# Calculating error metrics for knn model

# first we must get the predicted or fitted values from the model.
# We can use the fitted() or the predict() function to do that.
# In this instance the functions return the same values.

caret_knn %>%
  fitted %>%
  head

caret_knn %>%
  predict %>%
  head

all(fitted(caret_knn) == predict(caret_knn)) # same values

# Calculate error metrics.  Write functions for:

# Residuals:  actual - predicted

# Residual sum of square or RSS:  sum((actual - predicted)^2)

# Total sum of squares or TSS: sum((actual - average)^2)

# Mean squared error or MSE: mean((actual - predicted)^2)

# Root mean square error or RMSE: sqrt(mean((actual - predicted)^2))
# RMSE returns an error metric that is on the scale of the outcome.

# PROBLEM: using RMSE, which model fits the data better?
# KNN or OLS regression?

###################################################################
## PRACTICE: Explore lm(), model residuals and error metrics (RSS, RMSE)

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
# Multiple R squared penalized for the number of predictors.
# F-statistic: a test statistic indicating whether the fitted
#               model explains the data better than the null model 
#               (with just an intercept).

# Stripped down output (from arm package):
display(lm1)

# second regression
display(lm(cnt ~ temp + season, data = d)) # or
display(lm(cnt ~ temp + factor(season), data = d))

# DISCUSSION:  Should we factor season?

# The temperature variable is a bit of a problem in both models:
# the data has no zero values, hence the intercept in this model
# is not interpretable.  We need to center.
# We also need to rescale because it is not possible to increase
# temp by 1 unit:

range(d$temp)

?rescale #in arm package will center and scale (divide by 2 sds)

# What is centering? 
(x <- c(1, 2, 3, 4, 5))
mean(x)
(x - mean(x)) # The mean is now 0

# Example of centering and scaling
display(lm2 <- lm(cnt ~ rescale(temp) + factor(season), data = d)) #or

display(standardize(lm(cnt ~ rescale(temp) + factor(season), data = d)))

# We have rescaled temp by dividing by 2 sd and centered it.
# Rescaling allows us to judge relative effect sizes now because the 
# predictors are on comparable scales. The intercept is also interpretable
# as average ridership when temp is average (= 0 after centering)
# and season is at the reference level (1).

# Which model, lm1 or lm2, fits the data better?
# Calculate RMSE for both models

###################################################################
## PRACTICE:   Interpreting linear model output and 
# using the model to predict

display(lm2)

# (Intercept)  3837.65: expected ridership when season == 1 (reference
# category) and temp == 0 (is average for centered temp).

# rescale(temp) 2284.97:  expected change in ridership associated 
#                         with a 1 unit 2 sd increase in temp.
# factor(season)2 848.72: expected change in ridership associated 
#                         with a change in season from reference to 
#                         season 2.
# factor(season)3 490.2: expected change in ridership associated 
#                         with a change in season from reference to 
#                         season 3.
# factor(season)4 1342.87: expected change in ridership associated 
#                          with a change in season from reference to 
#                          season 4.


# Use the model to predict ridership
predict(lm2) %>% head #or
fitted(lm2) %>% head

# We can make a prediction for specific values of 
# the predictors:

display(lm2)

# predicted ridership for avg temp in season 1
3837.65 + 2284.97 * 0 

# predicted ridership for avg temp in season 2
3837.65 + 2284.97 * 0 + 848.72

# Predicted ridership for  temp of .45 in season 2
3837.65 + 2284.97 * .45 + 848.72

# model coefficients using coef(): (better--avoids rounding error)

# Data frame method (easiest):

###################################################################
## PRACTICE:   More linear model

# Refit the model with additional predictors. Can we improve model
# R square?

new_lm <- lm(cnt ~ temp + 
               factor(season) +
               hum +
               yr +
               workingday +
               holiday +
               factor(weathersit) +
               temp + 
               windspeed, data = d)

summary(arm::standardize(new_lm)) # improved fit!

#standardize is a function in the arm 
# package for rescaling inputs in a model


###################################################################
## PRACTICE: More bootstrap

# Bootstrapping is great for statistical communication.

# HARDER PROBLEM: You work in public policy research and you 
# would like to express the relationship between wt and mpg
# in simpler terms.  Your boss wants you to do the research
# to complete the following sentence:  "If, through improvements
# in materials, we were able to make heavy cars light, then 
# expected mpg would decline by ____, plus or minus ____."
# The first task is to define "heavy" and "light."  Let's 
# say that "light" is the 20th percentile of wt and heavy is the 
# 80th percentile. (This is equivalent to reducing car weight by
# 1500 pounds.)

# Find percentiles
quantile(mtcars$wt, probs = seq(0, 1, .1))

# Extract percentiles
quantile(mtcars$wt, probs = seq(0, 1, .1))[3]
quantile(mtcars$wt, probs = seq(0, 1, .1))[9]




