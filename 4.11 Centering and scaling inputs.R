### Statistics and Predictive Analytics

# Tutorial topic:  Centering and scaling inputs

library(tidyverse)
library(arm) 

d <- read.csv("day.csv")

?rescale #in arm package will center and scale (divide by 2 sds)
?standardize

# What is centering? 
(x <- c(1, 2, 3, 4, 5))
mean(x)
(x - mean(x)) # The mean is now 0

# Example of centering and scaling with rescale()
display(lm2 <- lm(cnt ~ rescale(temp) + 
                    factor(season), 
                  data = d)) #or

anova(lm2)

range(rescale(d$temp))

# standardize()
display(standardize(lm(cnt ~ temp + factor(season), 
                       data = d),
                    binary.inputs = "leave.alone"))

summary(train(cnt ~ temp + factor(season),
              method = "lm",
              data = d,
              preProcess = c("center","scale")))

# We have rescaled temp by dividing by 2 sd and centered it.
# Rescaling allows us to judge relative effect sizes now because the 
# predictors are on comparable scales. The intercept is also interpretable
# as average ridership when temp is average (= 0 after centering)
# and season is at the reference level (1).

# Does the model with season fit the data better than a model with just
# temp?

display(lm2)
display(lm1 <- lm(cnt ~ rescale(temp), 
                  data = d))

# Calculate RMSE for both models

rmse <- function(actual, predicted) sqrt(mean((actual-predicted)^2))

rmse(d$cnt, fitted(lm1))
rmse(d$cnt, fitted(lm2))

# Fit full model
lm_full <- lm(cnt ~ factor(season) +
                yr +
                factor(mnth) +
                holiday +
                factor(weekday) +
                workingday + 
                factor(weathersit) +
                temp +
                hum +
                windspeed,
              data=d)

RMSE(d$cnt, fitted(lm_full))



