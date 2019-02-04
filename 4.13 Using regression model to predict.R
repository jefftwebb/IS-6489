### Statistics and Predictive Analytics

# Tutorial topic:  Using a regression model to predict (confidence intervals)

# Packages, data
library(tidyverse)
library(arm)
d <- read.csv("day.csv")

# Model
display(lm2 <- lm(cnt ~ hum + holiday + temp, data = d))


# We've seen how we can make  a prediction for specific values 
# of the predictors, the easiest and most powerful approach
# being the "dataframe method."

# Let's say that we would like to predict for specific values
# of the inputs.  Here is an example:

data.frame(hum = mean(d$hum),
           holiday = 0,
           temp = mean(d$temp))

predict(lm2, newdata = data.frame(hum = mean(d$hum),
                                  holiday = 0,
                                  temp = mean(d$temp)))

# The predict() function is quite powerful and can produce not
# just a point estimate but an interval:

?predict.lm

predict(lm2, newdata = data.frame(hum = mean(d$hum),
                                  holiday = 0,
                                  temp = mean(d$temp)), 
        interval = "confidence")

# Why would we want a confidence interval?  It is usually better to 
# report an interval so that the point estimate does not lead to 
# a false sense of confidence.

# We can use this method to replace reporting coefficients, which most
# people won't understand.  Instead, to improve statistical communication
# we use our model to produce what are called "first differences"--the expected 
# change in predictions if we manipulate one of the predictors from a low to a 
# high level, with "low" and "high" being values that are interesting relative 
# to the business context.  

# For example:  Let's say we would like to know how bike sharing will be impacted 
# by changes in temperature in May.  Here is the base model:

d$mnth <- factor(d$mnth)

display(lm3 <- lm(cnt ~ hum + 
                    windspeed + 
                    temp + 
                    mnth, 
                  data = d))

# How does temp vary in May?

subset(d, mnth==5)$temp %>%
  range

# So rather than reporting a coefficient, we can much more easily explain the
# model results by saying:  "The model predicts that if we increase temperature
# from the minimum to the maximum in May ridership will change by
# x, at average windspeed and humidity."

# Note that to use the dataframe method the column titles
# must be the same as the coefficient names.

data.frame(hum = mean(d$hum),
           windspeed = mean(d$windspeed),
           mnth = factor(5),
           temp = c(.41, .78))

predict(lm3, newdata = data.frame(hum = mean(d$hum),
                                  windspeed = mean(d$windspeed),
                                  mnth = factor(5),
                                  temp = c(.41, .78)))

# So the change in ridership due to temp is expected to be:

6916 - 4228

# It would be even better if we could place an interval on this difference:

predict(lm3, newdata = data.frame(hum = mean(d$hum),
                                  windspeed = mean(d$windspeed),
                                  mnth = factor(5),
                                  temp = .78),
        interval = "confidence") - 

  predict(lm3, newdata = data.frame(hum = mean(d$hum),
                                    windspeed = mean(d$windspeed),
                                    mnth = factor(5),
                                    temp = .41),
          interval = "confidence")

# The expected ridership increase if temperature increases 
# from the minimum to maximum in May, at average humidity
# and windspeed, will be 2687 +/- about 10 riders: [2679, 2696].

# For most people this is a more intuitive quantity that a coefficient.

# You'll notice that one of the options for an interval in predict() is
# interval = "prediction".  This option includes not just estimation uncertainty
# but fundamental uncertainty, and consequently offers a far more conservative 
# estimate.  Compare:

predict(lm3, newdata = data.frame(hum = mean(d$hum),
                                  windspeed = mean(d$windspeed),
                                  mnth = factor(5),
                                  temp = mean(d$temp)),
        interval = "confidence") # interval is about 700

predict(lm3, newdata = data.frame(hum = mean(d$hum),
                                  windspeed = mean(d$windspeed),
                                  mnth = factor(5),
                                  temp = mean(d$temp)),
        interval = "prediction") # interval is about 5000!

