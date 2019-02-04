### Statistics and Predictive Analytics

# Tutorial topic:  Calculating error metrics

library(tidyverse)
library(caret)

# Data

d <- read.csv("day.csv") # Get data

# Linear regression model 
caret_lm <- train(cnt ~ temp,
                  method = "lm",
                  data = d)

# KNN regression model
caret_knn <- train(cnt ~ temp,
                  method = "knn",
                  preProcess = c("center", "scale"),
                  data = d)


# Fitted values

# First we must get the predicted or fitted values from the models.
# We can use the fitted() or the predict() function to do that.
# In this instance the functions return the same values.

# KNN regression
caret_knn %>%
  fitted %>%
  head

caret_knn %>%
  predict %>%
  head

all(fitted(caret_knn) == predict(caret_knn)) # same values

# Linear regression
caret_lm %>%
  fitted %>%
  head

caret_lm %>%
  predict %>%
  head

# Calculate error metrics.  Write functions for:

# Residuals:  actual - predicted

resid <- function(actual, fitted){
  actual - fitted
}

resid(actual = d$cnt, fitted = fitted(caret_knn))
  
# Residual sum of squares or RSS:  sum((actual - predicted)^2)

RSS <- function(actual, fitted){ 
  sum((actual - fitted)^2)
}

RSS(d$cnt, fitted(caret_knn))

# Total sum of squares or TSS: sum((actual - average)^2)

TSS <- function(actual, average) sum((actual - average)^2)

TSS(d$cnt, mean(d$cnt))

# Mean squared error or MSE: mean((actual - fitted)^2)

MSE <- function(actual, fitted) mean((actual - fitted)^2)
MSE(d$cnt, fitted(caret_knn))

# Root mean square error or RMSE: sqrt(mean((actual - fitted)^2))
# RMSE returns an error metric that is on the scale of the outcome.

RMSE <- function(actual, fitted) sqrt(mean((actual - fitted)^2))
RMSE(d$cnt, fitted(caret_knn))


# PROBLEM: using RMSE, which model fits the data better?
# KNN or OLS regression?

RMSE(d$cnt, fitted(caret_knn))
RMSE(d$cnt, fitted(caret_lm))

