### Statistics and Predictive Analytics

# Tutorial topic:  Caret package

library(tidyverse)
library(arm) # Andrew Gelman's package

# Explore caret package and knn regression + error metrics

library(caret)

# Train function in caret fits a large number of models.
?train

d <- read.csv("day.csv") # Get data at Canvas

# Linear model. train() is a wrapper for lm()

caret_lm <- train(cnt ~ temp,
                  method = "lm",
                  data = d)

caret_lm

# What does caret print to the screen? The cross-validation estimate of the 
# model's out of sample performance. 

# We can get the model summary (and the actual in-sample model performance) 
# like this: 

summary(caret_lm) #which is identical to:
summary(lm(cnt ~ temp, data = d))

# It is easy in caret to use a different model such as knn (same syntax).
# Must always center and scale for KNN.

caret_knn <- train(cnt ~ temp,
                  method = "knn",
                  preProcess = c("center", "scale"),
                  data = d)

caret_knn

summary(caret_knn) # Doesn't work:  there are no coefficients in a knn model!

# caret automatically finds the best value of k!
# Remember though:  the output caret prints to the screen is the
# cross-validation estimate of how the model will perform on new data.

