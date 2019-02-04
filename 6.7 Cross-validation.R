### Statistics and Predictive Analytics

# Tutorial topic: Cross-validation

library(tidyverse)
library(MASS)
library(arm)
library(caret)
rmse <- function(actual, predicted) sqrt(mean((actual - predicted)^2))

data("Pima.tr");data("Pima.te")
pima <- rbind(Pima.tr,Pima.te)
glimpse(pima)

### Validation set approach

# Split into test and train sets
# Randomly pick rows from the dataset
set.seed(327)
rows <- sample(nrow(pima), round(nrow(pima)*.7), replace = F)
rows
train <- pima[rows, ]
dim(train)
test <- pima[-rows, ]
dim(test)

head(train)

# Here is another method using caret's createDataPartition() function.
set.seed(327)
rows <- createDataPartition(pima$bp, times = 1,
                            p = .7, list = FALSE)

head(rows)

train <- pima[rows, ]
dim(train)
test <- pima[-rows, ]
dim(test)

# Fit a model on the train dataset that will be
# extremely accurate (low bias) but is ridicululous: 

display(low_bias <- lm(bp ~ glu * age * bmi * ped * 
                         type * npreg * skin, 
                       data = train)) 

#R2 is .49.  Killing it, we are geniuses

# Compare to a simple model:
display(low_variance <- lm(bp ~ ., data = train))
#R2 is only .22. 

# Calculate in-sample RMSE for low-bias and low_variance models.

rmse(train$bp, fitted(low_bias))
rmse(train$bp, fitted(low_variance))

# Calculate out of sample RMSE for low-bias and low_variance models
# using test set.

rmse(test$bp, predict(low_bias, newdata = test))
rmse(test$bp, predict(low_variance, newdata = test))

### K fold cross validation with Caret

# Use caret to perform cross-validation for you. Caret prints CV-estimated
# out of sample performance to the screen. 

train(bp ~ .,
      data = train,
      method = "lm")

# Estimated performance changes if we run it multiple times. Why?

# Change CV defaults in caret: 10-fold CV:

train(bp ~ .,
      data = train,
      method = "lm", 
      trControl = trainControl(
        method="cv", 
        number=10)
      )

# 10 fold repeated five times, the more repeats the greater stability in
# the estimates. We can define the trainControl object outside 
# of the train function.

myControl <- trainControl(
  method="repeatedcv", 
  number=10, 
  repeats = 5)

train(bp ~ .,
      data = train,
      method = "lm", 
      trControl = myControl
      )

