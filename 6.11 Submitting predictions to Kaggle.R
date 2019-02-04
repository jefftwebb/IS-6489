### Statistics and Predictive Analytics

# Tutorial topic: Submitting predictions to Kaggle

library(tidyverse)
library(caret)

# You are working with two files from Kaggle: test.csv and train.csv.

# You will have created a model using the train dataset, which 
# includes the outcome, SalePrice.  This is a supervised learning
# problem--the outcome is doing the supervising--whose 
# "solution" is a model that relates the outcome to your predictors.
# This model can then be used to predict housing prices in situations
# where actual housing price is not known, as in the test dataset.

train <- read.csv("train.csv")
glimpse(train)

test <- read.csv("test.csv")
glimpse(test)

# There are problems to solve, however, in using your model to predict
# on the test set:

# 1.  Missing observations in the test set, specifically in the predictors
# you used to create the model on the train set, will cause the model to
# produce and NA where Kaggle requires a value.  So, depending on your 
# model, you'll need to impute missing values.  (If you use variables that 
# have no missing values you won't encounter this problem, but you also might 
# have a very good model.)

# 2. If you trained your model using a predictor that has a certain number
# of factor levels, then the model expects those same factor levels in
# the test data, or the predict function won't work.

# I'll let your work out how to solve these problems.  For illustration
# purposes we will fit a simple model using predictors that have no
# missing values or discrepant factor levels.

summary(train)

# We'll use LotArea, Neighborhood and OverallCond, which have no missings.
# Let's make sure that these columns are complete in the test set.

test %>%
  dplyr::select(LotArea, Neighborhood, OverallCond) %>%
  summary

# Model
model <- train(SalePrice ~ LotArea + Neighborhood + OverallCond,
               method = "lm",
               data = train)

model

(model <- train(SalePrice ~ LotArea + GrLivArea + OverallCond,
               method = "lm",
               data = train))

summary(model)

# Now use this model to predict the missing SalePrice column in the 
# test set.

predictions <- predict(model, newdata = test)

head(predictions) # A vector

# How do we need to submit to Kaggle?

sample <- read.csv("sample_submission.csv")

head(sample)
tail(sample)

# So we must create a dataset that exactly matches this one,
# but with our own predictions in the second column.

sample_predictions <- data.frame(Id = test$Id,
                                 SalePrice = predictions)

head(sample_predictions)

# Make sure there are no NAs

all(complete.cases(sample_predictions))

# Export your prediction data.frame as a .csv file.

write.csv(sample_predictions, "sample_predictions.csv")
