### Statistics and Predictive Analytics

# Tutorial topic: Imputation of missing values

library(tidyverse)
library(MASS)
library(arm)
library(caret)
rmse <- function(actual, predicted) sqrt(mean((actual - predicted)^2))

# Get data 
data("Pima.tr");data("Pima.te")
pima <- rbind(Pima.tr,Pima.te)

glimpse(pima)

# Add missing values to Pima using missForest package
library(missForest)
set.seed(327)
pmiss <- cbind(bp = pima$bp, prodNA(pima[,-3], .2))

# Check missing values
summary(pmiss)

# If we use this dataset what happens?  Fit a model of BP with
# all the variables.

lm(bp ~ ., data = pmiss) %>%
  display

# lm() handles missing silently by removing them. Not many obvservations
# left in this case.

# Try fitting this model with caret (default cv)

train(bp ~ ., 
      data = pmiss,
      method = "lm") 

# Caret's default action when using train() is to fail with NAs. 
# You can instruct caret to pass the method for handling NAs on to lm()
# na.pass leaves na.action up to the source function, here lm,
# which omits NAs. 

summary(basic <- train(bp ~ ., 
                       na.action = na.pass,
                       data=pmiss, 
                       method="lm"))

length(predict(basic)) 

# Deletion can be improved upon for MAR data.
# How do we impute missing values?
# If we are only interested in prediction then we can use the 
# preProcess function inside train to impute with medians
# or knn regression:

# Median imputation (need to specify x and y; don't use formula syntax)
med_imp <- train(y = pmiss$bp,
              x = pmiss[,-1],
              method = "lm",
              preProcess = c("medianImpute"))

summary(med_imp) 
head(predict(med_imp))

# Notice that there are still some deletions! MedianImpute
# does not work with categorical variables like type.

# If you want to create a new imputed dataset then use the following code.
# Note:  Here we include the outcome variable, which we obviously could not do
# if we were imputing on the test set.

medimp <- preProcess(pmiss, method=c("medianImpute")) %>%
  predict(pmiss)

summary(medimp) # NAs remain in type

#KNN imputation
install.packages("RANN") #might need to install

knn_imp <- train(y = pmiss$bp,
                 x = pmiss[,-1],
                 method = "lm",
                 preProcess = c("knnImpute"))

head(predict(knn_imp))

# KNN imputation does not work with categorical variables either.

# Frustrating!

# missForest is (I think) the best alternative for single imputation
# using a multivariate model. 
# https://arxiv.org/abs/1105.0828
?missForest
library(missForest)


head(pmiss)

mfimp <- missForest(pmiss)

str(mfimp)
summary(mfimp$ximp)

display(lm(bp ~ ., data = mfimp$ximp))
