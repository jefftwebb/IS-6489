### Statistics and Predictive Analytics

# Tutorial topic: Regularization using caret package

library(tidyverse)
library(caret)
library(ISLR)
data(Hitters)

h <- na.omit(Hitters)

glimpse(h)

#We will evaluate models using caret's built in CV estimates

(lm_mod <- train(Salary ~ ., 
                data = h,
                preProcess = c("center", "scale"),
                method = "lm")) #rmse 350ish

summary(lm_mod)
coef(lm_mod$finalModel) #Pull out coefficients from final model

#We'll use glmnet for both ridge and lasso and for the mixture of the two.
#Note alpha = 0 is ridge, alpha = 1 is lasso.
# (We could also use method = "ridge" or method = "lasso.")

set.seed(111)
(ridge_mod <- train(Salary ~ ., 
                   data = h,
                   preProcess = c("center", "scale"),
                   method = "glmnet",
                   tuneGrid= expand.grid(
                     alpha=0,
                     lambda = seq(0,300, 1)))) #rmse 350ish


plot(ridge_mod)
#Lambda is the shrinkage parameter; lambda of 0 indicates no shrinkage,
#which would be identical to a linear model

#Another visualization:
plot(ridge_mod$finalModel)
#L1 norm is mislabelled here--should be L2 for ridge.
#This is the budget for the size of the coefficients; the smaller 
# the budget the closer they are to 0.

#If we want to extract coefficients need to find the optimal lambda:
ridge_mod$finalModel$tuneValue
coef(ridge_mod$finalModel, ridge_mod$finalModel$tuneValue$lambda)

#Prediction from this model will automatically use the best lambda

#So, ridge is similar to OLS, but actual performance can be
#variable due to cross-validation sampling variation leading to different
#values of lambda being selected.

#Let's compare:
compare <- data.frame(variables = rep(as.character(names(coef(lm_mod$finalModel))),2))
compare$method <- c(rep("ridge", 20), rep("lm", 20))
compare$coefs <- c(as.numeric(as.character(coef(ridge_mod$finalModel, ridge_mod$bestTune$lambda))),
                   as.numeric(as.character(coef(lm_mod$finalModel))))

ggplot(compare, aes(variables, coefs, group=method, fill=method)) + 
  geom_bar(stat = "identity",position="dodge") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Lasso
(lasso_mod <- train(Salary ~ ., 
                   data = h,
                   preProcess = c("center", "scale"),
                   method = "glmnet",
                   tuneGrid= expand.grid(
                     alpha=1,
                     lambda = seq(0,5, .01)))) # 340ish
plot(lasso_mod)
plot(lasso_mod$finalModel)

lasso_mod$finalModel$tuneValue
coef(lasso_mod$finalModel, lasso_mod$finalModel$tuneValue$lambda)

#Prediction from this model will automatically use the best lambda

#Add lasso to compare
compare_temp  <- data.frame(variables = as.character(names(coef(lm_mod$finalModel))))
compare_temp$method <- rep("lasso", 20)
compare_temp$coefs <- c(as.numeric(as.character(coef(lasso_mod$finalModel, 
                                                     lasso_mod$finalModel$tuneValue$lambda))))
compare <- rbind(compare, compare_temp)

ggplot(compare, aes(variables, coefs, group=method, fill=method)) + 
  geom_bar(stat = "identity",position="dodge") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Mixture of lasso and ridge
(mix_mod <- train(Salary ~ ., 
                 data = h,
                 preProcess = c("center", "scale"),
                 method = "glmnet")) # 350ish
plot(mix_mod)
plot(mix_mod$finalModel)
mix_mod$finalModel$tuneValue
coef(mix_mod$finalModel, mix_mod$finalModel$tuneValue$lambda)

# Prediction from this model will automatically use the best lambda and alpha
predict(mix_mod, newdata =...)

## Practice/Demonstrate use of regularization on high dimensional data

# Get a high dimensional dataset:
crime_data <- read.csv("http://archive.ics.uci.edu/ml/machine-learning-databases/00211/CommViolPredUnnormalizedData.txt", 
                       header = F, 
                       sep = ",", 
                       quote = "\"", 
                       dec = ".", 
                       fill = TRUE, 
                       comment.char = "", 
                       na.strings = "?", 
                       strip.white=TRUE, 
                       stringsAsFactors = F)

# Information: http://archive.ics.uci.edu/ml/datasets/communities+and+crime

glimpse(crime_data)

# The final variable, ViolentCrimesPerPop, will be the dependent variable.

# Missing observations
any(is.na(crime_data))

# Impute missing observations with medians for speed.
crime_data <- predict(preProcess(crime_data[, -c(1:2)], 
                                 method = c("medianImpute")), 
                      crime_data[, -c(1:2)])

all(complete.cases(crime_data))

# Check predictors for NZV
nearZeroVar(crime_data)

crime_data <- crime_data[, -nearZeroVar(crime_data)]

# PRACTICE
# 1.  Fit Linear model and estimate rmse out of sample
# target variable = V145
names(crime_data)

(lm_model <- train(V145 ~ .,
                   method = "lm",
                   data = crime_data)) #RMSE = 43

# 2. Fit glmnet model and estimate rmse out of sample

(glmnet_mod <- train(V145 ~ ., 
                     method = "glmnet",
                     preProcess = c("center", "scale"),
                     data = crime_data)) # RMSE = 32


# Look at selected variables
glmnet_mod$finalModel$tuneValue
coef(glmnet_mod$finalModel, glmnet_mod$finalModel$tuneValue$lambda)

