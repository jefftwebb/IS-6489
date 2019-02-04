### Statistics and Predictive Analytics

# Tutorial topic:   Adding controls to linear regression

library(tidyverse)
library(arm)
library(MASS)
library(caret)

# Get data
data(Boston)
b <- Boston

# Often in descriptive/inferential analysis we are interested in testing the 
# effect of one variable on the outcome, after controlling for possibly
# confounding variables.

# In Lab 2, for example, we were interested in whether nox is associated with 
# median home values.  

# In a simple model, nox predicts home price:

display(lm(medv ~ nox, b)) # nox is predictive

# But the effect goes away if we add lstat:

display(lm(medv ~ nox + lstat, b)) 

# The most model for testing an effect of nox will be one in which
# we control as many other predictors as possible.

display(lm(medv ~ crim + zn + indus + chas + nox + rm + age +
             dis + rad + tax + ptratio + black + lstat, b))

# or, using as.formula():

names(b)
terms <- paste(names(b)[1:13], collapse = "+")
terms
formula <- as.formula(paste("medv ~", terms, sep =""))

lm(formula,data = b) %>%
  summary

# or, using ~ .

summary(lm(medv ~ .,data = b))

## Centering and scaling issues

# Differences in scaling!
summary(b)

# Center and scale nox using rescale() and scale()

b$nox %>% head
rescale(b$nox) %>% head # arm package
scale(b$nox) %>% head # base R

# Now center and scale nox in the model

# Center and scale the entire model using standardize().

display(standardize(lm(medv ~ crim + zn + indus + chas + nox + rm + age +
                         dis + rad + tax + ptratio + black + lstat, b))) 


# Compare to standardize(binary.inputs = "leave.alone") 

display(standardize(lm(medv ~ crim + zn + indus + chas + nox + rm + age +
                         dis + rad + tax + ptratio + black + lstat, b),
                    binary.inputs = "leave.alone")) 


# What does the intercept represent in a centered model?

# Now use caret to center and scale the same model.

train(formula, 
      data = b,
      method = "lm",
      preProcess = c("center", "scale")) %>%
  summary

