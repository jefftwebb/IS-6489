### Statistics and Predictive Analytics

# Tutorial topic: Choosing variables (or, model selection)

library(tidyverse)
library(MASS)
library(arm)
library(caret)

data(Boston)
b <- Boston

# As noted in lecture, methods for choosing variables necessarily either include
# or exclude variables; selection is discrete.  Regularized models generally
# perform better than discrete selection with high dimensional data (many
# predictors). Moreover you are often better off when doing inference selecting
# the predictors that make sense in your analytic context. That said, here are
# some methods for variable selection.

### Manual forward selection 

# use anova() to compare nested models with the likelihood ratio test (LRT):
# start with the null model and forward select

mnull <- lm(medv ~ 1, data = b)
m1 <- lm(medv ~ crim, data = b)
anova(mnull, m1)  #The null hypothesis is:  no difference.  
# This model is better than the null.

m2 <- lm(medv ~ crim + zn, data = b)
anova(m1, m2)  #Also much better.

# And so on.  The process is tedious.

# Another approach is to fit a model by comparing AIC:  
# better models have lower AIC.

AIC(lm(medv ~ 1, data = b))
AIC(lm(medv ~ crim, data = b))
AIC(lm(medv ~ crim + zn, data = b))

# Also tedious.

# Fortunately these methods have been automated.
summary(stepAIC(lm(medv ~ ., data=b), direction = "forward")) # all variables
summary(stepAIC(lm(medv ~ ., data=b), direction = "backward")) #11 variables
summary(stepAIC(lm(medv ~ ., data=b), direction  = "both")) #11 variables

# Additional algorithm
library(leaps)
summary(regsubsets(medv ~ ., data=b))
plot(regsubsets(medv ~ ., data=b)) #8 variables

# How does this compare to the full model's R-square?
summary(lm(medv ~ ., data = b)) # R^2 .7338

summary(lm(medv ~ zn + chas +nox + rm + dis + ptratio +
              black + lstat, data=b)) # .7222. 

# Lower R^2 than full model, 
# but simpler model and possibly lower variance or no worse.  

