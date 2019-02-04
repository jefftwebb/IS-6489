### Statistics and Predictive Analytics

# Tutorial topic:  Comparing classification models

library(caret)
library(arm)
library(tidyverse)

# Data consists in predictors of grad school admission +
# the outcome variable, admit. We will use the dataset to practice
# working with logistic regression coefficients.
d <- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")

# Transform rank
d$rank <- factor(d$rank)

# Create a full model
(logistic_model <- glm(admit ~ ., family = binomial, d))

# AIC and residual deviance for comparing nested models:

glm(admit ~ gre, data = d, family = binomial) # AIC = 490, residual deviance = 486
glm(admit ~ gre + gpa, data = d, family = binomial) # Better model?
glm(admit ~ gre + gpa + rank, data = d, family = binomial) # Better model?

# Now fit a KNN model of admit using the same variables.
set.seed(3118)
(knn_model <- train(factor(admit) ~ ., 
                    data=d, 
                    preProcess=c("center", "scale"),
                    method="knn"))

AIC(knn_model)

# Doesn't work! How do we know which model is better? Use
# the confusion matrix and calculate accuracy.

# Syntax for confusionMatrix(): confusionMatrix(predicted, actual).

confusionMatrix(ifelse(predict(logistic_model, newdata=d, type="response")> .5, 1, 0),
                d$admit)

confusionMatrix(predict(knn_model, newdata=d), d$admit)

# PROBLEM: How would we calculate accuracy for these models ourselves?

new_data <- data.frame(observed = d$admit, 
                       predicted = ifelse(predict(logistic_model, 
                                                  newdata=d, type="response")> .5, 1, 0))

head(new_data)

new_data$diff <- ifelse(new_data$observed != new_data$predicted, 1, 0)

head(new_data)

1 - sum(new_data$diff)/nrow(new_data)


## AUC and ROC curve (using pROC package)

# note:  caret uses
  predict(knn_model, newdata = d, type = "prob") %>% head
  predict(knn_model, newdata = d) %>% head # uses default threshold of .5

library(pROC) # The second argument must be predicted probabilities
  
roc(d$admit, 
    predict(knn_model, newdata = d, type = "prob")[,2], 
    plot=T)

roc(d$admit, 
    predict(logistic_model, type = "response"), 
    plot=T, 
    add= T,
    col=2)

# Change decision threshold on the knn model to .1 then .8

confusionMatrix(predict(knn_model, newdata=d), d$admit)$table


confusionMatrix(ifelse(predict(knn_model, newdata=d, type="prob")[,2]>.1, 1, 0), 
                d$admit)$table

confusionMatrix(ifelse(predict(knn_model, newdata=d, type="prob")[,2]>.8, 1, 0), 
                d$admit)$table

## Residual analysis
# Binned residuals (binnedplot in arm)

?binnedplot

# binnedplot(x, y)
# Note: x = fitted values, y = observed - fitted

# First, traditional residual plot

plot(logistic_model, which = 1)

binnedplot(fitted(logistic_model), 
           d$admit - fitted(logistic_model))

# Quick note on residuals and their scale

?residuals.glm # Beware: many options!

(d$admit - fitted(logistic_model)) %>% head
residuals(logistic_model, type="response") %>% head # Probability scale

# Individual predictors against residuals.
binnedplot(d$gre, residuals(logistic_model, type="response"))
binnedplot(d$gpa, residuals(logistic_model, type="response"))

