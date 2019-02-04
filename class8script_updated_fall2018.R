# Class 8 script
# IS 6489

library(caret)
library(arm)
library(tidyverse)

###############################################
## PRACTICE:  Working with logistic regression coefficients

# Function to transform log odds into probabilities
?invlogit
invlogit

# Grab data.  The data consists in predictors of grad school admission +
# the outcome variable, admit. We will use the dataset to practice
# working with logistic regression coefficients.
d <- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")

glimpse(d)
summary(d)

# Think about cleaning and data modelling.  Do the ranges of the variables
# make sense?  Do we need to transform any variables?

d$rank <- factor(d$rank)

# Do EDA

# 1. boxplot of admit and gre and gpa

ggplot(d, aes(factor(admit), gre)) +
  geom_boxplot()

ggplot(d, aes(factor(admit), gpa)) +
  geom_boxplot()


#2. facet boxplots by rank

ggplot(d, aes(factor(admit), gre)) +
  geom_boxplot() +
  facet_wrap(~rank)

ggplot(d, aes(factor(admit), gpa)) +
  geom_boxplot()+
  facet_wrap(~rank)

# 3. Histograms of gre and gpa, facetting by rank

ggplot(d, aes(gre)) +
  geom_histogram()

ggplot(d, aes(gre)) +
  geom_histogram() +
  facet_wrap(~rank)

ggplot(d, aes(gpa)) +
  geom_histogram() +
  facet_wrap(~rank)

# 4. Table admissions and rank

# Model
# 1. Fit and display a logistic regression of admit with all 
# the predictors.

logistic_model <- glm(admit ~ ., family = binomial, d)

confint(logistic_model) # Nice function for calculating CIs

# 2.  Interpret the coefficients in terms of log odds.

summary(logistic_model)

# 3. Interpret the coefficients in terms of odds ratios.

# 4. Transform results into probabilities, using the inverse logit function.
# Probabilities have the advantage of being more interpretable but harder to
# produce. Because the inverse logit is non-linear we must pick the predictor
# values at which to evaluate probabilites of the outcome.

# Use dataframe method.  Set up data frame with the quantities
# for which you would like probabilites, then transform them 
# with the inverse logit function.

new <- data.frame(gre = mean(d$gre), 
                  gpa = mean(d$gpa), 
                  rank = factor(c(1, 2, 3, 4)))
new

predict(logistic_model, newdata= new) # log odds, untransformed

invlogit(predict(logistic_model, newdata= new)) #Probabilities, or

predict(logistic_model, newdata= new, type = "response")

# 5.  What is the change in the probability of admission if we go from rank of
# 4 to 1 when GPA and GRE are average?

invlogit(predict(logistic_model, newdata= new))[1]-
  invlogit(predict(logistic_model, newdata= new))[4]




# 6.  Demonstration.  Probability curves for GRE by rank

new2 <- data.frame(gre = rep(seq(from = 200, to = 800, length.out = 100), 4), 
                   gpa = mean(d$gpa), 
                   rank = factor(rep(1:4, each = 100)))

new2

new2$predicted_prob <- predict(logistic_model, newdata = new2, type = "response")

head(new2)

ggplot(new2, aes(gre, predicted_prob, col = rank)) +
  geom_line() +
  labs(title = "Predicted probability of admission by GRE and School rank")

############################################################
## Comparing models 

# AIC and residual deviance for comparing nested models:

glm(admit ~ gre, data = d, family = binomial) # AIC = 490, residual deviance = 486
glm(admit ~ gre + gpa, data = d, family = binomial) # Better model?
glm(admit ~ gre + gpa + rank, data = d, family = binomial) # Better model?

# Compare logistic_model from above to a KNN model using the same variables.
# But, we can't use AIC or residual deviance!

set.seed(3118)
(knn_model <- train(factor(admit) ~ ., 
                    data=d, 
                    preProcess=c("center", "scale"),
                    method="knn"))

AIC(knn_model)

# confusion matrix in caret:  which is the better model?
# Syntax for confusionMatrix(): confusionMatrix(predicted, actual).

confusionMatrix(ifelse(predict(logistic_model, newdata=d, type="response")> .5, 1, 0),
                d$admit)

confusionMatrix(predict(knn_model, newdata=d), d$admit)

# PROBLEM: How would we calculate accuracy for these models ourselves?

new_data <- data.frame(observed = d$admit, 
                       predicted = ifelse(predict(logistic_model, newdata=d, type="response")> .5, 1, 0))

head(new_data)

new_data$diff <- ifelse(new_data$observed != new_data$predicted, 1, 0)

head(new_data)

1 - sum(new_data$diff)/nrow(new_data)


## AUC and ROC curve (using pROC package)

# note:  
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

#############################################
# Decision boundaries

# Plot with points colored by actual admissions
ggplot(d, aes(gre, gpa, col = factor(admit))) +
  geom_point() 

# small multiples by rank
ggplot(d, aes(gre, gpa, col = factor(admit))) +
  geom_point() +
  facet_wrap(~rank)

# Plot with points colored by  glm model predictions
d$glm_pred <- ifelse(fitted(logistic_model)> .5, 1, 0)

ggplot(d, aes(gre, gpa, col = factor(glm_pred))) +
  geom_point() 

# small multiples by rank
ggplot(d, aes(gre, gpa, col = factor(glm_pred))) +
  geom_point() +
  facet_wrap(~rank)

# Plot with points colored by  knn model predictions
d$knn_pred <- fitted(knn_model)

ggplot(d, aes(gre, gpa, col = factor(knn_pred))) +
  geom_point() 

# small multiples by rank
ggplot(d, aes(gre, gpa, col = factor(knn_pred))) +
  geom_point() +
  facet_wrap(~rank)

#############################################
# SVM

#Linear
set.seed(31)
d$admit2 <- factor(ifelse(d$admit == 0, "no", "yes"))
summary(d$admit2)
(svm_model <- train(admit2 ~ gre + gpa + rank, 
                   data = d,
                   method = "svmLinear2",
                   preProcess = c("center","scale"),
                   trControl = trainControl(classProbs =  TRUE))) #We need this argument


# Visualize result
d$svm_pred <- predict(svm_model)
ggplot(d, aes(gre, gpa, col = factor(svm_pred))) +
  geom_point() 

ggplot(d, aes(gre, gpa, col = factor(svm_pred))) +
  geom_point()+
  facet_wrap(~rank)

confusionMatrix(predict(svm_model), ifelse(d$admit==0, "no","yes"))

# Polynomial
(svm_poly_model <- train(admit2 ~ gre + gpa + rank, 
                    data = d,
                    method = "svmPoly",
                    preProcess = c("center","scale"),
                    trControl = trainControl(classProbs =  TRUE)))


# Visualize result
d$svm_poly_pred <- predict(svm_poly_model)
ggplot(d, aes(gre, gpa, col = factor(svm_poly_pred))) +
  geom_point() 

ggplot(d, aes(gre, gpa, col = factor(svm_poly_pred))) +
  geom_point()+
  facet_wrap(~rank)

confusionMatrix(predict(svm_poly_model), ifelse(d$admit==0, "no","yes"))


## Summary model comparison
roc(d$admit, predict(knn_model, newdata = d, type = "prob")[,2], 
    plot = T) # AUC = .76

roc(d$admit, predict(logistic_model, type = "response"), 
    plot = T, 
    add= T, 
    col = 2) # AUC = .69

roc(d$admit, predict(svm_model, newdata = d, type = "prob")[,2], 
    plot = T, 
    add= T, 
    col = 3) # AUC = .67

roc(d$admit, predict(svm_poly_model, newdata = d, type = "prob")[,2], 
    plot = T, 
    add= T, 
    col = 4) # AUC = .65

