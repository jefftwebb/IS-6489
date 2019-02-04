# Class 9 script
# IS 6489

library(caret)
library(arm)
library(tidyverse)

d <- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")

summary(d)

d$rank <- factor(d$rank)

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

logistic_model <- glm(admit ~., data = d, family = binomial)

d$glm_pred <- ifelse(fitted(logistic_model) > .5, 1, 0)

ggplot(d, aes(gre, gpa, col = factor(glm_pred))) +
  geom_point() 

# Plot with points colored by  knn model predictions

knn_model <- train(factor(admit) ~ gre + gpa + rank,
                   preProcess = c("center","scale"),
                   method= "knn",
                   data = d)

d$knn_pred <- fitted(knn_model)

ggplot(d, aes(gre, gpa, col = factor(knn_pred))) +
  geom_point() 

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
library(pROC)
roc(d$admit, predict(knn_model, 
                     newdata = d, 
                     type = "prob")[,2], 
    plot = T) # AUC = .76

roc(d$admit, fitted(logistic_model), 
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

