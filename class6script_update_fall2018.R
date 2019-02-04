### Class 6 script
### IS 6489

library(arm)
library(tidyverse)
library(caret)
library(MASS)
rmse <- function(actual, predicted) sqrt(mean((actual - predicted)^2))

############################################################
# EXPLORATION: Overfitting

bv <- data.frame(species = c("afarensis", "africanus", "habilis", "boisei", 
                                   "rudolfensis","ergaster", "sapiens"),
                 brain = c(458, 432, 612, 521, 752, 871, 1350),
                 mass = c(37, 35.5, 34.5, 41.5, 55.5, 61, 53.5))

bv

# Make a scatterplot of brain volume ~ body mass with linear fit

ggplot(bv, aes(mass, brain)) +
  geom_point() +
  stat_smooth(method = "lm")

display(p1 <- lm(brain ~ mass, bv))

rmse(bv$brain, predict(p1))

# Quadratic
display(p2 <- lm(brain ~ mass + I(mass^2), bv))
bv$p2 <- fitted(p2) #Add these fitted values to the data frame
ggplot(bv, aes(mass, brain)) +
  geom_point() +
  geom_line(aes(mass, p2)) +
  labs(title = "R-squared = .54")

rmse(bv$brain, predict(p2))

# Cubic
display(p3 <- lm(brain ~ mass + I(mass^2) + I(mass^3), bv))
bv$p3 <- fitted(p3) 
ggplot(bv, aes(mass, brain)) +
  geom_point() +
  geom_line(aes(mass, p3)) +
  labs(title = "R-squared = .68")

rmse(bv$brain, predict(p3))

# order = 4
display(p4 <- lm(brain ~ mass + I(mass^2) + I(mass^3) + I(mass^4), bv))
bv$p4 <- fitted(p4)
ggplot(bv, aes(mass, brain)) +
  geom_point() +
  geom_line(aes(mass, p4)) +
  labs(title = "R-squared = .80")

rmse(bv$brain, predict(p4))

# order = 5
display(p5 <- lm(brain ~ mass + I(mass^2) + I(mass^3) + I(mass^4) +
                   + I(mass^5), bv))
bv$p5 <- fitted(p5)
ggplot(bv, aes(mass, brain)) +
  geom_point() +
  geom_line(aes(mass, p5)) +
  labs(title = "R-squared = .98")

rmse(bv$brain, predict(p5))

# order = 6
display(p6 <- lm(brain ~ mass + I(mass^2) + I(mass^3) + I(mass^4) +
                   + I(mass^5) + I(mass^6), bv))
bv$p6 <- fitted(p6)
ggplot(bv, aes(mass, brain)) +
  geom_point() +
  geom_line(aes(mass, p6)) +
  labs(title = "R-squared = 1")

rmse(bv$brain, predict(p6))

# A perfect model! But, this is overfitting.  The six degree polynomial has
# enough parameters to assign one to each data point. The fit is no longer
# summarizing.  It IS the data.

# Question:  what will happen when this model encounters different data?

new_bv <- rbind(bv[, 2:3], data.frame(brain = abs(rnorm(2, mean = 700, sd = 500)), 
                                      mass = abs(rnorm(2, 50, 20))))
new_bv

rmse(new_bv$brain, predict(p6, newdata=new_bv))

ggplot(new_bv, aes(mass, brain)) +
  geom_point() +
  geom_line(aes(new_bv$mass, predict(p6, newdata = new_bv))) +
  theme_minimal()

# Schockingly bad!

#######################################################
# Cross-validation

# Get data and format into test and train sets
data("Pima.tr");data("Pima.te")
pima <- rbind(Pima.tr,Pima.te)

#randomly pick rows from the dataset
set.seed(327)
rows <- sample(nrow(pima), round(nrow(pima)*.7), replace = F)
rows
train <- pima[rows, ]
test <- pima[-rows, ]

head(train)

# Here is another method using caret's createDataPartition() function.
set.seed(327)
rows <- createDataPartition(pima$bp, times = 1,
                            p = .7, list = FALSE)
train <- pima[rows, ]
test1 <- pima[-rows, ]


#Fit a model on the train dataset that will be
# extremely accurate (low bias) but is ridicululous: 

display(low_bias <- lm(bp ~ glu * age * bmi * ped * 
                         type * npreg * skin, 
                       data = train)) 

#R2 is .51.  Killing it, we are geniuses

# Compare to a simple model:
display(low_variance <- lm(bp ~ ., data = train))
#R2 is only .29. 

# PROBLEM: Calculate in sample RMSE for low-bias and low_variance models.


# PROBLEM: Calculate out of sample RMSE for low-bias and low_variance models
# using test set.

# Which is the better model?

#######################################
# K fold cross validation with Caret

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
        number=10, 
        verboseIter = T
        )
      )

# 10 fold repeated five times, the more repeats the greater stability in
# the estimates. We can define the trainControl object outside 
# of the train function.

myControl <- trainControl(
  method="repeatedcv", 
  number=10, 
  repeats = 5,
  verboseIter = T
)

train(bp ~ .,
      data = train,
      method = "lm", 
      trControl = myControl
      )


#######################################################
# Choosing variables
data(Boston)
b <- Boston

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

#######################################################
### Imputation of missing values

# Add missing values to Pima using missForest package
library(missForest)
set.seed(327)
pmiss <- cbind(bp = pima$bp, prodNA(pima[,-3], .2))

# Check missing values
summary(pmiss)

# If we use this dataset what happens?  Fit a model of BP with
# all the variables.

# Try fitting this model with caret (default cv)



#Caret's default action when using predict() is to 
# complain about missing values:

# Using caret, we can do single imputation using knnImpute,
# medianImpute
# But there are nuances...

lm(bp ~ ., data = pmiss) # lm() handles missing silently by removing them.

# But you can instruct caret to pass the method for handling NAs on to lm()
# na.pass leaves na.action up to the source function, here lm,
# which omits NAs. 

summary(basic <- train(bp ~ ., 
                       na.action = na.pass,
                       data=pmiss, 
                       method="lm")) # small df!

length(predict(basic)) # short predict vector!

# Deletion can be improved upon for MAR data.
# If we are only interested in prediction then we can use the 
# preProcess function inside train to impute with medians
# or knn regression:

med_imp <- train(y = pmiss$bp,
              x = pmiss[,-1],
              method = "lm",
              trControl = myControl,
              preProcess = c("medianImpute"))

install.packages("RANN") #might need to install

knn_imp <- train(y = pmiss$bp,
                 x = pmiss[,-1],
                 method = "lm",
                 trControl = myControl,
                 preProcess = c("knnImpute"))

# If you want to create a new imputed dataset then use the 
# following code.  
#Note:  Here we include the outcome variable, which we obviously 
#could not do if we were imputing on the test set.

## Impute with medians
medimp <- preProcess(p, method=c("medianImpute")) %>%
  predict(p)

# However, there is a shortcoming with these caret methods:  they
# don't work for categorical variables.

summary(medimp)
head(predict(med_imp)) 
head(predict(knn_imp)) # missing values where there are missing values in type
head(pmiss$type)

# Frustrating!

# missForest is (I think) the best alternative for single imputation
# using a multivariate model. 
# https://arxiv.org/abs/1105.0828
?missForest
library(missForest)
mfimp <- missForest(pmiss)

str(mfimp)
summary(mfimp$ximp)

display(lm(bp ~ ., data = mfimp$ximp))
