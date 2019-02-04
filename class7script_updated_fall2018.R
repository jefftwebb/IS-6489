#Class 7 script
# IS 6489

# 1. Missing data imputation
# 2. Ridge and Lasso regression using caret package
# 3. Logistic regression

library(tidyverse)
library(missForest)
library(caret)
library(ISLR)
data(Hitters)

################################################
## Missing Data

# clean first!
names(Hitters)
Hitters[7,1] <- -3
Hitters[53, 7] <- 200
summary(Hitters)

# PRACTICE: Write code to clean

Hitters <- subset(Hitters, AtBat > 0 & Years < 200)

# Housekeeping: Rearrange columns, remove missing outcomes (Salary),
# produce missings, leaving the target alone
Hitters <- cbind(Salary = Hitters$Salary, Hitters[, -19])
h <- na.omit(Hitters)
set.seed(124)
hmiss <- cbind(Salary = h$Salary, prodNA(h[, -1], .2))

# Look at missing data
summary(hmiss)
str(hmiss)

# Impute
hmiss_imp <- missForest(xmis = hmiss,
           variablewise = T,
           verbose = T,
           xtrue = h)

str(hmiss_imp) # Notice that this produces a list; $ximp is the imputed dataset

# Check imputation performance on League
summary(hmiss_imp$ximp$League)
summary(hmiss$League)
summary(h$League)

# compare imputation with the truth
data.frame(Truth = h$League, Imputed = hmiss_imp$ximp$League) %>%
  mutate(diff = ifelse(Truth != Imputed, 1,0))   %>%
  summarize(sum(diff)) # vs. sum(is.na(hmiss$League))

# Overall summary
hmiss_imp$OOBerror %>% round(2)  # Is this good? 

# An alternative is to use caret but turn all variables into 
# numeric variables with the dummyVar() function.

hmiss_dmy <- dummyVars("~.", data = hmiss) %>%
  predict(newdata = hmiss) %>%
  data.frame 

summary(hmiss_dmy)

# Then use medianImpute (or knnImpute or bagImpute)
hmiss_dmy_imp <- preProcess(x = hmiss_dmy, method = "medianImpute") %>%
  predict(newdata = hmiss_dmy) 

summary(hmiss_dmy_imp)

train(Salary~.,
      method = "lm",
      data = hmiss_dmy_imp)

# Check for near zero variance predictors
nearZeroVar(hmiss_dmy)

# Or we can just impute using preProcess in train, if we 
# use the dummyized dataset.  We can't use the formula method...

train(y = hmiss_dmy$Salary,
      x = hmiss_dmy[, -1],
      preProcess = "medianImpute",
      method = "lm")

# Here are the missforest errors
mixError(hmiss_imp$ximp, hmiss, h)

# Here are the medianImpute errors -- all numeric
h_dmy <- dummyVars("~.", data = h) %>%
  predict(newdata = h) %>%
  data.frame 

mixError(hmiss_dmy_imp, hmiss_dmy, h_dmy)


############################################
### Regularization

#We will evaluate model's using caret's built in CV estimates

(lm_mod <- train(Salary ~ ., 
                data = h,
                preProcess = c("center", "scale"),
                method = "lm")) #rmse 350ish

summary(lm_mod)
coef(lm_mod$finalModel) #Pull out coefficients from final model

#We'll use glmnet for both ridge and lasso and for the mixture of the two.
#Note alpha = 0 is ridge, alpha = 1 is lasso.

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
#This is the budget for the size of the coefficients; the smaller the budget
#the closer they are to 0.

#If we want to extract coefficients need to find the optimal lambda:
ridge_mod$finalModel$tuneValue
coef(ridge_mod$finalModel, ridge_mod$finalModel$tuneValue$lambda)

#Prediction from this model will automatically use the best lambda

#So, ridge does better than OLS, but actual performance can be
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

#Prediction from this model will automatically use the best lambda and alpha


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

# PRACTICE
# 1.  Fit Linear model and estimate rmse out of sample
# target variable = V145
names(crime_data)

(lm_model <- train(V145 ~ .,
                   method = "lm",
                   data = crime_data)) #RMSE = 47

# 2. Fit glmnet model and estimate rmse out of sample

(glmnet_mod <- train(V145 ~ ., 
                     method = "glmnet",
                     preProcess = c("center", "scale"),
                     data = crime_data)) # RMSE = 32


# Look at selected coefficients
glmnet_mod$finalModel$tuneValue
coef(glmnet_mod$finalModel, glmnet_mod$finalModel$tuneValue$lambda)

# Which one does better?

#######################################################################
### PRACTICE: Logistic regression

library(arm)
library(MASS)
data("Pima.tr")
data("Pima.te")

d <- rbind(Pima.te, Pima.tr)

# 1. Number of times pregnant 
# 2. Plasma glucose concentration a 2 hours in an oral glucose tolerance test 
# 3. Diastolic blood pressure (mm Hg) 
# 4. Triceps skin fold thickness (mm) 
# 5. 2-Hour serum insulin (mu U/ml) 
# 6. Body mass index (weight in kg/(height in m)^2) 
# 7. Diabetes pedigree function 
# 8. Age (years) 
# 9. Type (Yes or No) 

head(d)

summary(d)

str(d)

# Working with classification models:
# Fit a logistic model :  type ~ bmi + age

summary(glmmod <- glm(type ~ bmi + age, data = d, family=binomial))
arm::display(glmmod)

summary(glmmod <- glm(type ~ bmi + age, data = d)) # Doesn't work


# Look at fitted values
head(fitted(glmmod))
summary(fitted(glmmod))

# We must choose the classification threshold.

ifelse(fitted(glmmod) > .5, T, F) %>% sum
ifelse(fitted(glmmod) > .3, T, F) %>% sum

# The lower the residual deviance and AIC the better the model.

# PROBLEM:  Fit a model with all variables

display(glmmod1 <- glm(type ~., data=d, family=binomial))


# The intercept:  predicted log odds when the predictors = 0. 
# Not interpretable without centering!


# PROBLEM:  Fit a centered and scaled model with all variables.
# Call it glmmod_c
names(d)
display(glmmod_c <- standardize(glm(type ~ npreg + 
                                      glu +
                                      bp +
                                      skin + 
                                      bmi +
                                      ped +
                                      age, data = d, family = "binomial")))

# Function to transform log odds into probabilities
?invlogit
invlogit


# PROBLEM:  find the probability of diabetes for the average person.

coef(glmmod_c)[1] %>% invlogit

# Use odds ratio to understand npreg coefficient

# Interpret the coefficient of bmi using OR.

exp(coef(glmmod_c)[6])


# Now a  more involved case:
# How does the probability of having diabetes increase with npreg?
# must choose where on the inverse logit curve to evaluate.
# Let's work with a reduced model.
# type ~ npreg + glu 

new <- glm(type ~ npreg + glu, data = d, family = "binomial")
summary(d)

invlogit(coef(new)[1] + coef(new)[2]*4 + coef(new)[3]*mean(d$glu)) -
invlogit(coef(new)[1] + coef(new)[2]*3 + coef(new)[3]*mean(d$glu))


#This model says: increasing pregnancies from 2 to 3 with average 
# glucose is associated with a 3% increase the probability of having diabetes?

# Add BMI to this model and calculate the probability of increasing BMI
# from 35 to 40 with average npreg and glu.

#What is the  OR of BMI when npreg and glu are average?

# The odds ratio tells us how the odds of diabetes increase with a one unit 
# change in bmi.  One unit change in bmi
# is associated with a what %  increase in the odds of having diabetes?


