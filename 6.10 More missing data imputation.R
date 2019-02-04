### Statistics and Predictive Analytics

# Tutorial topic: More imputation of missing values

library(tidyverse)
library(missForest)
library(caret)
library(ISLR)
data(Hitters)

glimpse(Hitters)

# Clean first then impute!
names(Hitters)
Hitters[7,1] <- -3
Hitters[53, 7] <- 200

summary(Hitters)

# Clean

Hitters <- subset(Hitters, AtBat >= 0 & Years < 200)

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
set.seed(920)
hmiss_imp <- missForest(xmis = hmiss, xtrue = h)

str(hmiss_imp) 
# Notice that this produces a list; $ximp is the imputed dataset

# Check imputation performance on League, as an example
summary(hmiss_imp$ximp$League)
summary(hmiss$League)
summary(h$League)

# compare imputation with the truth
data.frame(Truth = h$League, Imputed = hmiss_imp$ximp$League) %>%
  mutate(diff = ifelse(Truth != Imputed, 1,0))   %>%
  summarize(sum(diff)) # vs. sum(is.na(hmiss$League))

# Overall summary
hmiss_imp$OOBerror 
# NRMSE = root mean squared difference between imputed and actual continuous
# values divided by the variance of the actual values.

# An alternative is to use caret but turn all variables into 
# numeric variables with the dummyVar() function.

hmiss_dmy <- dummyVars("~.", 
                       data = hmiss,
                       fullRank = TRUE) %>%
  predict(newdata = hmiss) %>%
  data.frame 

summary(hmiss_dmy) 
# Notice that the fullRank argument ensures that one level
# for each factor is left out, which is appropriate for regression
# modelling.  Why?  In the case of binary variable, the information 
# for the missing level is already encoded in the single column of 0s 
# 1s.

# Then use medianImpute (or knnImpute or bagImpute)
hmiss_dmy_imp <- preProcess(x = hmiss_dmy, method = "medianImpute") %>%
  predict(newdata = hmiss_dmy) 

summary(hmiss_dmy_imp)

train(Salary~.,
      method = "lm",
      data = hmiss_dmy_imp)

# Check for near zero variance predictors
nearZeroVar(hmiss_dmy)
nearZeroVar(cbind(hmiss_dmy, rep(1, nrow(hmiss_dmy))))

# Or we can just impute using preProcess in train, if we 
# use the dummyized dataset.  We can't use the formula method...

train(y = hmiss_dmy$Salary,
      x = hmiss_dmy[, -1],
      preProcess = "medianImpute",
      method = "lm")

# Here are the missforest errors
?mixError
mixError(ximp = hmiss_imp$ximp, 
         xmis = hmiss, 
         xtrue = h)

# Here are the medianImpute errors -- all numeric
h_dmy <- dummyVars("~.", data = h) %>%
  predict(newdata = h) %>%
  data.frame 

mixError(hmiss_dmy_imp, hmiss_dmy, h_dmy)


