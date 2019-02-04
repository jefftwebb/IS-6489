### Statistics and Predictive Analytics

# Tutorial topic:  Intro to logistic regression

# Packages, data
library(tidyverse)
library(missForest)
library(caret)
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

display(glmmod <- glm(type ~ bmi + age, data = d, family=binomial))

display(glm(type ~ bmi + age, data = d)) # Doesn't work


# Look at fitted values
head(fitted(glmmod))
summary(fitted(glmmod))

# We must choose the classification threshold.

ifelse(fitted(glmmod) > .5, T, F) %>% sum
ifelse(fitted(glmmod) > .3, T, F) %>% sum

# The lower the residual deviance and AIC the better the model.

summary(glmmod) # vs.

glm(type ~ ., data = d, family=binomial) %>%
  summary

# The intercept:  predicted log odds when the predictors = 0. 
# Not interpretable without centering!

# Fit a centered and scaled model with all variables.
# Call it glmmod_c

glmmod_c <- glm(type ~ npreg + 
                  glu +
                  bp +
                  skin + 
                  bmi +
                  ped +
                  age, 
                data = d, 
                family = "binomial") %>%
  standardize 

display(glmmod_c)

# Function to transform log odds into probabilities
?invlogit
invlogit

# PROBLEM:  find the probability of diabetes for the average person.

coef(glmmod_c)[1] %>% 
  invlogit %>%
  round(2)

# Use odds ratio (OR) to understand npreg coefficient

coef(glmmod_c)[2] %>%
  exp()

# The baseline is 1.  Hence, increasing z.npreg by 1 unit is associated with
# 125% greater odds of having diabetes.

# Interpret the coefficient of bmi using OR.

coef(glmmod_c)[6] %>%
  exp()

# Increasing z.bmi by 1 unit is associated with 212% greater odds of 
# having diabetes.

# Now a  more involved case: How does the probability of having diabetes
# increase with npreg? Must choose where on the inverse logit curve to evaluate.
# Let's work with a reduced model. type ~ npreg + glu

new <- glm(type ~ npreg + glu, data = d, family = "binomial")

mean(d$npreg)

invlogit(coef(new)[1] + coef(new)[2]*4 + coef(new)[3]*mean(d$glu)) -
invlogit(coef(new)[1] + coef(new)[2]*3 + coef(new)[3]*mean(d$glu))

# This model says: increasing pregnancies from 3 to 4 with average 
# glucose is associated with a 3% increase the probability of having diabetes.

# We could obtain the same result with the dataframe method.

predict_frame <- data.frame(npreg = c(3,4),
                            glu = mean(d$glu))

predict_frame

predict(new, newdata = predict_frame[2, ]) %>% invlogit -
  predict(new, newdata = predict_frame[1, ]) %>% invlogit

predict(new, newdata = predict_frame[2, ], type = "response")  -
  predict(new, newdata = predict_frame[1, ], type = "response") 

# Add BMI to this model and calculate the probability of increasing BMI
# from 35 to 40 with average npreg and glu.

new2 <- glm(type ~ npreg + glu + bmi, data = d, family = "binomial")

# Again, we'll use the dataframe method:

predict_frame2 <- data.frame(npreg = mean(d$npreg),
                            glu = mean(d$glu),
                            bmi = c(35, 40))

predict_frame2

predict(new2, newdata = predict_frame2[2, ], type = "response")  -
  predict(new2, newdata = predict_frame2[1, ], type = "response") 

