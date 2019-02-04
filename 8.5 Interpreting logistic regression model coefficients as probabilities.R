### Statistics and Predictive Analytics

# Tutorial topic:  More logistic regression

library(caret)
library(arm)
library(tidyverse)

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

logistic_model <- glm(admit ~ gre +
                        gpa +
                        rank, family = binomial, d)

confint(logistic_model) # Nice function for calculating CIs

# 2.  Interpret the coefficients in terms of log odds.

summary(standardize(logistic_model))

# 3. Interpret the coefficients in terms of odds ratios.

coef(standardize(logistic_model)) %>%
  exp

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

new2$predicted_prob <- predict(logistic_model, 
                               newdata = new2, 
                               type = "response")

head(new2)

ggplot(new2, aes(gre, predicted_prob, col = rank)) +
  geom_line() +
  labs(title = "Predicted probability of admission by GRE and School rank")

