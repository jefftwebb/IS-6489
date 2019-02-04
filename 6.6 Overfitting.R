### Statistics and Predictive Analytics

# Tutorial topic: Overfitting

library(tidyverse)
library(MASS)
library(arm)
library(caret)

rmse <- function(actual, predicted) sqrt(mean((actual - predicted)^2))

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

# degree = 4
display(p4 <- lm(brain ~ mass + I(mass^2) + I(mass^3) + 
                   I(mass^4), bv))

bv$p4 <- fitted(p4)

ggplot(bv, aes(mass, brain)) +
  geom_point() +
  geom_line(aes(mass, p4)) +
  labs(title = "R-squared = .80")

rmse(bv$brain, predict(p4))

# order = 5
display(p5 <- lm(brain ~ mass + I(mass^2) + I(mass^3) + 
                   I(mass^4) +
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

new_bv <- rbind(bv[, 2:3], 
                data.frame(brain = abs(rnorm(2, mean = 700, sd = 500)), 
                                      mass = abs(rnorm(2, 50, 20))))
new_bv

rmse(new_bv$brain, predict(p6, newdata=new_bv))

ggplot(new_bv, aes(mass, brain)) +
  geom_point() +
  geom_line(aes(new_bv$mass, predict(p6, newdata = new_bv))) +
  theme_minimal()

# Schockingly bad!  That's overfitting.  When models overfit
# they perform terribly in prediction because they have fit noise
# in the training sample that does not exist in new data.