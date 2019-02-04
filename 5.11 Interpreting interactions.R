### Statistics and Predictive Analytics

# Tutorial topic: Interactions

library(tidyverse)
library(MASS)
library(arm)
library(caret)

data(Boston)
b <- Boston

# Does the effect of nox on housing price vary with proximity to 
# the charles? To keep things simple we will model medv.

lm(medv ~ nox * 
     chas + 
     rm + 
     lstat + 
     dis, b) %>%
  standardize(binary.inputs = "leave.alone") %>%
  display

# The interaction coefficient indicates the difference in slope
# of the regression lines for nox, on the charles and off:

# First, illustrate with a plot.

ggplot(b, aes(rescale(nox), medv, col = factor(chas)))+
  geom_point() +
  geom_smooth(method = "lm")

# Now interpret output.

# z.nox:chas. -1.31.  The regression line between z.nox and 
# medv loses 1.31 in slope when charles goes from 0 to 1. 
# (This is a relatively small effect--the smallest coefficient--
# and is relatively uncertain:  -1.31 + 2*1.6 > 0.  The 95% CI
# contains 0.)

# The main effects for the interacted variables are more interpretable
# because we have centered.

# z.nox: -3.35.  Increasing z.nox by 1 unit (2 sd) is associated
# with a change in medv of -3.35, among those districts where
# chas = 0. 

# chas: 4.25.  Increasing chas by 1 unit (0 to 1) is associated
# with a change in medv of 4.25, among those districts where
# z.nox = 0 (is average)


# Does the effect of nox on housing price vary with # of rooms?

lm(medv ~ nox * 
     rm + 
     lstat + 
     chas + 
     dis, b) %>%
  standardize(binary.inputs = "leave.alone") %>%
  display

# Illustrate this result with a plot.

# 1. Turn rm into a binary variable

b$rm_bin <- ifelse(b$rm > mean(b$rm), "high", "low")

# 2. plot nox vs medv, varying by rm_bin

ggplot(b, aes(rescale(nox),medv, col = rm_bin))+
  geom_point() +
  geom_smooth(method = "lm")

# Interpret regression output in light of the plot.

# z.nox:z.rm -8.33.  The slope between z.nox and medv changes
# by -8.33 with each additional unit of z.rm.  Or the same point
# can be stated reciprocally:  The slope between z.rm and medv changes
# by -8.33 with each additional unit of z.nox.

# z.nox -4.87.  Among those districts with average rooms (z.rm = 0),
# an increase of 1 unit in z.nox is associated with a change of -4.87 
# in medv.

# z.rm 7.47.  Among those districts with average nox (z.nox = 0),
# an increase of 1 unit in z.rm is associated with a change of 7.47 
# in medv.

