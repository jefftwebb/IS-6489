### Statistics and Predictive Analytics

# Tutorial topic:  Standard error of the mean

# Load packages
library(dplyr)
library(ggplot2)

# SEM = s/sqrt(n)

# Note: if we had a sampling distribution we would simply use the standard 
# deviation of the sampling distribution to estimate SEM
# and not divide by sqrt(n)

# 1. Make up some data
set.seed(123)
n <- 1000
x1 <- rnorm(n, mean = 5, sd = 5)
x2 <- rnorm(n, mean = 5, sd = 1)

# 2. Calculate the SEM using the formula

sd(x1)/sqrt(n) # .16
sd(x2)/sqrt(n) # .03

# 3. Creat a sampling distribution and estimate SEM.

samp_dist <- replicate(1000, mean(rnorm(n, mean = 5, sd = 5)))

samp_dist %>%
  head

samp_dist %>%
  sd
 

