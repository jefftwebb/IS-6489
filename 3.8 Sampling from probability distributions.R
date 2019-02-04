### Statistics and Predictive Analytics

# Tutorial topic:  Sampling from probability distributions

# Load packages
library(dplyr)
library(ggplot2)


# Functions for taking random samples
set.seed(123)


# Bernoulli
rbinom(n = 10, size = 1, prob = .5)

# Binomial

rbinom(n = 10, size = 10, prob = .5))


hist(rbinom(1000, 10, .5))

# Uniform
runif(n = 10, min = 5, max = 10)

hist(runif(n = 10000, min = 5, max = 10))


# Guassian or normal
hist(rnorm(n = 10, mean = 0, sd = 1))
hist(rnorm(n = 100, mean = 0, sd = 1))
hist(rnorm(n = 1000, mean = 0, sd = 1))
hist(rnorm(n = 10000, mean = 0, sd = 1))

mean(rnorm(n = 10, mean = 0, sd = 1))
mean(rnorm(n = 10000, mean = 0, sd = 1))


