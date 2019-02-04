# Practice script, class 3
# Sampling, simulation, inference
# IS 6489

library(ggplot2); library(dplyr)


###################################################################

## PRACTICE: SEM and Confidence intervals in R.  
# Generate two random samples of n = 100 from N(0, 4) and N(0, 1).
# (This will be standard deviation of 2 and 1, respectively.)

# Functions for taking random samples

# Bernoulli
rbinom(n = 10, size = 1, prob = .5 )

# Binomial
rbinom(n = 10, size = 10, prob = .5 )

hist(rbinom(1000, 10, .5))

# Uniform
runif(n = 10, min = 5, max = 10)

# Guassian or normal
rnorm(n = 10, mean = 0, sd = 0)

set.seed(123)

# SEM = s/sqrt(n)
# Note: if we had a sampling distribution we would simply use s to estimate SE
# and not divide by sqrt(n)

# Lower 95% CI: mean(x) - 1.96 x SEM


# Upper 95% CI: mean(x) - 1.96 x SEM


###################################################################

## PRACTICE:  coding the bootstrap in R

# Bootstrap $SEM$ and CIs for daily bike ridership in the bike data,
# compare to analytical computation.

d <- read.csv("day.csv")
glimpse(d)

# Analytical calculation of SEM: s/sqrt(n)


# Analytical calculation of 95% CI:

# Lower 95% CI: mean(x) - 1.96 x SEM

# Upper 95% CI: mean(x) - 1.96 x SEM

# Now use bootstrap:
# 1. initialize vector, boot_est, to store bootstrap distribution of means
# 2. create a loop (1000 iterations) inside of which you sample
# the data and calculate and store the mean for that sample.
# Key point: use set.seed() before (not inside) your bootstrap loop.
# It is possible to vectorize bootstrapping but loops are easier conceptually.

# Difference between sampling with and without replacement
sample(LETTERS[1:10], replace = F)
sample(LETTERS[1:10], replace = T)

set.seed(123)
# find the boostrap estimate of the SEM


# find the bootstrap estimate of the 95% CI (two methods):
# parametric bootstrap

# percentile method

# Bootstrap $SE$ and CIs for a correlation coefficient temperature and ridership
# in the bike data. R code:  cor(x, y). 

# compare to cor.test()

cor.test(d$temp, d$cnt)


###################################################################

## DEMONSTRATION:  Exploring NHST 

## P-values: example

# You flip a coin 100 times and get 35 heads.  Is this a fair coin?
# H_0: fair coin.
# H_a: unfair coin
# Would 65% tails be a  surprising event under H_0?
# Let's investigate.

# 1.  Create a null distribution for the fair coin, H_0.  The null
# distribution will be defined by  Binomial(100, .5).

null_dist <- rbinom(n = 10000, 100, prob= .5)

head(null_dist)

plot(density(null_dist))
abline(v = 35, col = 2)
abline(v = quantile(null_dist, probs = c(.025, .975)), 
       lty = 2)

# 2. Estimate a probability value for 35% heads. 


# Compare to binom.test
?binom.test
binom.test(35, 100, p = .5, alternative = "less")

###################################################################

## PRACTICE:  Explore the t-test

# Define two samples  of size 10, x1 and x2, from different distributions: 
# N(1,1) and N(1.25,1). Start with n = 10. 

set.seed(123)
x1 <- rnorm(10, 1, 1)
x2 <- rnorm(10, 1.25, 1)

# Use a t-test to compare them. Does the test identify them as significantly
# different? In R code: t.test()

t.test(x1, x2)


# Set n = 1000.  Does the t-test pick up the difference?  Why?

set.seed(123)
x1 <- rnorm(1000, 1, 1)
x2 <- rnorm(1000, 1.25, 1)

t.test(x1, x2)