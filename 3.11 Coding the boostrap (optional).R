### Statistics and Predictive Analytics

# Tutorial topic:  Coding the boostrap

# Load packages
library(dplyr)
library(ggplot2)



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


