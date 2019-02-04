### Statistics and Predictive Analytics

# Tutorial topic:  Exploring NHST

# Load packages
library(dplyr)
library(ggplot2)

## Exploring NHST 

## P-values: example

# You flip a coin 100 times and get 35 heads.  Is this a fair coin?
# H_0: fair coin.
# H_a: unfair coin
# Would 65% tails be a  surprising event under H_0?
# Let's investigate.

# 1.  Create a null distribution for the fair coin, H_0.  The null
# distribution will be defined by  Binomial(100, .5).

null_dist <- rbinom(n = 10000, size = 100, prob= .5)

head(null_dist)

plot(density(null_dist))
abline(v = 35, col = 2)
abline(v = quantile(null_dist, probs = c(.025, .975)), 
       lty = 2)

# 2. Estimate a probability value for 35% heads. 


# Compare to binom.test
?binom.test
binom.test(35, 100, p = .5, alternative = "less")

