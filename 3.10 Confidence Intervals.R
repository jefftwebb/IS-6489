### Statistics and Predictive Analytics

# Tutorial topic:  Confidence intervals

# Load packages
library(dplyr)
library(ggplot2)

# SEM = s/sqrt(n)

# 1.  Make up data
set.seed(123)
n <- 100
x1 <- rnorm(n, mean = 5, sd = 5)
x2 <- rnorm(n, mean = 5, sd = 1)

head(x1)
head(x2)

# 2. Calculate CIs

(SEM1 <- sd(x1)/sqrt(n))
(SEM2 <- sd(x2)/sqrt(n))

# Lower 95% CI: mean(x) - 1.96 x SEM

mean(x1) - 1.96*SEM1
mean(x2) - 1.96*SEM2


# Upper 95% CI: mean(x) - 1.96 x SEM

mean(x1) + 1.96*SEM1
mean(x2) + 1.96*SEM2

# Note: if we had a sampling distribution we would simply use sd to estimate SE
# and not divide by sqrt(n)

# Experiment
n = 1000
df <- data.frame(lower = rep(0, n), upper = rep(0, n))

for(i in 1:1000){
  x <- rnorm(n, mean = 5, sd = 5)
  SEM <- sd(x)/sqrt(n)
  df$lower[i] <- mean(x) - 1.96*SEM
  df$upper[i] <- mean(x) + 1.96*SEM
}

head(df)

df$contains <- ifelse(df$lower < 5 & df$upper > 5, T, F)

head(df$contains)

sum(df$contains)
